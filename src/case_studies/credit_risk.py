"""Credit risk: which loan applicants are likely to default? (Lending Club loans, 2007-2011)

Changes from the 2018 R exploratory analysis (16 ggplot charts):

* **Only completed loans.** Loans still marked "Current" have no outcome yet and are excluded
  from default rates.
* **Only information available at application time.** Fields recorded after the loan was issued
  (payments, recoveries, last payment date, ...) are excluded. They describe the outcome rather
  than predict it.
* **Quantified drivers.** Each candidate driver gets a default rate per segment with a 95% Wilson
  confidence interval, and is ranked by **Information Value** (weight of evidence), the standard
  credit-scoring screen, instead of being judged by eye from bar charts.
* **A simple scorecard model** (logistic regression) with an **out-of-time test** (train on loans
  issued before 2011, test on 2011 loans), which is how a lender would actually deploy it.

Usage::

    python -m case_studies.credit_risk --data data/credit_risk/loan.csv --out outputs/credit_risk
"""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer, make_column_selector
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import average_precision_score, roc_auc_score
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler

from ._common import ensure_dir, save_figure, save_json

import matplotlib.pyplot as plt  # noqa: E402

NUMERIC = ["loan_amnt", "int_rate", "installment", "annual_inc", "dti", "revol_util",
           "inq_last_6mths", "delinq_2yrs", "open_acc", "total_acc", "pub_rec_bankruptcies",
           "emp_length_years", "term_months"]
CATEGORICAL = ["grade", "home_ownership", "verification_status", "purpose"]


def load(path: str | Path) -> pd.DataFrame:
    df = pd.read_csv(path, low_memory=False)
    df = df[df["loan_status"].isin(["Fully Paid", "Charged Off"])].copy()
    df["default"] = (df["loan_status"] == "Charged Off").astype(int)
    df["term_months"] = pd.to_numeric(df["term"].astype(str).str.extract(r"(\d+)")[0], errors="coerce")
    for col in ["int_rate", "revol_util"]:
        df[col] = pd.to_numeric(df[col].astype(str).str.rstrip("%"), errors="coerce")
    emp = df["emp_length"].astype(str).str.replace("< 1", "0", regex=False).str.extract(r"(\d+)")[0]
    df["emp_length_years"] = pd.to_numeric(emp, errors="coerce")
    df["issue_date"] = pd.to_datetime(df["issue_d"], format="%b-%y", errors="coerce")
    df["issue_year"] = df["issue_date"].dt.year
    keep = NUMERIC + CATEGORICAL + ["default", "issue_date", "issue_year", "addr_state"]
    return df[[c for c in keep if c in df.columns]]


def wilson_interval(successes, n, z: float = 1.96):
    successes, n = np.asarray(successes, float), np.asarray(n, float)
    p = successes / n
    denom = 1 + z ** 2 / n
    centre = (p + z ** 2 / (2 * n)) / denom
    half = z * np.sqrt(p * (1 - p) / n + z ** 2 / (4 * n ** 2)) / denom
    return centre - half, centre + half


def segment_rates(df: pd.DataFrame, column: str, bins: int = 5) -> pd.DataFrame:
    series = df[column]
    if pd.api.types.is_numeric_dtype(series) and series.nunique() > 12:
        series = pd.qcut(series, bins, duplicates="drop")
    table = df.groupby(series, observed=True)["default"].agg(loans="size", defaults="sum")
    table["default_rate"] = table["defaults"] / table["loans"]
    table["ci_low"], table["ci_high"] = wilson_interval(table["defaults"], table["loans"])
    return table


def information_value(df: pd.DataFrame, column: str, bins: int = 10) -> float:
    series = df[column]
    if pd.api.types.is_numeric_dtype(series) and series.nunique() > 12:
        series = pd.qcut(series, bins, duplicates="drop")
    series = series.astype("object").where(series.notna(), "missing")
    counts = pd.crosstab(series, df["default"])
    goods = (counts.get(0, 0) + 0.5) / (counts.get(0, 0).sum() + 0.5)
    bads = (counts.get(1, 0) + 0.5) / (counts.get(1, 0).sum() + 0.5)
    woe = np.log(goods / bads)
    return float(((goods - bads) * woe).sum())


def scorecard(df: pd.DataFrame, test_year: int) -> dict:
    train, test = df[df["issue_year"] < test_year], df[df["issue_year"] == test_year]
    if train["default"].nunique() < 2 or test["default"].nunique() < 2:
        return {"skipped": "not enough loans on both sides of the out-of-time split"}
    features = [c for c in NUMERIC + CATEGORICAL if c in df.columns]
    pre = ColumnTransformer([
        ("num", make_pipeline(SimpleImputer(strategy="median"), StandardScaler()), make_column_selector(dtype_include="number")),
        ("cat", OneHotEncoder(handle_unknown="ignore", min_frequency=20), make_column_selector(dtype_exclude="number")),
    ])
    model = make_pipeline(pre, LogisticRegression(C=0.1, max_iter=5000, class_weight="balanced"))
    model.fit(train[features], train["default"])
    proba = model.predict_proba(test[features])[:, 1]
    return {
        "train_years": f"< {test_year}", "test_year": test_year,
        "n_train": int(len(train)), "n_test": int(len(test)),
        "test_default_rate": float(test["default"].mean()),
        "test_roc_auc": float(roc_auc_score(test["default"], proba)),
        "test_average_precision": float(average_precision_score(test["default"], proba)),
        "gini": float(2 * roc_auc_score(test["default"], proba) - 1),
    }


def run(data: str | Path, out_dir: str | Path) -> dict:
    out = ensure_dir(out_dir)
    df = load(data)
    candidates = [c for c in NUMERIC + CATEGORICAL if c in df.columns]
    iv = pd.Series({c: information_value(df, c) for c in candidates}, name="information_value").sort_values(ascending=False)
    iv.to_csv(out / "information_value.csv")

    seg_dir = ensure_dir(out / "segments")
    for col in candidates:
        segment_rates(df, col).to_csv(seg_dir / f"{col}.csv")

    fig, axes = plt.subplots(2, 2, figsize=(12, 8))
    for ax, col in zip(axes.flat, ["grade", "term_months", "int_rate", "purpose"]):
        if col not in df.columns:
            ax.set_visible(False)
            continue
        t = segment_rates(df, col)
        labels = [str(i) for i in t.index]
        ax.bar(labels, t["default_rate"] * 100,
               yerr=[(t["default_rate"] - t["ci_low"]) * 100, (t["ci_high"] - t["default_rate"]) * 100], capsize=3)
        ax.set(title=f"Default rate by {col}", ylabel="% charged off")
        ax.tick_params(axis="x", rotation=45 if col == "purpose" else 0, labelsize=8)
    save_figure(fig, out / "default_rates.png")

    fig, ax = plt.subplots(figsize=(7, 5))
    iv.sort_values().plot.barh(ax=ax)
    for x, label in [(0.02, "weak"), (0.1, "medium"), (0.3, "strong")]:
        ax.axvline(x, ls="--", lw=0.8, color="grey")
        ax.text(x, -0.9, label, fontsize=8, ha="center")
    ax.set(xlabel="Information value", title="Predictive strength of each applicant attribute")
    save_figure(fig, out / "information_value.png")

    test_year = int(df["issue_year"].max())
    results = {
        "n_completed_loans": int(len(df)),
        "overall_default_rate": float(df["default"].mean()),
        "information_value": iv.round(4).to_dict(),
        "scorecard_out_of_time": scorecard(df, test_year),
    }
    save_json(results, out / "metrics.json")
    print(f"{len(df):,} completed loans, default rate {df['default'].mean():.1%}")
    print("Top drivers by information value:", iv.head(5).round(3).to_dict())
    print("Out-of-time scorecard:", results["scorecard_out_of_time"])
    return results


def main(argv=None) -> None:
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--data", default="data/credit_risk/loan.csv")
    p.add_argument("--out", default="outputs/credit_risk")
    a = p.parse_args(argv)
    run(a.data, a.out)


if __name__ == "__main__":
    main()
