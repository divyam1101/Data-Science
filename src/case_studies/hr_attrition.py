"""Employee attrition: who is likely to leave, and why?

The company loses about 15% of employees a year. Five files are combined (general data, employee
and manager surveys, and daily badge-in/badge-out times) to predict attrition and explain its drivers.

Changes from the 2018 R version:

* **No leakage.** The R script removed outliers, dropped rows with missing values and standardised
  all variables *before* the train/test split. Here imputation and scaling sit inside a pipeline that
  is fitted on training data only, and no rows are thrown away.
* **Threshold chosen without the test set.** The R script tuned its probability cutoff on the test
  set, and its ``perform_fn`` had a bug: it always used the fixed 0.40 predictions, so the cutoff
  sweep was flat. The cutoff is now chosen with Youden's J on *out-of-fold* training predictions.
* **Threshold-free metrics.** ROC-AUC, average precision (more informative when only ~16% are
  positive) and the Brier score. The KS statistic is computed on probabilities, not on 0/1 labels.
* **Regularised logistic regression** with cross-validated strength, plus a gradient-boosting
  comparison, replaces 18 rounds of manual stepwise/VIF elimination.
* **New feature:** days of leave taken, from the badge data, alongside average hours worked.

Usage::

    python -m case_studies.hr_attrition --data-dir data/hr_attrition --out outputs/hr_attrition
"""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt  # noqa: E402
import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer, make_column_selector
from sklearn.ensemble import HistGradientBoostingClassifier
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegressionCV
from sklearn.metrics import (
    RocCurveDisplay,
    average_precision_score,
    brier_score_loss,
    confusion_matrix,
    roc_auc_score,
    roc_curve,
)
from sklearn.model_selection import StratifiedKFold, cross_val_predict, train_test_split
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler

from ._common import RANDOM_STATE, ensure_dir, save_figure, save_json

CONSTANT_COLUMNS = ["EmployeeCount", "StandardHours", "Over18"]


def badge_features(in_time: pd.DataFrame, out_time: pd.DataFrame) -> pd.DataFrame:
    """Average hours per working day and number of leave days per employee.

    Days where *nobody* badged in are company holidays and are ignored.
    """
    in_time = in_time.set_index(in_time.columns[0])
    out_time = out_time.set_index(out_time.columns[0])
    t_in = in_time.apply(pd.to_datetime, errors="coerce")
    t_out = out_time.apply(pd.to_datetime, errors="coerce")
    working_days = t_in.columns[t_in.notna().any(axis=0)]
    hours = (t_out[working_days] - t_in[working_days]).apply(lambda c: c.dt.total_seconds() / 3600)
    feats = pd.DataFrame({
        "avg_hours_worked": hours.mean(axis=1),
        "leave_days": t_in[working_days].isna().sum(axis=1),
    })
    feats.index.name = "EmployeeID"
    return feats.reset_index()


def load(data_dir: str | Path) -> pd.DataFrame:
    d = Path(data_dir)
    general = pd.read_csv(d / "general_data.csv")
    employee = pd.read_csv(d / "employee_survey_data.csv")
    manager = pd.read_csv(d / "manager_survey_data.csv")
    badges = badge_features(pd.read_csv(d / "in_time.csv"), pd.read_csv(d / "out_time.csv"))
    df = (general.merge(employee, on="EmployeeID", how="left", validate="one_to_one")
          .merge(manager, on="EmployeeID", how="left", validate="one_to_one")
          .merge(badges, on="EmployeeID", how="left", validate="one_to_one"))
    df = df.drop(columns=[c for c in CONSTANT_COLUMNS if c in df.columns])
    for col in df.select_dtypes(include=["object", "string"]).columns:
        df[col] = df[col].astype("category")
    return df


def preprocessor() -> ColumnTransformer:
    numeric = make_pipeline(SimpleImputer(strategy="median"), StandardScaler())
    categorical = make_pipeline(SimpleImputer(strategy="most_frequent"),
                                OneHotEncoder(handle_unknown="ignore", drop="if_binary", sparse_output=False))
    return ColumnTransformer([
        ("num", numeric, make_column_selector(dtype_exclude="category")),
        ("cat", categorical, make_column_selector(dtype_include="category")),
    ], verbose_feature_names_out=False)


def models() -> dict:
    cv = StratifiedKFold(5, shuffle=True, random_state=RANDOM_STATE)
    return {
        "Logistic regression": make_pipeline(preprocessor(), LogisticRegressionCV(
            Cs=np.logspace(-3, 2, 12), cv=cv, scoring="roc_auc", max_iter=5000)),
        "Gradient boosting": make_pipeline(preprocessor(), HistGradientBoostingClassifier(
            learning_rate=0.05, max_iter=300, max_leaf_nodes=15, l2_regularization=1.0,
            random_state=RANDOM_STATE)),
    }


def youden_threshold(y_true, proba) -> float:
    fpr, tpr, thresholds = roc_curve(y_true, proba)
    return float(thresholds[np.argmax(tpr - fpr)])


def ks_statistic(y_true, proba) -> float:
    fpr, tpr, _ = roc_curve(y_true, proba)
    return float(np.max(tpr - fpr))


def gain_table(y_true, proba, groups: int = 10) -> pd.DataFrame:
    df = pd.DataFrame({"y": np.asarray(y_true), "p": np.asarray(proba)})
    df["decile"] = pd.qcut(df["p"].rank(method="first", ascending=False), groups, labels=range(1, groups + 1))
    table = df.groupby("decile", observed=True).agg(n=("y", "size"), leavers=("y", "sum"))
    table["cum_gain_pct"] = 100 * table["leavers"].cumsum() / table["leavers"].sum()
    table["cum_lift"] = table["cum_gain_pct"] / (100 * table["n"].cumsum() / table["n"].sum())
    return table


def run(data_dir: str | Path, out_dir: str | Path) -> dict:
    out = ensure_dir(out_dir)
    df = load(data_dir)
    y = (df.pop("Attrition").astype(str) == "Yes").astype(int)
    X = df.drop(columns=["EmployeeID"])
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, stratify=y, random_state=RANDOM_STATE)

    results = {"attrition_rate": float(y.mean()), "n_train": len(X_train), "n_test": len(X_test), "models": {}}
    fitted = {}
    oof_cv = StratifiedKFold(5, shuffle=True, random_state=RANDOM_STATE + 1)
    for name, model in models().items():
        oof = cross_val_predict(model, X_train, y_train, cv=oof_cv, method="predict_proba")[:, 1]
        threshold = youden_threshold(y_train, oof)
        model.fit(X_train, y_train)
        fitted[name] = model
        proba = model.predict_proba(X_test)[:, 1]
        tn, fp, fn, tp = confusion_matrix(y_test, (proba >= threshold).astype(int)).ravel()
        results["models"][name] = {
            "threshold_from_oof": threshold,
            "test_roc_auc": float(roc_auc_score(y_test, proba)),
            "test_average_precision": float(average_precision_score(y_test, proba)),
            "test_brier": float(brier_score_loss(y_test, proba)),
            "test_ks": ks_statistic(y_test, proba),
            "test_sensitivity": float(tp / (tp + fn)),
            "test_specificity": float(tn / (tn + fp)),
            "test_accuracy": float((tp + tn) / (tp + tn + fp + fn)),
        }
        r = results["models"][name]
        print(f"{name:20s} AUC {r['test_roc_auc']:.3f}  AP {r['test_average_precision']:.3f}  "
              f"sens {r['test_sensitivity']:.3f}  spec {r['test_specificity']:.3f}  (cutoff {threshold:.3f})")

    best = max(results["models"], key=lambda k: results["models"][k]["test_roc_auc"])
    results["best_model"] = best
    gain_table(y_test, fitted[best].predict_proba(X_test)[:, 1]).to_csv(out / "gain_lift_table.csv")

    # interpretable drivers: odds ratios per 1 SD (numeric) or vs reference level (categorical)
    logit = fitted["Logistic regression"]
    names = logit[0].get_feature_names_out()
    odds = pd.Series(np.exp(logit[-1].coef_[0]), index=names, name="odds_ratio").sort_values()
    odds.to_csv(out / "odds_ratios.csv")
    results["top_risk_factors"] = odds.tail(5).round(2).to_dict()
    results["top_protective_factors"] = odds.head(5).round(2).to_dict()

    fig, ax = plt.subplots(figsize=(6, 5))
    for name, model in fitted.items():
        RocCurveDisplay.from_estimator(model, X_test, y_test, name=name, ax=ax)
    ax.plot([0, 1], [0, 1], "k--", lw=1)
    ax.set_title("Attrition model ROC curves (test set)")
    save_figure(fig, out / "roc_curves.png")

    fig, ax = plt.subplots(figsize=(7, 6))
    top = pd.concat([odds.head(8), odds.tail(8)])
    ax.barh(top.index, np.log(top.values), color=["tab:green"] * 8 + ["tab:red"] * 8)
    ax.axvline(0, color="k", lw=1)
    ax.set(xlabel="log odds ratio (per SD / vs reference)", title="Strongest attrition drivers (logistic regression)")
    save_figure(fig, out / "drivers.png")

    save_json(results, out / "metrics.json")
    return results


def main(argv=None) -> None:
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--data-dir", default="data/hr_attrition")
    p.add_argument("--out", default="outputs/hr_attrition")
    a = p.parse_args(argv)
    run(a.data_dir, a.out)


if __name__ == "__main__":
    main()
