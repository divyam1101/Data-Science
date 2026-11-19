"""Investment strategy for an asset-management firm ("Spark Funds").

Constraints: invest USD 5-15 million per round, only in English-speaking countries. Questions:
which funding type, which countries, which sectors?

Changes from the 2018 R version:

* **No mean imputation of missing amounts.** The R script filled missing ``raised_amount_usd``
  values with the overall mean before averaging by funding type, pulling every average towards
  the same number. Missing amounts are now excluded.
* **Median as well as mean.** Round sizes are heavily skewed (a few mega-rounds), so the typical
  round size per funding type is judged by the median, with the mean reported alongside.
* **Explicit English-speaking country list**, instead of judging by eye.
* **pandas instead of SQL strings** (``sqldf``), so each step is a short, testable function.
* The **"0" -> "na" fix** in the sector mapping file ("A0lytics" -> "Analytics") keeps the
  legitimate "Enterprise 2.0".

Usage::

    python -m case_studies.investment --data-dir data/investment --out outputs/investment
"""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

from ._common import ensure_dir, save_figure, save_json

import matplotlib.pyplot as plt  # noqa: E402

FUNDING_TYPES = ["venture", "seed", "angel", "private_equity"]
RANGE_USD = (5_000_000, 15_000_000)
# Countries where English is an official language (ISO 3166-1 alpha-3), as relevant to this dataset.
ENGLISH_SPEAKING = {
    "USA", "GBR", "IND", "CAN", "AUS", "IRL", "NZL", "SGP", "ZAF", "NGA", "KEN", "PHL", "PAK",
    "GHA", "UGA", "MLT", "JAM", "BHS", "BRB", "BMU", "CYM", "HKG", "BWA", "ZWE", "RWA", "TZA",
}


def _read_text_table(path: Path, sep: str) -> pd.DataFrame:
    for encoding in ("utf-8", "latin-1"):
        try:
            return pd.read_csv(path, sep=sep, encoding=encoding, low_memory=False)
        except UnicodeDecodeError:
            continue
    raise UnicodeDecodeError("utf-8/latin-1", b"", 0, 1, f"could not decode {path}")


def load(data_dir: str | Path) -> tuple[pd.DataFrame, pd.DataFrame]:
    d = Path(data_dir)
    companies = _read_text_table(d / "companies.txt", sep="\t")
    rounds = _read_text_table(d / "rounds2.csv", sep=",")
    mapping = _read_text_table(d / "mapping.csv", sep=",")

    companies["permalink"] = companies["permalink"].str.strip().str.lower()
    rounds["company_permalink"] = rounds["company_permalink"].str.strip().str.lower()
    master = rounds.merge(companies, how="left", left_on="company_permalink", right_on="permalink")
    master["primary_sector"] = master["category_list"].str.split("|").str[0].str.strip().str.lower()
    return master, sector_lookup(mapping)


def sector_lookup(mapping: pd.DataFrame) -> pd.DataFrame:
    """Wide 0/1 mapping file -> long (category -> main_sector) table."""
    long = mapping.melt(id_vars="category_list", var_name="main_sector", value_name="flag")
    long = long[(long["flag"] == 1) & (long["main_sector"] != "Blanks")].drop(columns="flag")
    fixed = long["category_list"].str.replace("0", "na", regex=False).str.replace("2.na", "2.0", regex=False)
    long["category_list"] = fixed.str.strip().str.lower()
    return long.drop_duplicates("category_list")


def funding_type_summary(master: pd.DataFrame) -> pd.DataFrame:
    sub = master[master["funding_round_type"].isin(FUNDING_TYPES)].dropna(subset=["raised_amount_usd"])
    summary = sub.groupby("funding_round_type")["raised_amount_usd"].agg(["count", "median", "mean"])
    lo, hi = RANGE_USD
    summary["median_in_range"] = summary["median"].between(lo, hi)
    return summary.sort_values("median", ascending=False)


def choose_funding_type(summary: pd.DataFrame) -> str:
    in_range = summary[summary["median_in_range"]]
    if in_range.empty:
        target = sum(RANGE_USD) / 2
        return (summary["median"] - target).abs().idxmin()
    return in_range["count"].idxmax()


def top_countries(master: pd.DataFrame, funding_type: str, n: int = 9) -> pd.DataFrame:
    sub = master[(master["funding_round_type"] == funding_type) & master["country_code"].notna()]
    top = sub.groupby("country_code")["raised_amount_usd"].sum().nlargest(n).to_frame("total_usd")
    top["english_speaking"] = top.index.isin(ENGLISH_SPEAKING)
    return top


def sector_analysis(master: pd.DataFrame, sectors: pd.DataFrame, funding_type: str, countries: list[str]) -> dict:
    lo, hi = RANGE_USD
    data = master.merge(sectors, how="inner", left_on="primary_sector", right_on="category_list")
    data = data[(data["funding_round_type"] == funding_type)
                & data["raised_amount_usd"].between(lo, hi)
                & data["country_code"].isin(countries)]
    out = {}
    for country in countries:
        c = data[data["country_code"] == country]
        by_sector = c.groupby("main_sector")["raised_amount_usd"].agg(investments="count", total_usd="sum")
        by_sector = by_sector.sort_values("investments", ascending=False)
        top_companies = {}
        for sector in by_sector.index[:2]:
            per_company = c[c["main_sector"] == sector].groupby("name")["raised_amount_usd"].sum()
            top_companies[sector] = per_company.idxmax() if len(per_company) else None
        out[country] = {"by_sector": by_sector, "top_company_in_top_sectors": top_companies,
                        "total_investments": int(by_sector["investments"].sum()),
                        "total_usd": float(by_sector["total_usd"].sum())}
    return out


def run(data_dir: str | Path, out_dir: str | Path) -> dict:
    out = ensure_dir(out_dir)
    master, sectors = load(data_dir)

    summary = funding_type_summary(master)
    summary.to_csv(out / "funding_types.csv")
    funding_type = choose_funding_type(summary)

    countries = top_countries(master, funding_type)
    countries.to_csv(out / "top9_countries.csv")
    top3 = list(countries[countries["english_speaking"]].index[:3])

    sector_results = sector_analysis(master, sectors, funding_type, top3)
    for country, res in sector_results.items():
        res["by_sector"].to_csv(out / f"sectors_{country}.csv")

    fig, axes = plt.subplots(1, 3, figsize=(15, 4.5))
    (summary["median"] / 1e6).plot.bar(ax=axes[0], rot=0)
    axes[0].axhspan(RANGE_USD[0] / 1e6, RANGE_USD[1] / 1e6, color="green", alpha=0.15, label="target range")
    axes[0].set(title="Median round size by funding type", ylabel="USD million")
    axes[0].legend()
    (countries["total_usd"] / 1e9).plot.bar(ax=axes[1], rot=0,
                                            color=["tab:blue" if e else "tab:grey" for e in countries["english_speaking"]])
    axes[1].set(title=f"Top 9 countries, total {funding_type} funding", ylabel="USD billion")
    counts = pd.DataFrame({c: r["by_sector"]["investments"] for c, r in sector_results.items()}).fillna(0)
    counts.plot.bar(ax=axes[2], rot=60)
    axes[2].set(title="Number of investments by sector (top 3 English-speaking countries)", ylabel="investments")
    axes[2].tick_params(axis="x", labelsize=7)
    save_figure(fig, out / "investment_overview.png")

    results = {
        "chosen_funding_type": funding_type,
        "funding_type_medians_usd": summary["median"].round(0).to_dict(),
        "top9_countries": list(countries.index),
        "top3_english_speaking": top3,
        "sectors": {c: {"total_investments": r["total_investments"], "total_usd": r["total_usd"],
                        "top_sectors": list(r["by_sector"].index[:3]),
                        "top_company_in_top_sectors": r["top_company_in_top_sectors"]}
                    for c, r in sector_results.items()},
    }
    save_json(results, out / "summary.json")
    print(f"Funding type: {funding_type}; countries: {top3}")
    for c, r in results["sectors"].items():
        print(f"  {c}: top sectors {r['top_sectors']}")
    return results


def main(argv=None) -> None:
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--data-dir", default="data/investment")
    p.add_argument("--out", default="outputs/investment")
    a = p.parse_args(argv)
    run(a.data_dir, a.out)


if __name__ == "__main__":
    main()
