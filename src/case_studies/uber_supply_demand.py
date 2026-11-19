"""Uber supply-demand gap between the city and the airport (July 2016 request log).

Changes from the 2018 R version:

* **Robust timestamp parsing.** The raw file mixes ``11/7/2016 11:51`` and
  ``13-07-2016 08:33:16``. Both are parsed day-first in one vectorised call instead of a
  row-by-row regex function.
* **Numeric hours.** The R script compared hour *strings* with numbers (``"04" <= 10``) to build
  its time slots, which only worked by accident of zero-padding.
* **Finer, documented time slots.** The R version put everything from 2 pm to midnight, and from
  midnight to 10 am, into the same buckets. Five slots now separate the early-morning airport rush
  from the late-evening one.
* **The gap is quantified.** Gap = requests minus completed trips, plus the share of requests
  unfulfilled, by slot and pickup point, with the main failure mode (cancellation or no car) named.

Usage::

    python -m case_studies.uber_supply_demand --data "data/uber/Uber Request Data.csv" --out outputs/uber
"""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

from ._common import ensure_dir, save_figure, save_json

import matplotlib.pyplot as plt  # noqa: E402

SLOTS = [(0, 4, "Late night (00-03)"), (4, 10, "Early morning (04-09)"), (10, 17, "Daytime (10-16)"),
         (17, 22, "Evening (17-21)"), (22, 24, "Night (22-23)")]
SLOT_ORDER = [s[2] for s in SLOTS]


def parse_timestamps(series: pd.Series) -> pd.Series:
    normalised = series.astype("string").str.strip().str.replace("/", "-", regex=False)
    return pd.to_datetime(normalised, format="mixed", dayfirst=True, errors="coerce")


def time_slot(hour: int) -> str:
    for start, end, label in SLOTS:
        if start <= hour < end:
            return label
    raise ValueError(f"invalid hour {hour}")


def load(path: str | Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    df.columns = [c.strip().lower().replace(" ", "_") for c in df.columns]
    df["request_time"] = parse_timestamps(df["request_timestamp"])
    df["drop_time"] = parse_timestamps(df["drop_timestamp"])
    if df["request_time"].isna().any():
        raise ValueError(f"{df['request_time'].isna().sum()} request timestamps could not be parsed")
    df["hour"] = df["request_time"].dt.hour
    df["weekday"] = df["request_time"].dt.day_name()
    df["slot"] = pd.Categorical(df["hour"].map(time_slot), categories=SLOT_ORDER, ordered=True)
    df["fulfilled"] = df["status"] == "Trip Completed"
    return df


def gap_table(df: pd.DataFrame) -> pd.DataFrame:
    g = df.groupby(["pickup_point", "slot"], observed=True)
    table = g.agg(requests=("request_id", "size"), completed=("fulfilled", "sum"))
    status = df.pivot_table(index=["pickup_point", "slot"], columns="status", values="request_id",
                            aggfunc="size", fill_value=0, observed=True)
    table = table.join(status)
    table["gap"] = table["requests"] - table["completed"]
    table["gap_pct"] = 100 * table["gap"] / table["requests"]
    failure_cols = [c for c in ("Cancelled", "No Cars Available") if c in table.columns]
    table["main_failure"] = table[failure_cols].idxmax(axis=1) if failure_cols else None
    return table.sort_values("gap", ascending=False)


def run(data: str | Path, out_dir: str | Path) -> dict:
    out = ensure_dir(out_dir)
    df = load(data)
    table = gap_table(df)
    table.to_csv(out / "supply_demand_gap.csv")

    fig, axes = plt.subplots(1, 2, figsize=(14, 4.5), sharey=True)
    for ax, (point, sub) in zip(axes, df.groupby("pickup_point")):
        counts = sub.pivot_table(index="hour", columns="status", values="request_id", aggfunc="size", fill_value=0)
        counts.reindex(range(24), fill_value=0).plot.bar(stacked=True, ax=ax, width=0.85, rot=0)
        ax.set(title=f"Requests from {point} by hour", xlabel="hour of request", ylabel="requests")
    save_figure(fig, out / "requests_by_hour.png")

    pct = df.pivot_table(index="pickup_point", columns="hour", values="fulfilled", aggfunc="mean")
    fig, ax = plt.subplots(figsize=(14, 2.8))
    im = ax.imshow(100 * (1 - pct.values), aspect="auto", cmap="Reds", vmin=0, vmax=100)
    ax.set(yticks=range(len(pct.index)), yticklabels=pct.index, xticks=range(pct.shape[1]), xticklabels=pct.columns,
           xlabel="hour of request", title="% of requests not fulfilled")
    fig.colorbar(im, ax=ax)
    save_figure(fig, out / "gap_heatmap.png")

    worst = table.head(2).reset_index()
    results = {
        "n_requests": int(len(df)),
        "date_range": [str(df["request_time"].min().date()), str(df["request_time"].max().date())],
        "overall_fulfilment_rate": float(df["fulfilled"].mean()),
        "worst_segments": [
            {"pickup_point": r.pickup_point, "slot": str(r.slot), "requests": int(r.requests),
             "unfulfilled_pct": round(float(r.gap_pct), 1), "main_failure": r.main_failure}
            for r in worst.itertuples()
        ],
    }
    save_json(results, out / "summary.json")
    for seg in results["worst_segments"]:
        print(f"{seg['pickup_point']:8s} {seg['slot']:22s} {seg['unfulfilled_pct']:5.1f}% unfulfilled, "
              f"mostly '{seg['main_failure']}'")
    return results


def main(argv=None) -> None:
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--data", default="data/uber/Uber Request Data.csv")
    p.add_argument("--out", default="outputs/uber")
    a = p.parse_args(argv)
    run(a.data, a.out)


if __name__ == "__main__":
    main()
