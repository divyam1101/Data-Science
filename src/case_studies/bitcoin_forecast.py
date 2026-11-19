"""Bitcoin price forecasting: does ARIMA beat a naive forecast?

Changes from the 2018 R version:

* **A baseline.** The R script reported ARIMA's MAPE on a single 3-4 point hold-out with nothing
  to compare it to. Financial prices are close to a random walk, so the bar to beat is the
  **naive forecast** (tomorrow = today). A model is only useful if it beats that.
* **Walk-forward (rolling-origin) evaluation.** Each step refits on all data up to time *t* and
  forecasts *t+1*, over many steps. That mimics real use and gives a far more reliable error
  estimate than one tiny split.
* **Log prices.** Volatility scales with price level; modelling log prices stabilises variance.
* **Order selection by AIC** over a small (p, d=1, q) grid at each step (statsmodels), the Python
  equivalent of ``auto.arima``.

Usage::

    python -m case_studies.bitcoin_forecast --data data/bitcoin/bitcoin_price.csv --out outputs/bitcoin
"""

from __future__ import annotations

import argparse
import itertools
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
from statsmodels.tsa.arima.model import ARIMA

from ._common import ensure_dir, save_figure, save_json

import matplotlib.pyplot as plt  # noqa: E402


def load(path: str | Path) -> pd.Series:
    df = pd.read_csv(path)
    price_col = next(c for c in df.columns if c.strip().lower() in ("price", "close", "close price"))
    prices = pd.to_numeric(df[price_col].astype(str).str.replace(",", ""), errors="coerce")
    date_col = next((c for c in df.columns if c.strip().lower() in ("date", "timestamp")), None)
    if date_col:
        prices.index = pd.to_datetime(df[date_col], errors="coerce")
        prices = prices[prices.index.notna()].sort_index()
    return prices.dropna().rename("price")


def best_arima(log_prices: np.ndarray, max_p: int = 2, max_q: int = 2):
    best, best_aic = None, np.inf
    for p, q in itertools.product(range(max_p + 1), range(max_q + 1)):
        try:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                fit = ARIMA(log_prices, order=(p, 1, q), trend="t" if (p, q) == (0, 0) else "n").fit()
        except Exception:  # noqa: BLE001 - some orders fail to converge on short series
            continue
        if fit.aic < best_aic:
            best, best_aic = fit, fit.aic
    return best


def walk_forward(prices: pd.Series, n_test: int, min_train: int = 20) -> pd.DataFrame:
    log_p = np.log(prices.to_numpy())
    start = max(min_train, len(log_p) - n_test)
    rows = []
    for t in range(start, len(log_p)):
        history = log_p[:t]
        arima = best_arima(history)
        arima_fc = float(np.exp(arima.forecast(1)[0])) if arima is not None else np.nan
        drift = (history[-1] - history[0]) / (len(history) - 1)
        rows.append({
            "t": prices.index[t], "actual": float(np.exp(log_p[t])),
            "naive": float(np.exp(history[-1])),
            "drift": float(np.exp(history[-1] + drift)),
            "arima": arima_fc,
            "arima_order": str(arima.model.order) if arima is not None else None,
        })
    return pd.DataFrame(rows).set_index("t")


def scores(forecasts: pd.DataFrame) -> dict:
    out = {}
    for model in ("naive", "drift", "arima"):
        err = forecasts[model] - forecasts["actual"]
        out[model] = {
            "mape_pct": float(100 * np.mean(np.abs(err) / forecasts["actual"])),
            "rmse": float(np.sqrt(np.mean(err ** 2))),
        }
    out["arima_vs_naive_mape_ratio"] = out["arima"]["mape_pct"] / out["naive"]["mape_pct"]
    return out


def run(data: str | Path, out_dir: str | Path, n_test: int | None = None) -> dict:
    out = ensure_dir(out_dir)
    prices = load(data)
    if n_test is None:
        n_test = max(3, int(round(len(prices) * 0.2)))
    fc = walk_forward(prices, n_test)
    fc.to_csv(out / "walk_forward_forecasts.csv")
    result = {"n_observations": int(len(prices)), "n_forecasts": int(len(fc)), "scores": scores(fc)}

    fig, ax = plt.subplots(figsize=(10, 4))
    ax.plot(range(len(prices)), prices.values, color="black", lw=1.5, label="actual")
    idx = range(len(prices) - len(fc), len(prices))
    ax.plot(idx, fc["arima"].values, "o-", label="ARIMA (1-step)")
    ax.plot(idx, fc["naive"].values, "s--", label="naive (1-step)")
    ax.set(title="Walk-forward one-step-ahead forecasts", xlabel="observation", ylabel="price")
    ax.legend()
    save_figure(fig, out / "forecasts.png")
    save_json(result, out / "metrics.json")
    s = result["scores"]
    print(f"MAPE: naive {s['naive']['mape_pct']:.2f}%, drift {s['drift']['mape_pct']:.2f}%, "
          f"ARIMA {s['arima']['mape_pct']:.2f}% (ratio vs naive {s['arima_vs_naive_mape_ratio']:.2f})")
    return result


def main(argv=None) -> None:
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--data", default="data/bitcoin/bitcoin_price.csv")
    p.add_argument("--out", default="outputs/bitcoin")
    p.add_argument("--n-test", type=int, default=None)
    a = p.parse_args(argv)
    run(a.data, a.out, a.n_test)


if __name__ == "__main__":
    main()
