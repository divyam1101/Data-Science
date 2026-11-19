import json

import numpy as np
import pandas as pd
import pytest

from case_studies import (bitcoin_forecast, car_price, credit_risk, digit_recognition, hr_attrition, investment,
                          uber_supply_demand)


# ------------------------------------------------------------------ car price

def test_car_brand_typos_are_fixed(car_csv):
    df = car_price.load(car_csv)
    assert set(df["brand"].cat.categories) <= {"toyota", "bmw", "volkswagen", "mazda", "porsche"}


def test_car_price_end_to_end(car_csv, tmp_path):
    res = car_price.run(car_csv, tmp_path / "out")
    assert res["models"]["Lasso"]["cv_r2_mean"] > 0.5  # synthetic data has strong signal
    assert (tmp_path / "out" / "price_drivers.csv").exists()
    assert "enginesize" in res["ols_features"]


# ------------------------------------------------------------------ HR attrition

def test_badge_features_ignore_company_holidays(hr_dir):
    feats = hr_attrition.badge_features(pd.read_csv(hr_dir / "in_time.csv"), pd.read_csv(hr_dir / "out_time.csv"))
    assert feats["leave_days"].max() < 14, "the all-NaN holiday column must not count as leave"
    assert feats["avg_hours_worked"].notna().all()
    assert feats["avg_hours_worked"].between(3, 13).all()  # synthetic days are 8h +/- a few hours


def test_youden_threshold_and_ks():
    y = np.array([0, 0, 0, 1, 1, 1])
    p = np.array([0.1, 0.2, 0.3, 0.7, 0.8, 0.9])
    assert 0.3 < hr_attrition.youden_threshold(y, p) <= 0.7
    assert hr_attrition.ks_statistic(y, p) == pytest.approx(1.0)


def test_hr_attrition_end_to_end(hr_dir, tmp_path):
    res = hr_attrition.run(hr_dir, tmp_path / "out")
    assert res["models"]["Logistic regression"]["test_roc_auc"] > 0.7
    gains = pd.read_csv(tmp_path / "out" / "gain_lift_table.csv")
    assert gains["cum_gain_pct"].iloc[-1] == pytest.approx(100)
    assert "avg_hours_worked" in pd.read_csv(tmp_path / "out" / "odds_ratios.csv").iloc[:, 0].tolist()


# ------------------------------------------------------------------ credit risk

def test_wilson_interval_contains_rate():
    lo, hi = credit_risk.wilson_interval([10], [100])
    assert lo[0] < 0.10 < hi[0]


def test_credit_risk_excludes_current_loans_and_leaky_fields(loan_csv):
    df = credit_risk.load(loan_csv)
    assert "total_pymnt" not in df.columns
    assert df["default"].isin([0, 1]).all()
    raw = pd.read_csv(loan_csv)
    assert len(df) == (raw["loan_status"] != "Current").sum()


def test_credit_risk_end_to_end(loan_csv, tmp_path):
    res = credit_risk.run(loan_csv, tmp_path / "out")
    iv = res["information_value"]
    assert max(iv, key=iv.get) in {"int_rate", "grade"}  # the planted signal
    assert res["scorecard_out_of_time"]["test_roc_auc"] > 0.6


# ------------------------------------------------------------------ investment

def test_sector_mapping_fixes_zero_typo_but_keeps_enterprise_2_0(investment_dir):
    lookup = investment.sector_lookup(pd.read_csv(investment_dir / "mapping.csv"))
    cats = set(lookup["category_list"])
    assert {"analytics", "finance", "enterprise 2.0"} <= cats
    assert "Blanks" not in set(lookup["main_sector"])


def test_investment_end_to_end(investment_dir, tmp_path):
    res = investment.run(investment_dir, tmp_path / "out")
    assert res["chosen_funding_type"] == "venture"
    assert "CHN" not in res["top3_english_speaking"]
    assert res["top3_english_speaking"][0] == "USA"


# ------------------------------------------------------------------ uber

def test_mixed_timestamp_formats_parse_day_first():
    parsed = uber_supply_demand.parse_timestamps(pd.Series(["11/7/2016 11:51", "13-07-2016 08:33:16"]))
    assert parsed.dt.month.tolist() == [7, 7]
    assert parsed.dt.day.tolist() == [11, 13]


def test_time_slots_cover_every_hour():
    assert uber_supply_demand.time_slot(4) == "Early morning (04-09)"
    assert uber_supply_demand.time_slot(23) == "Night (22-23)"
    assert {uber_supply_demand.time_slot(h) for h in range(24)} == set(uber_supply_demand.SLOT_ORDER)


def test_uber_end_to_end_finds_planted_gaps(uber_csv, tmp_path):
    res = uber_supply_demand.run(uber_csv, tmp_path / "out")
    worst = {(s["pickup_point"], s["slot"]): s["main_failure"] for s in res["worst_segments"]}
    assert ("Airport", "Evening (17-21)") in worst
    assert worst[("Airport", "Evening (17-21)")] == "No Cars Available"


# ------------------------------------------------------------------ bitcoin

def test_bitcoin_loader_sorts_by_date_and_strips_commas(bitcoin_csv):
    prices = bitcoin_forecast.load(bitcoin_csv)
    assert prices.index.is_monotonic_increasing
    assert prices.iloc[0] < 20_000


def test_bitcoin_walk_forward(bitcoin_csv, tmp_path):
    res = bitcoin_forecast.run(bitcoin_csv, tmp_path / "out", n_test=5)
    assert res["n_forecasts"] == 5
    assert set(res["scores"]) >= {"naive", "drift", "arima"}
    assert json.loads((tmp_path / "out" / "metrics.json").read_text())["n_observations"] == 40


# ------------------------------------------------------------------ digits (real sample data)

def test_digit_recognition_on_real_sample(digits_sample, tmp_path):
    res = digit_recognition.run(digits_sample, tmp_path / "out", fast=True)
    assert res["models"]["PCA(50) + RBF SVM"]["test_accuracy"] > 0.8
    assert (tmp_path / "out" / "confusion_matrix.png").exists()
