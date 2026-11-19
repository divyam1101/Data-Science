"""Synthetic datasets with the same schema as the real course datasets.

They let every case study run end to end in CI without redistributing third-party data.
"""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

REPO = Path(__file__).resolve().parents[1]


@pytest.fixture
def rng():
    return np.random.default_rng(0)


@pytest.fixture
def car_csv(tmp_path, rng):
    n = 150
    brands = rng.choice(["toyota", "toyouta", "bmw", "vw", "volkswagen", "maxda", "mazda", "porsche", "porcshce"], n)
    df = pd.DataFrame({
        "car_ID": range(1, n + 1),
        "symboling": rng.integers(-2, 4, n),
        "CarName": [f"{b} model{i % 7}" for i, b in enumerate(brands)],
        "fueltype": rng.choice(["gas", "diesel"], n, p=[0.9, 0.1]),
        "aspiration": rng.choice(["std", "turbo"], n, p=[0.8, 0.2]),
        "doornumber": rng.choice(["two", "four"], n),
        "carbody": rng.choice(["sedan", "hatchback", "wagon", "convertible"], n),
        "drivewheel": rng.choice(["fwd", "rwd", "4wd"], n),
        "enginelocation": rng.choice(["front", "rear"], n, p=[0.97, 0.03]),
        "wheelbase": rng.normal(98, 6, n), "carlength": rng.normal(174, 12, n),
        "carwidth": rng.normal(66, 2, n), "carheight": rng.normal(54, 2.4, n),
        "curbweight": rng.normal(2550, 520, n),
        "enginetype": rng.choice(["ohc", "dohc", "ohcv", "rotor"], n, p=[0.7, 0.15, 0.1, 0.05]),
        "cylindernumber": rng.choice(["four", "six", "five"], n, p=[0.75, 0.15, 0.1]),
        "enginesize": rng.normal(127, 40, n).clip(60),
        "fuelsystem": rng.choice(["mpfi", "2bbl", "idi"], n),
        "boreratio": rng.normal(3.3, 0.27, n), "stroke": rng.normal(3.25, 0.3, n),
        "compressionratio": rng.normal(10, 3, n), "horsepower": rng.normal(104, 39, n).clip(48),
        "peakrpm": rng.normal(5125, 476, n), "citympg": rng.normal(25, 6.5, n), "highwaympg": rng.normal(30, 7, n),
    })
    premium = df["CarName"].str.startswith(("bmw", "porsche", "porcshce")).astype(float)
    df["price"] = np.exp(7.5 + 0.012 * df["enginesize"] + 0.6 * premium + rng.normal(0, 0.12, n)).round(0)
    path = tmp_path / "CarPrice_Assignment.csv"
    df.to_csv(path, index=False)
    return path


@pytest.fixture
def hr_dir(tmp_path, rng):
    n = 400
    ids = np.arange(1, n + 1)
    years = rng.integers(0, 20, n)
    overtime = rng.normal(0, 1, n)
    logit = -2.2 + 1.2 * overtime - 0.08 * years
    attr = rng.random(n) < 1 / (1 + np.exp(-logit))
    general = pd.DataFrame({
        "Age": rng.integers(18, 60, n), "Attrition": np.where(attr, "Yes", "No"),
        "BusinessTravel": rng.choice(["Travel_Rarely", "Travel_Frequently", "Non-Travel"], n),
        "Department": rng.choice(["Sales", "Research & Development", "Human Resources"], n),
        "DistanceFromHome": rng.integers(1, 30, n), "Education": rng.integers(1, 6, n),
        "EducationField": rng.choice(["Life Sciences", "Medical", "Marketing"], n),
        "EmployeeCount": 1, "EmployeeID": ids, "Gender": rng.choice(["Male", "Female"], n),
        "JobLevel": rng.integers(1, 6, n), "JobRole": rng.choice(["Sales Executive", "Research Scientist", "Manager"], n),
        "MaritalStatus": rng.choice(["Single", "Married", "Divorced"], n),
        "MonthlyIncome": rng.integers(10_000, 200_000, n),
        "NumCompaniesWorked": np.where(rng.random(n) < 0.02, np.nan, rng.integers(0, 10, n)),
        "Over18": "Y", "PercentSalaryHike": rng.integers(11, 26, n), "StandardHours": 8,
        "StockOptionLevel": rng.integers(0, 4, n), "TotalWorkingYears": years + rng.integers(0, 10, n),
        "TrainingTimesLastYear": rng.integers(0, 7, n), "YearsAtCompany": years,
        "YearsSinceLastPromotion": rng.integers(0, 8, n), "YearsWithCurrManager": rng.integers(0, 10, n),
    })
    general.to_csv(tmp_path / "general_data.csv", index=False)
    pd.DataFrame({"EmployeeID": ids, "EnvironmentSatisfaction": rng.integers(1, 5, n).astype(float),
                  "JobSatisfaction": rng.integers(1, 5, n), "WorkLifeBalance": rng.integers(1, 5, n)}
                 ).to_csv(tmp_path / "employee_survey_data.csv", index=False)
    pd.DataFrame({"EmployeeID": ids, "JobInvolvement": rng.integers(1, 5, n), "PerformanceRating": rng.integers(3, 5, n)}
                 ).to_csv(tmp_path / "manager_survey_data.csv", index=False)

    days = pd.bdate_range("2015-01-01", periods=15)
    t_in, t_out = {"": ids}, {"": ids}
    for k, day in enumerate(days):
        start = day + pd.Timedelta(hours=9) + pd.to_timedelta(rng.integers(0, 60, n), unit="min")
        end = start + pd.to_timedelta(8 + overtime + rng.normal(0, 0.3, n), unit="h")
        absent = rng.random(n) < 0.05
        if k == 0:  # company holiday: nobody badges in
            absent[:] = True
        t_in[str(day.date())] = np.where(absent, None, start.astype(str))
        t_out[str(day.date())] = np.where(absent, None, end.astype(str))
    pd.DataFrame(t_in).to_csv(tmp_path / "in_time.csv", index=False)
    pd.DataFrame(t_out).to_csv(tmp_path / "out_time.csv", index=False)
    return tmp_path


@pytest.fixture
def loan_csv(tmp_path, rng):
    n = 3000
    grade = rng.choice(list("ABCDEFG"), n, p=[0.25, 0.3, 0.2, 0.13, 0.07, 0.03, 0.02])
    rate = {g: 6 + 3 * i for i, g in enumerate("ABCDEFG")}
    int_rate = np.array([rate[g] for g in grade]) + rng.normal(0, 1, n)
    term = rng.choice([36, 60], n, p=[0.75, 0.25])
    p_default = 1 / (1 + np.exp(-(-3.2 + 0.15 * (int_rate - 6) + 0.5 * (term == 60))))
    status = np.where(rng.random(n) < p_default, "Charged Off", "Fully Paid")
    status[rng.random(n) < 0.03] = "Current"
    df = pd.DataFrame({
        "loan_amnt": rng.integers(1000, 35000, n), "term": [f" {t} months" for t in term],
        "int_rate": [f"{r:.2f}%" for r in int_rate], "installment": rng.normal(320, 200, n).clip(20),
        "grade": grade, "emp_length": rng.choice(["< 1 year", "1 year", "5 years", "10+ years", "n/a"], n),
        "home_ownership": rng.choice(["RENT", "MORTGAGE", "OWN"], n),
        "annual_inc": rng.lognormal(11, 0.5, n), "verification_status": rng.choice(["Verified", "Not Verified"], n),
        "issue_d": rng.choice(["Jan-09", "Jun-10", "Dec-10", "Mar-11", "Dec-11"], n),
        "loan_status": status, "purpose": rng.choice(["debt_consolidation", "credit_card", "small_business", "car"], n),
        "addr_state": rng.choice(["CA", "NY", "TX"], n), "dti": rng.uniform(0, 30, n),
        "revol_util": [f"{v:.1f}%" for v in rng.uniform(0, 100, n)], "inq_last_6mths": rng.integers(0, 6, n),
        "delinq_2yrs": rng.integers(0, 3, n), "open_acc": rng.integers(2, 30, n), "total_acc": rng.integers(5, 60, n),
        "pub_rec_bankruptcies": rng.choice([0, 1, np.nan], n, p=[0.94, 0.04, 0.02]),
        "total_pymnt": rng.uniform(0, 40000, n),  # post-origination field: must be ignored
    })
    path = tmp_path / "loan.csv"
    df.to_csv(path, index=False)
    return path


@pytest.fixture
def investment_dir(tmp_path, rng):
    n_companies = 300
    countries = rng.choice(["USA", "GBR", "IND", "CHN", "CAN", "DEU"], n_companies, p=[0.4, 0.12, 0.12, 0.2, 0.08, 0.08])
    categories = rng.choice(["Analytics|Big Data", "Biotechnology", "Enterprise 2.0", "Games|Mobile", "Finance"], n_companies)
    companies = pd.DataFrame({
        "permalink": [f"/Organization/Co-{i}" for i in range(n_companies)],
        "name": [f"Company {i}" for i in range(n_companies)],
        "homepage_url": "", "category_list": categories, "status": "operating",
        "country_code": countries, "state_code": "", "region": "", "city": "", "founded_at": "",
    })
    companies.to_csv(tmp_path / "companies.txt", sep="\t", index=False)
    n_rounds = 1500
    kind = rng.choice(["venture", "seed", "angel", "private_equity", "debt_financing"], n_rounds, p=[0.5, 0.2, 0.1, 0.1, 0.1])
    scale = {"venture": 1e7, "seed": 5e5, "angel": 8e5, "private_equity": 6e7, "debt_financing": 3e6}
    amount = np.array([rng.lognormal(np.log(scale[k]), 0.6) for k in kind])
    amount[rng.random(n_rounds) < 0.1] = np.nan
    pd.DataFrame({
        "company_permalink": [f"/ORGANIZATION/CO-{i}" for i in rng.integers(0, n_companies, n_rounds)],
        "funding_round_permalink": [f"/funding-round/{i}" for i in range(n_rounds)],
        "funding_round_type": kind, "funding_round_code": "", "funded_at": "01-01-2014",
        "raised_amount_usd": amount,
    }).to_csv(tmp_path / "rounds2.csv", index=False)
    mapping = pd.DataFrame({
        "category_list": ["A0lytics", "Biotechnology", "Enterprise 2.0", "Games", "Fi0nce", ""],
        "Automotive & Sports": [0, 0, 0, 0, 0, 0], "Blanks": [0, 0, 0, 0, 0, 1],
        "Cleantech / Semiconductors": [0, 0, 0, 0, 0, 0], "Entertainment": [0, 0, 0, 1, 0, 0],
        "Health": [0, 1, 0, 0, 0, 0], "Manufacturing": [0, 0, 0, 0, 0, 0],
        "News, Search and Messaging": [0, 0, 0, 0, 0, 0], "Others": [0, 0, 1, 0, 0, 0],
        "Social, Finance, Analytics, Advertising": [1, 0, 0, 0, 1, 0],
    })
    mapping.to_csv(tmp_path / "mapping.csv", index=False)
    return tmp_path


@pytest.fixture
def uber_csv(tmp_path, rng):
    n = 1200
    day = rng.integers(11, 16, n)
    hour = rng.integers(0, 24, n)
    minute = rng.integers(0, 60, n)
    point = rng.choice(["City", "Airport"], n)
    morning_city = (point == "City") & (hour >= 4) & (hour < 10)
    evening_airport = (point == "Airport") & (hour >= 17) & (hour < 22)
    status = np.where(morning_city & (rng.random(n) < 0.5), "Cancelled",
                      np.where(evening_airport & (rng.random(n) < 0.7), "No Cars Available",
                               np.where(rng.random(n) < 0.1, "Cancelled", "Trip Completed")))
    request = [f"{d}/7/2016 {h}:{m:02d}" if i % 2 else f"{d}-07-2016 {h:02d}:{m:02d}:00"
               for i, (d, h, m) in enumerate(zip(day, hour, minute))]
    drop = [r if s == "Trip Completed" else None for r, s in zip(request, status)]
    pd.DataFrame({"Request id": range(1, n + 1), "Pickup point": point,
                  "Driver id": np.where(status == "No Cars Available", np.nan, rng.integers(1, 300, n)),
                  "Status": status, "Request timestamp": request, "Drop timestamp": drop}
                 ).to_csv(tmp_path / "Uber Request Data.csv", index=False)
    return tmp_path / "Uber Request Data.csv"


@pytest.fixture
def bitcoin_csv(tmp_path, rng):
    n = 40
    prices = 8000 * np.exp(np.cumsum(rng.normal(0.002, 0.03, n)))
    dates = pd.date_range("2018-01-01", periods=n, freq="D")
    df = pd.DataFrame({"Date": dates.strftime("%Y-%m-%d")[::-1], "Price": [f"{p:,.2f}" for p in prices[::-1]]})
    path = tmp_path / "bitcoin_price.csv"
    df.to_csv(path, index=False)
    return path


@pytest.fixture
def digits_sample(tmp_path):
    source = REPO / "data" / "digits" / "MyData.csv"
    df = pd.read_csv(source, nrows=600)
    path = tmp_path / "digits.csv"
    df.to_csv(path, index=False)
    return path
