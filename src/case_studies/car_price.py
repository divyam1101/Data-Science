"""Car price drivers for a new market entrant (Geely Auto / US car dataset).

Business question: which car attributes drive price, and how well can price be predicted?

Changes from the 2018 R version:

* **No leakage.** The R script built its final model (``model_30``) on the *full* dataset
  (``data = cars_assignment``) and then "validated" it on a test set it had already seen. Here the
  test set is held out from every step, feature selection included.
* **Automatic, cross-validated feature selection.** Thirty hand-edited ``lm`` calls, each dropping
  one variable by p-value or VIF, are replaced by LassoCV inside a pipeline. The selected features
  are then refitted with OLS (statsmodels) for interpretable coefficients and VIFs.
* **Log-price target.** Prices are right-skewed, so a log model has better-behaved residuals and
  its coefficients read as percentage effects.
* **Honest error estimates.** With only 205 cars a single 70/30 split is noisy, so models are
  compared with repeated 5-fold cross-validation, and the test set is scored once at the end.
* A **gradient-boosting** model is compared against the linear models.

Usage::

    python -m case_studies.car_price --data data/car_price/CarPrice_Assignment.csv --out outputs/car_price
"""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd
import statsmodels.api as sm
from sklearn.compose import ColumnTransformer, make_column_selector
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.linear_model import LassoCV, RidgeCV
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import RepeatedKFold, cross_validate, train_test_split
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from statsmodels.stats.outliers_influence import variance_inflation_factor

from ._common import RANDOM_STATE, ensure_dir, save_figure, save_json

import matplotlib.pyplot as plt  # noqa: E402

BRAND_FIXES = {
    "maxda": "mazda", "porcshce": "porsche", "toyouta": "toyota",
    "vokswagen": "volkswagen", "vw": "volkswagen", "alfa-romero": "alfa-romeo",
}
CATEGORICAL = ["brand", "fueltype", "aspiration", "doornumber", "carbody", "drivewheel",
               "enginelocation", "enginetype", "cylindernumber", "fuelsystem"]


def load(path: str | Path) -> pd.DataFrame:
    df = pd.read_csv(path)
    df["brand"] = df["CarName"].str.split().str[0].str.lower().replace(BRAND_FIXES)
    df = df.drop(columns=["car_ID", "CarName"])
    for col in CATEGORICAL:
        df[col] = df[col].astype("category")
    return df


def preprocessor() -> ColumnTransformer:
    return ColumnTransformer([
        ("num", StandardScaler(), make_column_selector(dtype_exclude="category")),
        ("cat", OneHotEncoder(handle_unknown="infrequent_if_exist", min_frequency=3, sparse_output=False,
                              drop="if_binary"), make_column_selector(dtype_include="category")),
    ], verbose_feature_names_out=False)


def models() -> dict:
    return {
        "Ridge": make_pipeline(preprocessor(), RidgeCV(alphas=np.logspace(-3, 3, 25))),
        "Lasso": make_pipeline(preprocessor(), LassoCV(alphas=50, cv=5, max_iter=50_000, random_state=RANDOM_STATE)),
        "Gradient boosting": make_pipeline(preprocessor(), GradientBoostingRegressor(
            n_estimators=400, learning_rate=0.05, max_depth=3, subsample=0.8, random_state=RANDOM_STATE)),
    }


def ols_on_selected(X_train: pd.DataFrame, y_train: pd.Series, lasso_pipeline, max_vif: float = 10.0):
    """Refit OLS on the Lasso-selected features, then drop features one at a time while VIF > max_vif."""
    pre = lasso_pipeline[0]
    coefs = lasso_pipeline[-1].coef_
    names = pre.get_feature_names_out()
    design = pd.DataFrame(pre.transform(X_train), columns=names, index=X_train.index)
    selected = [n for n, c in zip(names, coefs) if abs(c) > 1e-8]
    design = design[selected]
    while design.shape[1] > 1:
        vifs = pd.Series([variance_inflation_factor(design.values, i) for i in range(design.shape[1])],
                         index=design.columns)
        if vifs.max() <= max_vif:
            break
        design = design.drop(columns=vifs.idxmax())
    vifs = pd.Series([variance_inflation_factor(design.values, i) for i in range(design.shape[1])],
                     index=design.columns) if design.shape[1] > 1 else pd.Series(1.0, index=design.columns)
    ols = sm.OLS(y_train, sm.add_constant(design)).fit()
    return ols, vifs


def run(data: str | Path, out_dir: str | Path) -> dict:
    out = ensure_dir(out_dir)
    df = load(data)
    y = np.log(df.pop("price"))
    X_train, X_test, y_train, y_test = train_test_split(df, y, test_size=0.2, random_state=RANDOM_STATE)

    cv = RepeatedKFold(n_splits=5, n_repeats=5, random_state=RANDOM_STATE)
    results = {"n_train": len(X_train), "n_test": len(X_test), "models": {}}
    fitted = {}
    for name, model in models().items():
        scores = cross_validate(model, X_train, y_train, cv=cv,
                                scoring=("r2", "neg_root_mean_squared_error"))
        model.fit(X_train, y_train)
        fitted[name] = model
        pred_price = np.exp(model.predict(X_test))
        true_price = np.exp(y_test)
        results["models"][name] = {
            "cv_r2_mean": float(scores["test_r2"].mean()), "cv_r2_std": float(scores["test_r2"].std()),
            "cv_rmse_log_mean": float(-scores["test_neg_root_mean_squared_error"].mean()),
            "test_r2_log": float(r2_score(y_test, model.predict(X_test))),
            "test_rmse_usd": float(np.sqrt(mean_squared_error(true_price, pred_price))),
            "test_mae_usd": float(mean_absolute_error(true_price, pred_price)),
        }
        r = results["models"][name]
        print(f"{name:18s} CV R² {r['cv_r2_mean']:.3f} ± {r['cv_r2_std']:.3f}   test R² {r['test_r2_log']:.3f}   "
              f"test MAE ${r['test_mae_usd']:,.0f}")

    ols, vifs = ols_on_selected(X_train, y_train, fitted["Lasso"])
    (out / "ols_summary.txt").write_text(str(ols.summary()))
    drivers = pd.DataFrame({
        "coef_log": ols.params.drop("const"),
        "pct_effect_per_unit": (np.exp(ols.params.drop("const")) - 1) * 100,
        "p_value": ols.pvalues.drop("const"),
        "vif": vifs,
    }).sort_values("p_value")
    drivers.to_csv(out / "price_drivers.csv")
    results["ols_r2_train"] = float(ols.rsquared)
    results["ols_features"] = list(drivers.index)

    best = max(results["models"], key=lambda k: results["models"][k]["cv_r2_mean"])
    results["best_model_by_cv"] = best
    fig, ax = plt.subplots(figsize=(5.5, 5))
    ax.scatter(np.exp(y_test), np.exp(fitted[best].predict(X_test)), alpha=0.7)
    lims = [np.exp(y).min(), np.exp(y).max()]
    ax.plot(lims, lims, "k--", lw=1)
    ax.set(xlabel="Actual price ($)", ylabel="Predicted price ($)", title=f"{best}: test set")
    save_figure(fig, out / "predicted_vs_actual.png")
    save_json(results, out / "metrics.json")
    return results


def main(argv=None) -> None:
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--data", default="data/car_price/CarPrice_Assignment.csv")
    p.add_argument("--out", default="outputs/car_price")
    a = p.parse_args(argv)
    run(a.data, a.out)


if __name__ == "__main__":
    main()
