"""Handwritten digit recognition (MNIST sample) with support vector machines.

Changes from the 2018 R version:

* **Feature scaling.** The R script trained an RBF-kernel SVM on raw 0-255 pixel values
  (``scale = FALSE``). That makes the kernel width depend on pixel brightness, which is why its
  grid search needed tiny sigma values. Pixels are now scaled to [0, 1].
* **A single, stratified train/test split** with hyperparameters chosen by cross-validation on
  the training set only. The test set is used once, at the end.
* **More than accuracy.** The report adds macro-F1, a per-class report and a confusion matrix.
* **PCA + RBF-SVM** is compared with a linear SVM. Projecting 784 pixels onto about 50 principal
  components keeps most of the variance and speeds up the kernel SVM a lot.

Usage::

    python -m case_studies.digit_recognition --data data/digits/MyData.csv --out outputs/digit_recognition
"""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt  # noqa: E402  (backend set in _common)
import pandas as pd
from sklearn.decomposition import PCA
from sklearn.metrics import ConfusionMatrixDisplay, accuracy_score, classification_report, f1_score
from sklearn.model_selection import GridSearchCV, StratifiedKFold, train_test_split
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import FunctionTransformer
from sklearn.svm import SVC, LinearSVC

from ._common import RANDOM_STATE, ensure_dir, save_figure, save_json, scale_pixels


def load_digits_csv(path: str | Path) -> tuple[pd.DataFrame, pd.Series]:
    """Read the MNIST-format CSV used by the course (label column ``X5``, 784 pixel columns)."""
    df = pd.read_csv(path)
    df = df.drop(columns=[c for c in df.columns if c.startswith("Unnamed") or c == ""], errors="ignore")
    label_col = "X5" if "X5" in df.columns else df.columns[0]
    y = df.pop(label_col).astype(int)
    if df.shape[1] != 784:
        raise ValueError(f"expected 784 pixel columns, found {df.shape[1]}")
    return df.astype("float32"), y


def build_models(fast: bool = False) -> dict[str, GridSearchCV]:
    cv = StratifiedKFold(n_splits=3 if fast else 5, shuffle=True, random_state=RANDOM_STATE)
    scaler = FunctionTransformer(scale_pixels)
    linear = GridSearchCV(
        make_pipeline(scaler, LinearSVC(dual="auto", max_iter=5000, random_state=RANDOM_STATE)),
        {"linearsvc__C": [0.01, 0.1] if fast else [0.001, 0.01, 0.1, 1.0]},
        cv=cv, scoring="accuracy", n_jobs=-1,
    )
    rbf = GridSearchCV(
        make_pipeline(scaler, PCA(n_components=50, random_state=RANDOM_STATE), SVC(kernel="rbf")),
        {"svc__C": [5.0] if fast else [1.0, 5.0, 10.0], "svc__gamma": ["scale"] if fast else ["scale", 0.01, 0.05]},
        cv=cv, scoring="accuracy", n_jobs=-1,
    )
    return {"Linear SVM": linear, "PCA(50) + RBF SVM": rbf}


def run(data: str | Path, out_dir: str | Path, fast: bool = False) -> dict:
    out = ensure_dir(out_dir)
    X, y = load_digits_csv(data)
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, stratify=y, random_state=RANDOM_STATE)

    results = {"n_train": len(X_train), "n_test": len(X_test), "models": {}}
    best_name, best_model, best_acc = None, None, -1.0
    for name, search in build_models(fast).items():
        search.fit(X_train, y_train)
        pred = search.predict(X_test)
        acc = accuracy_score(y_test, pred)
        results["models"][name] = {
            "best_params": search.best_params_,
            "cv_accuracy": float(search.best_score_),
            "test_accuracy": float(acc),
            "test_macro_f1": float(f1_score(y_test, pred, average="macro")),
        }
        print(f"{name:20s} CV acc {search.best_score_:.4f}  test acc {acc:.4f}  params {search.best_params_}")
        if acc > best_acc:
            best_name, best_model, best_acc = name, search, acc

    pred = best_model.predict(X_test)
    results["best_model"] = best_name
    (out / "classification_report.txt").write_text(classification_report(y_test, pred, digits=4))
    fig, ax = plt.subplots(figsize=(7, 6))
    ConfusionMatrixDisplay.from_predictions(y_test, pred, ax=ax, colorbar=False)
    ax.set_title(f"{best_name}: test confusion matrix (accuracy {best_acc:.3f})")
    save_figure(fig, out / "confusion_matrix.png")
    save_json(results, out / "metrics.json")
    return results


def main(argv=None) -> None:
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--data", default="data/digits/MyData.csv")
    p.add_argument("--out", default="outputs/digit_recognition")
    p.add_argument("--fast", action="store_true", help="smaller grid, for quick checks")
    a = p.parse_args(argv)
    run(a.data, a.out, a.fast)


if __name__ == "__main__":
    main()
