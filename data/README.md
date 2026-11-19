# Data

Only the MNIST sample for digit recognition (`digits/MyData.csv`, 9,000 images) is stored in this repository. The other datasets are public course datasets owned by third parties. Download them and place them as shown below:

```
data/
├── digits/MyData.csv                      # included
├── car_price/CarPrice_Assignment.csv      # "Car Price Prediction" (Geely Auto) dataset, 205 cars
├── hr_attrition/                          # "HR Analytics Case Study" dataset, 4,410 employees
│   ├── general_data.csv
│   ├── employee_survey_data.csv
│   ├── manager_survey_data.csv
│   ├── in_time.csv
│   └── out_time.csv
├── credit_risk/loan.csv                   # Lending Club loans issued 2007-2011 (39,717 loans)
├── investment/                            # Crunchbase funding-rounds extract used in the Spark Funds case study
│   ├── companies.txt                      # tab-separated
│   ├── rounds2.csv
│   └── mapping.csv
├── uber/Uber Request Data.csv             # Uber request log, 11-15 July 2016 (6,745 requests)
└── bitcoin/bitcoin_price.csv              # any daily price history with Date and Price (or Close) columns
```

All of these are available on Kaggle under the names above. Every script also accepts `--data` / `--data-dir`, so the files can live anywhere.

The test suite does not need any of these files: `tests/conftest.py` generates synthetic data with the same columns, so every case study runs end to end in CI.
