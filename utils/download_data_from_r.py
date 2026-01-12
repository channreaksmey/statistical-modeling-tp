import r2py.robjects as ro
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr
from rpy2.robjects.conversion import localconverter

import pandas as pd

def download_data_from_r(package_name: str, dataset_name: str) -> pd.DataFrame:
    """
    Downloads a dataset from an R package and converts it to a pandas DataFrame.

    Parameters:
    package_name (str): The name of the R package containing the dataset.
    dataset_name (str): The name of the dataset to download.

    Returns:
    pd.DataFrame: The dataset as a pandas DataFrame.
    """
    # Import the R package
    r_package = importr(package_name)

    # Load the dataset from the R package
    ro.r(f"data('{dataset_name}', package='{package_name}')")

    # Get the dataset as an R object
    r_dataset = ro.r[dataset_name]

    # Convert the R object to a pandas DataFrame
    with localconverter(ro.default_converter + pandas2ri.converter):
        pandas_df = ro.conversion.rpy2py(r_dataset)

    return pandas_df