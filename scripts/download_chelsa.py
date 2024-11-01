import argparse
import os
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor
import multiprocessing

def download_file(year, month, variable):
    # Define the URL pattern for the files
    url_pattern = "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/{variable}/CHELSA_{variable}_{month}_{year}_V.2.1.tif"
    
    # Create the destination directory where the file will be saved
    destination_dir = "chelsea_download"
    
    # Create the destination directory if it doesn't exist
    if not os.path.exists(destination_dir):
        os.makedirs(destination_dir)
    
    if variable == "rsds":
        url_pattern = "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/{variable}/CHELSA_{variable}_{year}_{month}_V.2.1.tif"

    if variable == "pet":
        url_pattern = "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/{variable}/CHELSA_{variable}_penman_{month}_{year}_V.2.1.tif"

    # Create the URL using the provided year, month, and variable name
    url = url_pattern.format(year=str(year), month=str(month), variable=str(variable))
    
    # Define the destination file path
    file_path = os.path.join(destination_dir, f"{variable}_{month}_{year}.tif")
    
    # Use wget to download the file from the URL to the destination file path
    os.system(f"wget {url} -O {file_path}")
    print(f"File downloaded to: {file_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Download files based on years, months, and variables")
    parser.add_argument("--years", nargs="+", help="List of years", required=True)
    parser.add_argument("--months", nargs="+", help="List of months", required=True)
    parser.add_argument("--variables", nargs="+", help="List of variables", required=True)
    parser.add_argument("--parallel", action="store_true", help="Enable parallel downloading")
    parser.add_argument("--cores", type=int, help="Number of cores to use for parallel downloading")
    args = parser.parse_args()

    if args.parallel:
        num_cores = args.cores if args.cores else multiprocessing.cpu_count()

        with ProcessPoolExecutor(max_workers=num_cores) as executor:
            for year in args.years:
                for month in args.months:
                    for variable in args.variables:
                        executor.submit(download_file, year, month, variable)
    else:
        for year in args.years:
            for month in args.months:
                for variable in args.variables:
                    download_file(year, month, variable)



