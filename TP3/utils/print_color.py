class bcolors:
    DER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'  # Reset code
    BOLD = '\033[1m'
    HEA = '\033[0m'  # Reset code


def print_color(text, color_code):
    print(f"{color_code}{text}{bcolors.ENDC}")
