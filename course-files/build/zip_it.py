import zipfile
import os
import argparse
import sys
from excluded import EXCLUDED


def exclude(filename):
    import re
    from builtins import any
    for pattern in EXCLUDED:
        # for word in file_tokens:
        match = re.search(pattern, filename)
        try:
            if match.group(0):
                return True
        except Exception:
            continue
    return False


def zipfolder(foldername, target_dir):

    zipname = foldername + '.zip'

    zipobj = zipfile.ZipFile(zipname, 'w', zipfile.ZIP_DEFLATED)

    rootlen = len(target_dir) + 1
    for base, dirs, files in os.walk(target_dir):
        for file in files:
            full_path_to_file = os.path.join(base, file)
            if not exclude(full_path_to_file):
                #print(full_path_to_file)
                zipobj.write(
                    full_path_to_file, zipname.split("/")[-1].split(".")[0] + "/" + full_path_to_file[rootlen:])
            else:
                print('exclude:', full_path_to_file)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("directory")
    parser.add_argument("--header")
    args = parser.parse_args()
    dir = args.directory + '/'

    filenames = [
        fname for fname in sorted(os.listdir(dir))
        if fname not in EXCLUDED and os.path.isfile(dir+fname)
    ]
    dirnames = [
        fname for fname in sorted(os.listdir(dir))
        if fname not in EXCLUDED
    ]
    dirnames = [
        dir + fname for fname in dirnames if fname not in filenames
    ]

    print(
        'About to make zip files for the following directories:',
        dirnames
    )

    # for dirname in dirnames:
    #     zipfolder(dirname, dirname)

    go_ahead = input('Would you like to proceed? (y/N) ')
    # print(go_ahead)
    if go_ahead.upper() == 'Y':
        for dirname in dirnames:
            zipfolder(dirname, dirname)
            if "lecture" not in dirname:
                if "solutions" in dirname:
                    os.rename(dirname + ".zip", dirname + ".zip")
                elif "practice" in dirname:
                    os.rename(dirname + ".zip", dirname + "_practice.zip")
                else:
                    os.rename(dirname + ".zip", dirname + "_template.zip")
    else:
        print('Cancelled.')

    sys.exit()


if __name__ == '__main__':
    main()
