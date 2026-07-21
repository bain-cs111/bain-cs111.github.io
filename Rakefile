require "html-proofer"

# Note: this is NOT run during CI.
task :test do
  #sh "make build" you should build before a test
  options = { ignore_urls: [
    # 403s
    "https://www.nytimes.com/2024/07/26/style/what-is-brat-green.html",
    "https://www.autodesk.com/products/maya/bifrost",
    "https://www.respondus.com/lockdown/download.php?id=171646780",
    # Other
    # SSL Error
    "https://sitn.hms.harvard.edu/flash/2020/racial-discrimination-in-face-recognition-technology/",
    # Issue with # notation
    "https://www.netlogoweb.org/launch#https://ccl.northwestern.edu/netlogo/models/models/IABM%20Textbook/chapter%201/Ants%20Simple.nlogo",
    # ???
    "https://northwestern.apporto.com",
    ]}
  HTMLProofer.check_directory("./_site", options).run
end