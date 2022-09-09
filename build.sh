# zip everything:
python3 course-files/build/zip_it.py course-files/homework
python3 course-files/build/zip_it.py course-files/lectures
python3 course-files/build/zip_it.py course-files/tutorials

# Build the Project 2 docs
pdoc --show-source course-files/projects/project01/utilities -o course-files/projects/project01/docs

pdoc --show-source --logo "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Yelp_Logo.svg/800px-Yelp_Logo.svg.png" course-files/projects/project02/apis/yelp -o course-files/projects/project02/docs
pdoc --show-source --logo "https://upload.wikimedia.org/wikipedia/commons/thumb/7/7e/Twilio-logo-red.svg/2560px-Twilio-logo-red.svg.png" course-files/projects/project02/apis/twilio -o course-files/projects/project02/docs
pdoc --show-source --logo "https://upload.wikimedia.org/wikipedia/commons/thumb/2/26/Spotify_logo_with_text.svg/1024px-Spotify_logo_with_text.svg.png" course-files/projects/project02/apis/spotify -o course-files/projects/project02/docs

python3 course-files/build/zip_it.py course-files/projects

# build indexes:
# python build_navigator.py ../.
