# Course Website for CS 111 @ NU

This repo contains all of the student-facing content for Prof. Bain's COMP_SCI 111 course at Northwestern University. While this is a working jekyll site (i.e. it can be served via GitHub pages), it is mostly tailored to create HTML content for deployment onto the Canvas LMS. The template it uses is based off of `minima`.

## `pages/_lectures`

These are lecture pages. Add a new one by creating a new `md` file inside that folder. These are very structured and templated around Prof. Bain's course rhythm.

# `pages/_assignments`

These are assignment pages. Add a new one by creating a new `md` file inside that folder. They are distinguished from lectures by having a left-hand navigation panel.

# `pages/_resources`

These are one-off resource pages with very little template-work. Add new resources by creating a new `md` file inside that folder.

# `course-files`

This is where the majority of assignment templates, library files, etc., are stored. It is structured around a remote repo, `nu-cs111/cs111-library` that provides some common library files for assignments in 111. Use symbolic links to reference these files in specific assignment folders (so if the library is updated, then those assignments will also be automatically updated). In general, we avoid linking to `rkt` files directly and instead link to built `zip` files (see `build.sh`).

# `assets`

The usual assets folder for HTML content.

# Development and Running Locally

You'll need `ruby` (3.3.3) and `bundler` (2.5.14) installed on your local machine. If you're on a Mac with `brew.sh` installed, you can just:

```bash
brew install ruby # follow the instructions to link this ruby to override your system ruby
gem install bundler
```

Then to install the required rubygems, run:

```bash
bundle install
```

And finally, if you'd like to host locally to preview site contents:

```bash
bundle exec jekyll serve
```

# GitHub Actions and Hosting

While this repo itself is private, the site it generates is meant to be publicly available. As such, the build workflow on GitHub Actions, is a little more complex than normal.

Current Build Status: ![build status](https://github.com/bain-cs111/web/actions/workflows/jekyll/badge.svg)

1. Checkout the main repo 🛎️
2. Checkout the `nu-cs111/cs111-library` sub-module 🛎️ (which pulls in 111 libraries to be used in assignments)
  * Note, this requires an `NU_CS111_TOKEN` which needs to have read access to this repo
3. Setup Ruby 3.3.3
4. Install and Build 🔧
5. Deploy to GitHub Pages on the `bain-cs111/bain-cs111.github.io` repo
  * Using `peaceiris/actions-gh-pages`, we deploy the built site as an orphaned commit on this public repo
  * This requires `ACTIONS_DEPLOY_KEY` to have write access to the `bain-cs111/bain-cs111.github.io` repo

This allows us to obfuscate page updates and past commits from students while maintaining the commit history for future course development.

# Deployment to Canvas

Currently handled by a manually run deployment script in `bain-cs111/internal`. While the Canvas API does accept raw HTML, it does not support external CSS (nor does it support all CSS). Therefore that script must also `inline` the relevant CSS styles in order to be uploaded as a Canvas page. While that's annoying, we can also take advantage of hiding assignment details in each HTML page to determine whether a page in this site should be uploaded to Canvas as a Page, Assignment, Quiz, etc.
