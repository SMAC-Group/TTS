# Set git config information
git config user.name "James Balamuta"
git config user.email "balamut2@illinois.edu"

# Clone the gh-pages repository
git clone -b gh-pages \
  https://${GH_TOKEN}@github.com/${TRAVIS_REPO_SLUG}.git \
  book-output

# Change book output directory
cd book-output

# Copy output
cp -r ../_book/* ./

# Add all files to the repo
git add *
git commit -a -m "Updating book (${TRAVIS_BUILD_NUMBER})"
git push origin gh-pages