module.exports = {
  defaultBrowser: "Safari",
  options: {
    hideIcon: true
  },
  handlers: [
    {
      match: [
        // Open Schoology urls in Chrome
        "fuhsd.schoology.com*",
        // Open all google docs in Chrome by default
        "docs.google.com*",
        // Open padlet docs in Chrome
        "*padlet.org*",
        // Open quizlet links in Chrome
        "quizlet.com*",
        // Open college apps stuff in Chrome
        "apply.mitadmissions.org*",
        "myillini.illinois.edu*",
        // Open zoom sign in links in Chrome
        "*.zoom.us*",
      ],
      browser: "Google Chrome"
    },
  ]
};
