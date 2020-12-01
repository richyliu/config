module.exports = {
  defaultBrowser: "Safari",
  handlers: [
    {
      match: [
        // Open Schoology urls in Chrome
        "fuhsd.schoology.com*",
        // Oepn all google docs in Chrome by default
        "docs.google.com*",
        // Oepn padlet docs in Chrome
        "*padlet.org*",
        // Open college apps stuff in Chrome
        "apply.mitadmissions.org*",
        "myillini.illinois.edu*",
      ],
      browser: "Google Chrome"
    },
  ]
};
