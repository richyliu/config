module.exports = {
  defaultBrowser: "Safari",
  handlers: [
    {
      match: [
        // Open Schoology urls in Chrome
        "fuhsd.schoology.com*",
        // Oepn all google docs in Chrome by default
        "docs.google.com",
        // Open college apps stuff in Chrome
        "apply.mitadmissions.org*",
        "myillini.illinois.edu*",
      ],
      browser: "Google Chrome"
    },
  ]
};
