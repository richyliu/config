function swap() {
  local TMPFILE=tmp.$$
  mv "$1" $TMPFILE && mv "$2" "$1" && mv $TMPFILE "$2"
}

function rmdir() {
  # ignore .DS_Store when removing directories

  # source: http://hints.macworld.com/article.php?story=20091215071544343
  # only do this for interactive shells
  if [[ -n "${PS1}" ]]; then
    for dir in $@; do
      dss="${dir}/.DS_Store"
      # only delete if the .DS_Store is the only remaining file in directory
      if [[ -d "${dir}" && $(ls -a1 "${dir}" | wc -l | awk '{print $1}') = 3 && -f "${dss}" ]]; then
        rm "${dss}"
        echo "Note: also removed .DS_Store in ${dir}"
      fi
    done
  fi

  # execute the original rmdir command
  command rmdir $@
}
