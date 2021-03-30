# Should be linked to $ZSH_CUSTOM/functions.zsh
# Usually ~/.oh-my-zsh/custom/functions.zsh

function swap() {
  local TMPFILE=tmp.$$
  mv "$1" $TMPFILE && mv "$2" "$1" && mv $TMPFILE "$2"
}

function pics_compress() {
  COUNT=1
  echo "Compressing all jpgs in this folder with 1 sec delay between each..."
  for img in *.jpg; do
    mogrify -resize 1200x1200 -quality 80% -strip "$img"

    if (( COUNT % 10 == 0 )); then
      echo -n " $COUNT "
    else
      echo -n "."
    fi
    (( COUNT++ ))

    sleep 1
  done
  echo ""
  echo "Done"

}

function compress() {
  if [[ $1 == "--help" ]]; then
    echo "Usage: compress [FOLDERS...]"
    echo ""
    echo "Compresses all .jpg images in [FOLDERS...]. If no folders are provided,"
    echo "compress all images in the current directory"
    return
  fi

  # Compress all jpgs if no folders are specified
  if [[ $# -eq 0 ]]; then
    pics_compress
  else
    echo "Compressing all jpgs in: $*"
    for folder in "$@"; do
      if [[ -d "$folder" ]]; then
        (
        cd "$folder" || exit
        echo "In folder: $folder"
        pics_compress
        )
      else
        echo "WARNING: folder $folder does not exist"
      fi
    done
  fi
}

