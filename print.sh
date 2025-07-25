{
  for f in *.eliom dune dune-project Makefile Makefile.app; do
    echo "===== $f ====="
    cat "$f"
    echo
  done
  echo "===== ls ====="
  ls 
  #echo "===== make test.opt ===="
  #make test.opt 2>&1
} 
