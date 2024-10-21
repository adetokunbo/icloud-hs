sync_nix () {
  set -eu
  local target_dir=$1
  local script_path="${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}"
  local script_dir=$(dirname ${script_path})
  local full_script_dir=$(realpath ${script_dir})
  local full_start_dir=$(dirname ${full_script_dir})
  local dest_path="${full_start_dir}/${target_dir}"
  local target_def_nix=${dest_path}/default.nix

  [[ -f $target_def_nix ]] || {
      # if target_dir does not contain default.nix, abort
      __sync_nix_abort "aborting: no default nix in ${dest_path}"
  }

  local default_nix=${full_script_dir}/default.nix
  local the_as_of=$(grep 'as of' ${default_nix})
  local the_old_as_of=$(grep 'as of' ${target_def_nix})
  local the_pin=$(grep 'h8x-pin =' ${default_nix})
  local the_old_pin=$(grep 'h8x-pin =' ${target_def_nix})
  local the_sources=$(grep 'h8x.sources' ${default_nix})
  local the_old_sources=$(grep 'h8x.sources' ${target_def_nix})
  local the_idx_state=$(grep 'index-state =' ${default_nix})
  local the_old_idx_state=$(grep 'index-state =' ${target_def_nix})
  local the_comp_name=$(grep 'compiler-nix-name =' ${default_nix})
  local the_old_comp_name=$(grep 'compiler-nix-name =' ${target_def_nix})
  echo "Updating default.nix in ${dest_path}"
  echo -e "as of: ${the_as_of}\n (before: ${the_old_as_of})"
  echo -e "h8x-pin: ${the_pin}\n (before: ${the_old_pin})"
  echo -e "sources: ${the_sources}\n (before: ${the_old_sources})"
  echo -e "index_state: ${the_idx_state}\n (before: ${the_old_idx_state})"
  echo -e "compiler-nix-name: ${the_comp_name}\n (before: ${the_old_comp_name})"
  sed -e "s#.*h8x\.sources.*#${the_sources}#" \
      -e "s#.*h8x-pin =.*#${the_pin}#" \
      -e "s#.*compiler-nix-name =.*#${the_comp_name}#" \
      -e "s#.*index-state =.*#${the_idx_state}#" \
      -e "s:.*as of.*:${the_as_of}:" \
      -i \
      ${target_def_nix}
  set +eu
}

__sync_nix_abort() {
    local message=${1:-"sync nix aborted"}
    echo $message
    set +eu
    exit 1
}

sync_nix "$@"
