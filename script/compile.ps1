param(
  [parameter(Position = 0)] $lang,
  [string] $target,
  [string] $version
)

$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName

Push-Location $project_root
try {
  switch ($lang) {
    'all' {
      if (!$version) {
        $version = "nil"
      } else {
        $version = "\`"$version\`""
      }
      $code = "(tree-sitter-langs-create-bundle nil nil $version)"
    }
    'changed' {
      $base = $target
      if (!$base) {
        $base = "origin/master"
      }
      $code = "(tree-sitter-langs-compile-changed-or-all \`"$base\`")"
    }
    default { $code = "(tree-sitter-langs-compile '$lang)" }
  }
  emacs --batch `
    --directory "$project_root" `
    --load tree-sitter-langs-build `
    --eval "$code"
}
finally {
  Pop-Location
}
