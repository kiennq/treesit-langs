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
        $version = "`"$version`""
      }
      $code = "(treesit-langs-create-bundle nil nil $version)"
    }
    'changed' {
      $base = $target
      if (!$base) {
        $base = "origin/master"
      }
      $code = "(treesit-langs-compile-changed-or-all \`"$base\`")"
    }
    default { $code = "(treesit-langs-compile '$lang)" }
  }
  emacs --batch `
    --directory "$project_root" `
    --load treesit-langs-build `
    --eval "$code"
}
finally {
  Pop-Location
}
