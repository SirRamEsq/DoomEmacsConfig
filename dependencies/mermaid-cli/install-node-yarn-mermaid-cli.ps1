Function TestCommandExists {
 Param ($command)
 $oldPreference = $ErrorActionPreference
 $ErrorActionPreference = "stop"
  
 try {if(Get-Command $command){RETURN $true}}
 Catch {Write-Host $command "does not exist"ù; RETURN $false}
 Finally {$ErrorActionPreference=$oldPreference}
}
clear;

echo "Setting location to ~\.doom.d\dependencies\mermaid-cli"
Set-Location ~\.doom.d\dependencies\mermaid-cli


echo "Node installed?"
if (!(TestCommandExists npm)){echo "Nope. Go Install NPM!`nhttps://kalliphant.com/nodejs-npm-windows-install/"; exit}

echo "Yup"


echo "Yarn installed?"
$output = npm list -g | grep yarn
if([string]::IsNullOrEmpty($output)) {
    echo "Nope, installing with cmd 'npm install yarn'..."
    npm install -g yarn
} else {
    echo "Seems like it"
}

echo "Can I execute yarn?"
if (!(TestCommandExists yarn)){
    echo "Nope. Weird";
    echo "Ensure that the node path is in the Windows Environment Variables:"
    echo "node path is:"
    $nodePath= npm config get prefix
    echo $nodePath
    echo ""
    exit
} else{ echo "Yup, great"}

echo "Installling mermaid CLI locally..."
yarn init -y
yarn add @mermaid-js/mermaid-cli

echo "Running..."
./node_modules/.bin/mmdc -h