Function TestCommandExists {
 Param ($command)
 $oldPreference = $ErrorActionPreference
 $ErrorActionPreference = "stop"
  
 try {if(Get-Command $command){RETURN $true}}
 Catch {Write-Host $command "does not exist"ù; RETURN $false}
 Finally {$ErrorActionPreference=$oldPreference}
}
clear;

echo "Setting location to ~\.doom.d\dependencies\rufo"
Set-Location ~\.doom.d\dependencies\rufo


echo "gem installed?"
if (!(TestCommandExists gem)){echo "Nope. Go Install gem!"; exit}

echo "Yup"

echo "Installling rufo..."
gem install rufo