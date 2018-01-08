:: Equivalent of install script for Windows 10
:: TODO try cmder and clink (cmder uses clink)

@echo off

:: Run Ctrl2Cap
:: ; Run a command when CMD.exe starts
:: [HKEY_LOCAL_MACHINE\Software\Microsoft\Command Processor]
:: "AutoRun"=C:\Users\mikep\_cmdrc

:: Update GAC.ps1 (update Global Assembly Cache, speeds up powershell startup)
Set-Alias ngen (Join-Path ([System.Runtime.InteropServices.RuntimeEnvironment]::GetRuntimeDirectory()) ngen.exe)
[AppDomain]::CurrentDomain.GetAssemblies() |
    sort {Split-path $_.location -leaf} | 
    %{
        $Name = (Split-Path $_.location -leaf)
        if ([System.Runtime.InteropServices.RuntimeEnvironment]::FromGlobalAccessCache($_))
        {
            Write-Host “Already GACed: $Name”
        }else
        {
            Write-Host -ForegroundColor Yellow “NGENing      : $Name”
            ngen $_.location | %{“`t$_”}
         }
      }

:: Get-ExecutionPolicy (if wrong) Set-ExecutionPolicy RemoteSigned

:: Disable unwanted Windows features? (started in powershell script on desktop)

:: ln -s equivalent for vimrc -> $HOME/_vimrc

:: Potentially autohotkey can introduce the readline keyboard shortcuts you're
:: looking for.
:: #IfWinActive, ahk_class ConsoleWindowClass
::   ^d::
::     ; First send ESC, in case we're in select mode.
::     Send {Esc}{Esc}exit{Enter}
:: #IfWinActive
DOSKEY e=exit
