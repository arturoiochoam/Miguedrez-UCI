@echo off
setlocal enabledelayedexpansion

set "PROJECT_ROOT=%~dp0"
set "PROJECT_ROOT_SAFE=%PROJECT_ROOT:~0,-1%"
set "SBCL=%PROJECT_ROOT%compiler\sbcl\sbcl.exe"
if not exist "%SBCL%" set "SBCL=C:\Lisp\sbcl.exe"
set "COMPILATION_DIR=%PROJECT_ROOT%compilation"
set "DIST_DIR=%PROJECT_ROOT%distribution"
set "BUILD_LOG=%COMPILATION_DIR%\build.log"
set "EXE_NAME=miguedrez-0.95.11.5-win64.exe"
set "ZIP_NAME=miguedrez-0.95.11.5-win64.zip"

echo === Miguedrez 0.95.11.5 UCI build ===
echo Project root: %PROJECT_ROOT%
echo SBCL: %SBCL% (SBCL 2.6.6 expected)

if not exist "%SBCL%" (
    echo ERROR: SBCL not found at %SBCL%
    exit /b 1
)

if not exist "%COMPILATION_DIR%" mkdir "%COMPILATION_DIR%"
if not exist "%DIST_DIR%" mkdir "%DIST_DIR%"

(
    echo Build started: %date% %time%
    "%SBCL%" --lose-on-corruption --dynamic-space-size 4096 --control-stack-size 12 --noinform --disable-debugger --non-interactive --load "%PROJECT_ROOT%build\build-windows-optimized.lisp"
    if errorlevel 1 (
        echo Build failed.
        exit /b 1
    )
    echo Build completed: %date% %time%
) > "%BUILD_LOG%" 2>&1
if errorlevel 1 (
    type "%BUILD_LOG%"
    exit /b 1
)

if not exist "%COMPILATION_DIR%\%EXE_NAME%" (
    echo ERROR: executable was not produced.
    type "%BUILD_LOG%"
    exit /b 1
)

"%SBCL%" --lose-on-corruption --dynamic-space-size 4096 --control-stack-size 12 --noinform --disable-debugger --non-interactive --load "%PROJECT_ROOT%build\run-self-tests.lisp" >> "%BUILD_LOG%" 2>&1
if errorlevel 1 (
    echo ERROR: clean-process self-test gate failed.
    type "%BUILD_LOG%"
    exit /b 1
)

"%COMPILATION_DIR%\%EXE_NAME%" --self-test >> "%BUILD_LOG%" 2>&1
if errorlevel 1 (
    echo ERROR: executable self-test failed.
    type "%BUILD_LOG%"
    exit /b 1
)

where python > nul 2> nul
if errorlevel 1 (
    echo ERROR: Python is required for packaging.
    exit /b 1
)

echo Creating distribution package...
if exist "%DIST_DIR%\%ZIP_NAME%" del "%DIST_DIR%\%ZIP_NAME%"

python "%PROJECT_ROOT%build\package.py" "%PROJECT_ROOT_SAFE%" "%DIST_DIR%" "%ZIP_NAME%" "%COMPILATION_DIR%\%EXE_NAME%"
if errorlevel 1 (
    echo ERROR: packaging failed.
    exit /b 1
)

if not exist "%DIST_DIR%\%ZIP_NAME%" (
    echo ERROR: distribution ZIP was not created.
    exit /b 1
)

echo.
echo Build successful.
echo Executable: %COMPILATION_DIR%\%EXE_NAME%
echo Checksum:   %COMPILATION_DIR%\%EXE_NAME%.sha256
echo Package:    %DIST_DIR%\%ZIP_NAME%
echo Log:        %BUILD_LOG%

endlocal
exit /b 0

