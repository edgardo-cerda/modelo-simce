@echo off
chcp 65001 > nul
setlocal enabledelayedexpansion
title Prediccion SIMCE

echo.
echo =============================================================
echo  PREDICCION DE RESULTADOS SIMCE
echo =============================================================
echo.

rem --- Buscar Rscript ---------------------------------------------------
set "RSCRIPT="

for /f "delims=" %%i in ('where Rscript 2^>nul') do (
  if not defined RSCRIPT set "RSCRIPT=%%i"
)

if not defined RSCRIPT (
  for /f "delims=" %%i in ('dir /b /o-n "C:\Program Files\R\R-*" 2^>nul') do (
    if not defined RSCRIPT if exist "C:\Program Files\R\%%i\bin\Rscript.exe" (
      set "RSCRIPT=C:\Program Files\R\%%i\bin\Rscript.exe"
    )
  )
)

if not defined RSCRIPT (
  for /f "delims=" %%i in ('dir /b /o-n "%LOCALAPPDATA%\Programs\R\R-*" 2^>nul') do (
    if not defined RSCRIPT if exist "%LOCALAPPDATA%\Programs\R\%%i\bin\Rscript.exe" (
      set "RSCRIPT=%LOCALAPPDATA%\Programs\R\%%i\bin\Rscript.exe"
    )
  )
)

if not defined RSCRIPT (
  echo No se encontro R en este computador.
  echo.
  echo Instale R desde https://cran.r-project.org/bin/windows/base/
  echo y despues corra una vez instalar_dependencias.R
  echo.
  pause
  exit /b 1
)

echo Usando R:  %RSCRIPT%
echo.

rem --- Correr el driver -------------------------------------------------
"%RSCRIPT%" "%~dp0driver_prediccion.R"
set CODIGO=%ERRORLEVEL%

echo.
if %CODIGO% neq 0 (
  echo =============================================================
  echo  LA CORRIDA NO TERMINO BIEN. Lea el mensaje de arriba.
  echo =============================================================
) else (
  echo =============================================================
  echo  LISTO. Se abrio el informe en el navegador.
  echo =============================================================
)
echo.
pause
