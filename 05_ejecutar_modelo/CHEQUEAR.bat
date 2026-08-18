@echo off
chcp 65001 > nul
setlocal enabledelayedexpansion
title Chequeo de insumos

echo.
echo =============================================================
echo  CHEQUEO DE INSUMOS (no calcula nada, solo revisa)
echo =============================================================
echo.

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
  echo Instale R desde https://cran.r-project.org/bin/windows/base/
  echo.
  pause
  exit /b 1
)

"%RSCRIPT%" "%~dp000_validar_insumos.R"

echo.
pause
