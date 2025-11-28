@echo off
echo ================================================
echo   Compilando Sistema de Controle Remoto
echo ================================================
echo.

REM Definir caminho do compilador Delphi
REM Ajuste conforme sua instalação
set DELPHI_PATH=C:\Program Files (x86)\Embarcadero\Studio\23.0\bin
set DCC32=%DELPHI_PATH%\dcc32.exe

REM Verificar se o compilador existe
if not exist "%DCC32%" (
    echo ERRO: Compilador Delphi nao encontrado em: %DCC32%
    echo.
    echo Ajuste a variavel DELPHI_PATH no arquivo build.bat
    echo com o caminho correto da sua instalacao do Delphi.
    pause
    exit /b 1
)

echo Compilador encontrado: %DCC32%
echo.

REM Criar diretório de saída se não existir
if not exist "bin" mkdir bin

echo ================================================
echo   Compilando Servidor...
echo ================================================
cd ServerApp
"%DCC32%" -B -U..\Common -E..\bin RemoteServer.dpr
if errorlevel 1 (
    echo ERRO: Falha ao compilar o servidor!
    cd ..
    pause
    exit /b 1
)
cd ..
echo Servidor compilado com sucesso!
echo.

echo ================================================
echo   Compilando Cliente...
echo ================================================
cd ClientApp
"%DCC32%" -B -U..\Common -E..\bin RemoteClient.dpr
if errorlevel 1 (
    echo ERRO: Falha ao compilar o cliente!
    cd ..
    pause
    exit /b 1
)
cd ..
echo Cliente compilado com sucesso!
echo.

echo ================================================
echo   Compilacao Concluida!
echo ================================================
echo.
echo Executaveis gerados em: %CD%\bin
echo   - RemoteServer.exe (Servidor)
echo   - RemoteClient.exe (Cliente)
echo.
pause
