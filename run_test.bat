@echo off
echo ================================================
echo   Executando Sistema de Controle Remoto
echo ================================================
echo.

REM Verificar se os executáveis existem
if not exist "bin\RemoteServer.exe" (
    echo ERRO: RemoteServer.exe nao encontrado!
    echo Execute build.bat primeiro para compilar.
    pause
    exit /b 1
)

if not exist "bin\RemoteClient.exe" (
    echo ERRO: RemoteClient.exe nao encontrado!
    echo Execute build.bat primeiro para compilar.
    pause
    exit /b 1
)

echo Iniciando servidor...
start "Servidor de Controle Remoto" bin\RemoteServer.exe

echo Aguardando servidor inicializar...
timeout /t 2 /nobreak > nul

echo Iniciando cliente...
start "Cliente de Controle Remoto" bin\RemoteClient.exe

echo.
echo Sistema iniciado!
echo.
echo Instrucoes:
echo 1. No servidor: Clique em "Iniciar"
echo 2. No cliente: Clique em "Conectar"
echo 3. No servidor: Double-click no cliente para visualizar
echo.
