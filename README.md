# Sistema de Controle Remoto com Desktop Duplication

Sistema cliente-servidor desenvolvido em Delphi 12.3 para controle remoto de desktops usando Windows Desktop Duplication API (DGI).

## Características

### Servidor
- **Dashboard em tempo real** com informações dos clientes conectados:
  - ID do cliente
  - Nome da máquina
  - Endereço IP
  - Endereço MAC
  - Antivírus instalado
  - Ping em tempo real (ms)

- **Visualização remota**: Double-click no cliente abre painel com:
  - Imagem da tela do cliente em tempo real
  - Controle de mouse remoto
  - Controle de teclado remoto
  - Comandos para travar/destravar tela

- **Gerenciamento de múltiplas conexões**: Suporta vários clientes simultaneamente

### Cliente
- Conexão TCP/IP pura ao servidor
- Captura de tela usando Desktop Duplication API (alta performance)
- Compressão JPEG com ZLib para transmissão eficiente
- Execução de comandos remotos:
  - Movimento e cliques de mouse
  - Teclas do teclado
  - Travamento de tela
- Envio automático de informações do sistema

## Estrutura do Projeto

```
desktop-duplication/
├── Common/                      # Units compartilhadas
│   ├── Protocol.pas            # Protocolo de comunicação
│   ├── Compression.pas         # Compressão de dados (ZLib)
│   ├── SystemInfo.pas          # Coleta de informações do sistema
│   └── DesktopDuplication.pas  # Desktop Duplication API
│
├── ServerApp/                   # Aplicação servidor
│   ├── RemoteServer.dpr        # Projeto principal
│   ├── MainForm.pas/.dfm       # Dashboard principal
│   ├── RemoteViewForm.pas/.dfm # Visualização remota
│   └── ServerConnection.pas    # Gerenciamento TCP
│
└── ClientApp/                   # Aplicação cliente
    ├── RemoteClient.dpr        # Projeto principal
    ├── ClientMain.pas/.dfm     # Interface do cliente
    └── ClientConnection.pas    # Conexão e comandos
```

## ⚠️ Correções Aplicadas

**IMPORTANTE**: Este código já foi corrigido para compilar corretamente no Delphi 12.3.

As seguintes correções foram aplicadas:
- ✅ Tipos de string em Protocol.pas (Unicode strings com UTF-8 encoding)
- ✅ Import de Winapi.Winsock em MainForm.pas
- ✅ Construtor de TZCompressionStream atualizado para Delphi 12.3

Ver `BUILD_FIX.md` para detalhes completos das correções.

## Requisitos

- **Delphi 12.3** (Athens)
- **Windows 8 ou superior** (para Desktop Duplication API)
- **DirectX 11** (normalmente já instalado no Windows)

## Compilação

### No Delphi IDE

1. **Compilar o Servidor:**
   - Abrir `ServerApp/RemoteServer.dpr`
   - Adicionar os caminhos de busca (Project > Options > Delphi Compiler > Search Path):
     - `..\Common`
   - Build > Compile
   - Build > Build

2. **Compilar o Cliente:**
   - Abrir `ClientApp/RemoteClient.dpr`
   - Adicionar os caminhos de busca (Project > Options > Delphi Compiler > Search Path):
     - `..\Common`
   - Build > Compile
   - Build > Build

### Linha de Comando

```batch
# Servidor
cd ServerApp
dcc32 RemoteServer.dpr -U..\Common

# Cliente
cd ClientApp
dcc32 RemoteClient.dpr -U..\Common
```

## Uso

### Servidor

1. Executar `RemoteServer.exe`
2. Definir porta (padrão: 9999)
3. Clicar em "Iniciar"
4. Aguardar conexões de clientes
5. Double-click em um cliente para visualizar/controlar remotamente

### Cliente

1. Executar `RemoteClient.exe`
2. Inserir IP do servidor
3. Inserir porta (padrão: 9999)
4. Clicar em "Conectar"
5. Opcionalmente, marcar "Iniciar automático" para conexão na inicialização

## Protocolo de Comunicação

O sistema usa um protocolo binário personalizado com os seguintes comandos:

| Comando | Código | Descrição |
|---------|--------|-----------|
| CLIENT_INFO | 0x01 | Cliente envia informações do sistema |
| PING | 0x02 | Servidor envia ping |
| PONG | 0x03 | Cliente responde ao ping |
| SCREEN_START | 0x04 | Iniciar captura de tela |
| SCREEN_STOP | 0x05 | Parar captura de tela |
| SCREEN_DATA | 0x06 | Dados da tela (JPEG comprimido) |
| MOUSE_MOVE | 0x07 | Movimento do mouse |
| MOUSE_CLICK | 0x08 | Click do mouse |
| KEYBOARD | 0x09 | Tecla pressionada/solta |
| LOCK_SCREEN | 0x0A | Travar tela |
| UNLOCK_SCREEN | 0x0B | Destravar tela |
| DISCONNECT | 0xFF | Desconectar |

### Formato do Pacote

```
[Comando: 1 byte][Tamanho: 4 bytes][Dados: N bytes]
```

## Tecnologias Utilizadas

- **Desktop Duplication API (DXGI)**: Captura de tela de alta performance
- **Direct3D 11**: Processamento de imagens
- **WinSock**: Comunicação TCP/IP
- **ZLib**: Compressão de dados
- **JPEG**: Compressão de imagens

## Segurança

⚠️ **IMPORTANTE**: Este sistema foi desenvolvido para uso em redes locais confiáveis.

Para uso em produção, considere adicionar:
- Autenticação de clientes
- Criptografia de dados (TLS/SSL)
- Controle de acesso baseado em permissões
- Logs de auditoria

## Performance

- **Taxa de atualização**: ~10 FPS (configurável em `ClientConnection.pas:225`)
- **Qualidade JPEG**: 75% (configurável em `DesktopDuplication.pas:183`)
- **Compressão**: ZLib padrão (ajustável em `Compression.pas`)

## Troubleshooting

Consulte o arquivo `ERROS_E_SOLUCOES.md` para problemas comuns e suas soluções.

## Licença

Projeto desenvolvido para fins educacionais e comerciais.

## Autor

Sistema desenvolvido em Delphi 12.3
Desktop Duplication API - Windows 8+
