# Erros Comuns e Soluções

## Erros de Compilação

### 1. "Unit not found: Protocol"

**Erro:**
```
[dcc32 Error] MainForm.pas(8): E2202 Required package 'Protocol' not found
```

**Causa:** Caminho de busca não configurado para a pasta Common.

**Solução:**
1. Abrir Project > Options
2. Ir em Delphi Compiler > Search Path
3. Adicionar: `..\Common`
4. Clicar OK e recompilar

### 2. "Undeclared identifier: 'ID3D11Device'"

**Erro:**
```
[dcc32 Error] DesktopDuplication.pas(12): E2003 Undeclared identifier: 'ID3D11Device'
```

**Causa:** Units do DirectX não estão na cláusula uses.

**Solução:**
Verificar se as seguintes units estão declaradas em `DesktopDuplication.pas`:
```pascal
uses
  Winapi.Windows, Winapi.D3D11, Winapi.DXGI, Winapi.DxgiFormat,
  Winapi.DxgiType, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Imaging.jpeg;
```

### 3. "Incompatible types: 'string' and 'string[255]'"

**Erro:**
```
[dcc32 Error] Protocol.pas(45): E2010 Incompatible types: 'string' and 'string[255]'
```

**Causa:** TClientInfo usa strings com tamanho fixo.

**Solução:**
Ao atribuir valores, fazer cast ou usar Copy:
```pascal
Result.ComputerName := Copy(string(Buffer), 1, Len);
```

### 4. "Access violation at address..."

**Erro:**
```
Access violation at address 00401234 in module 'RemoteServer.exe'.
Read of address 00000000.
```

**Causa:** Ponteiro nulo ou objeto não inicializado.

**Solução:**
Sempre verificar se objetos estão criados antes de usar:
```pascal
if Assigned(FServer) then
  FServer.DoSomething;
```

## Erros de Execução

### 5. "Bind error: Address already in use"

**Erro:** Servidor não consegue iniciar - porta em uso.

**Causa:** Outro processo está usando a porta ou o servidor anterior não foi fechado corretamente.

**Solução:**
```batch
# Verificar qual processo está usando a porta
netstat -ano | findstr :9999

# Matar o processo (substitua PID pelo número encontrado)
taskkill /PID 1234 /F
```

Ou usar a opção `SO_REUSEADDR` (já implementada em `ServerConnection.pas:80`).

### 6. Desktop Duplication falha: "DXGI_ERROR_ACCESS_LOST"

**Erro:** Captura de tela retorna erro de acesso perdido.

**Causa:**
- Mudança de resolução da tela
- Troca de modo de exibição
- Bloqueio de tela
- Sessão RDP ativa

**Solução:**
O código já implementa reinicialização automática em `DesktopDuplication.pas:148`:
```pascal
if hr = DXGI_ERROR_ACCESS_LOST then
  Initialize;
```

### 7. "Desktop Duplication não funciona via RDP"

**Problema:** Tela preta ao conectar via Remote Desktop.

**Causa:** Desktop Duplication API não funciona em sessões RDP redirecionadas.

**Solução:**
Usar acesso físico ou considerar implementar fallback com GDI:
```pascal
// Adicionar em DesktopDuplication.pas
function CaptureScreenGDI(Bitmap: TBitmap): Boolean;
var
  DC: HDC;
begin
  DC := GetDC(0);
  try
    Bitmap.SetSize(GetSystemMetrics(SM_CXSCREEN),
                   GetSystemMetrics(SM_CYSCREEN));
    BitBlt(Bitmap.Canvas.Handle, 0, 0,
           Bitmap.Width, Bitmap.Height,
           DC, 0, 0, SRCCOPY);
    Result := True;
  finally
    ReleaseDC(0, DC);
  end;
end;
```

### 8. "Cliente não conecta ao servidor"

**Problema:** Cliente exibe "Erro ao conectar ao servidor!"

**Causas e Soluções:**

**A) Firewall bloqueando:**
```batch
# Adicionar regra no firewall (executar como Admin)
netsh advfirewall firewall add rule name="Remote Server" dir=in action=allow protocol=TCP localport=9999
```

**B) IP incorreto:**
- Verificar IP do servidor com `ipconfig`
- Testar com localhost (127.0.0.1) primeiro

**C) Servidor não está rodando:**
- Verificar se o servidor foi iniciado
- Verificar mensagem "Servidor iniciado na porta 9999"

**D) Porta diferente:**
- Verificar se cliente e servidor usam a mesma porta

### 9. "Ping sempre mostra '-'"

**Problema:** Coluna PING não mostra valores.

**Causa:** Cliente não está respondendo ao PING ou há problema de sincronização.

**Solução:**
Verificar em `ClientMain.pas` se o comando PING está sendo processado:
```pascal
CMD_PING:
begin
  FClient.SendPong;
  Log('Ping recebido');
end;
```

### 10. Performance ruim / Lag na transmissão

**Problema:** Imagem trava ou atualiza muito devagar.

**Soluções:**

**A) Reduzir qualidade JPEG:**
Em `ClientConnection.pas:206`, alterar qualidade:
```pascal
ScreenData := FDuplicator.CaptureScreenToJPEG(50); // Era 75
```

**B) Aumentar intervalo de captura:**
Em `ClientConnection.pas:221`, aumentar sleep:
```pascal
Sleep(150); // Era 100 - reduz FPS mas melhora performance
```

**C) Reduzir resolução:**
Adicionar redimensionamento antes de comprimir em `DesktopDuplication.pas`.

### 11. "Memory leak" / Consumo excessivo de memória

**Problema:** Aplicação consome cada vez mais memória.

**Causa:** Objetos não estão sendo liberados corretamente.

**Solução:**
Verificar se todos os `Create` têm `Free` correspondente:
```pascal
// Sempre usar try-finally
Stream := TMemoryStream.Create;
try
  // usar stream
finally
  Stream.Free;
end;
```

### 12. Travamento ao fechar servidor com clientes conectados

**Problema:** Servidor trava ao clicar em "Parar".

**Causa:** Threads de clientes não estão sendo finalizadas corretamente.

**Solução:**
Verificar implementação em `ServerConnection.pas:143`:
```pascal
for i := 0 to List.Count - 1 do
begin
  List[i].Terminate;
  closesocket(List[i].Socket);
end;
```

### 13. Controle de mouse/teclado não funciona

**Problema:** Comandos enviados mas cliente não responde.

**Causas:**

**A) Aplicação precisa rodar como Administrador:**
- Botão direito no executável
- "Executar como administrador"

**B) Verificar permissões UAC:**
Em alguns casos, `SendInput` é bloqueado por segurança do Windows.

**Solução temporária:**
Desabilitar UAC (não recomendado para produção):
```
Control Panel > User Accounts > Change User Account Control settings
```

### 14. "Stack overflow" em loop de rede

**Erro:**
```
Runtime Error 202 at 00401234
Stack overflow
```

**Causa:** Recursão infinita ou buffer muito grande na stack.

**Solução:**
Usar heap em vez de stack para buffers grandes:
```pascal
// Ruim
var
  Buffer: array[0..1000000] of Byte;

// Bom
var
  Buffer: TBytes;
begin
  SetLength(Buffer, 1000000);
```

## Erros de Direct3D

### 15. "D3D11CreateDevice failed"

**Erro:** Desktop Duplication não inicializa.

**Causas:**
1. **DirectX 11 não instalado**
   - Baixar e instalar DirectX End-User Runtime

2. **Driver de vídeo desatualizado**
   - Atualizar driver da placa de vídeo

3. **Placa de vídeo não suporta DX11**
   - Verificar especificações da placa
   - Usar fallback GDI (ver solução #7)

### 16. "DXGI_ERROR_UNSUPPORTED"

**Erro:** Formato de pixel não suportado.

**Solução:**
Verificar se o formato em `DesktopDuplication.pas` está correto:
```pascal
TextureDesc.Format := DXGI_FORMAT_B8G8R8A8_UNORM; // Formato mais compatível
```

## Dicas de Debug

### Habilitar logs detalhados

Adicionar em pontos críticos:
```pascal
procedure Log(const Msg: string);
var
  F: TextFile;
begin
  AssignFile(F, 'debug.log');
  if FileExists('debug.log') then
    Append(F)
  else
    Rewrite(F);
  try
    WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Msg);
  finally
    CloseFile(F);
  end;
end;
```

### Verificar tráfego de rede

Usar Wireshark para analisar pacotes:
```
Filter: tcp.port == 9999
```

### Monitor de recursos

Usar Task Manager para monitorar:
- CPU usage
- Memory usage
- Network traffic

## Otimizações Recomendadas

1. **Implementar delta encoding**: Enviar apenas mudanças na tela
2. **Usar codec H.264**: Melhor compressão que JPEG para vídeo
3. **Implementar threading pool**: Reutilizar threads em vez de criar novas
4. **Adicionar buffer de rede**: Reduzir overhead de envio
5. **Implementar QoS**: Priorizar pacotes de controle sobre tela

## Contato para Suporte

Para problemas não listados aqui:
1. Verificar versão do Delphi (deve ser 12.3)
2. Verificar versão do Windows (mínimo Windows 8)
3. Testar em máquina diferente
4. Revisar mensagens de erro no Event Viewer do Windows
