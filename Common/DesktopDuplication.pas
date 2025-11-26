unit DesktopDuplication;

interface

uses
  Winapi.Windows, Winapi.D3D11, Winapi.DXGI, Winapi.DxgiFormat, Winapi.DxgiType,
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Imaging.jpeg;

type
  TDesktopDuplicator = class
  private
    FDevice: ID3D11Device;
    FDeviceContext: ID3D11DeviceContext;
    FOutputDuplication: IDXGIOutputDuplication;
    FTexture: ID3D11Texture2D;
    FInitialized: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    function Initialize: Boolean;
    procedure Cleanup;
  public
    constructor Create;
    destructor Destroy; override;
    function CaptureScreen(Bitmap: TBitmap): Boolean;
    function CaptureScreenToJPEG(Quality: Integer = 75): TBytes;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Initialized: Boolean read FInitialized;
  end;

implementation

const
  D3D_DRIVER_TYPE_HARDWARE = 1;
  D3D11_SDK_VERSION = 7;

{ TDesktopDuplicator }

constructor TDesktopDuplicator.Create;
begin
  inherited Create;
  FInitialized := False;
  FDevice := nil;
  FDeviceContext := nil;
  FOutputDuplication := nil;
  FTexture := nil;
end;

destructor TDesktopDuplicator.Destroy;
begin
  Cleanup;
  inherited;
end;

procedure TDesktopDuplicator.Cleanup;
begin
  if Assigned(FOutputDuplication) then
  begin
    FOutputDuplication := nil;
  end;

  if Assigned(FTexture) then
  begin
    FTexture := nil;
  end;

  if Assigned(FDeviceContext) then
  begin
    FDeviceContext := nil;
  end;

  if Assigned(FDevice) then
  begin
    FDevice := nil;
  end;

  FInitialized := False;
end;

function TDesktopDuplicator.Initialize: Boolean;
var
  hr: HRESULT;
  DxgiDevice: IDXGIDevice;
  DxgiAdapter: IDXGIAdapter;
  DxgiOutput: IDXGIOutput;
  DxgiOutput1: IDXGIOutput1;
  OutputDesc: DXGI_OUTPUT_DESC;
  FeatureLevel: D3D_FEATURE_LEVEL;
begin
  Result := False;
  Cleanup;

  try
    // Create D3D11 Device
    hr := D3D11CreateDevice(
      nil,
      D3D_DRIVER_TYPE_HARDWARE,
      0,
      0,
      nil,
      0,
      D3D11_SDK_VERSION,
      @FDevice,
      @FeatureLevel,
      @FDeviceContext
    );

    if Failed(hr) then
      Exit;

    // Get DXGI Device
    hr := FDevice.QueryInterface(IDXGIDevice, DxgiDevice);
    if Failed(hr) then
      Exit;

    // Get DXGI Adapter
    hr := DxgiDevice.GetParent(IDXGIAdapter, Pointer(DxgiAdapter));
    if Failed(hr) then
      Exit;

    // Get output
    hr := DxgiAdapter.EnumOutputs(0, DxgiOutput);
    if Failed(hr) then
      Exit;

    // Get output description
    hr := DxgiOutput.GetDesc(OutputDesc);
    if Failed(hr) then
      Exit;

    FWidth := OutputDesc.DesktopCoordinates.Right - OutputDesc.DesktopCoordinates.Left;
    FHeight := OutputDesc.DesktopCoordinates.Bottom - OutputDesc.DesktopCoordinates.Top;

    // QI for Output 1
    hr := DxgiOutput.QueryInterface(IDXGIOutput1, DxgiOutput1);
    if Failed(hr) then
      Exit;

    // Create desktop duplication
    hr := DxgiOutput1.DuplicateOutput(FDevice, FOutputDuplication);
    if Failed(hr) then
      Exit;

    FInitialized := True;
    Result := True;
  except
    Cleanup;
    Result := False;
  end;
end;

function TDesktopDuplicator.CaptureScreen(Bitmap: TBitmap): Boolean;
var
  FrameInfo: DXGI_OUTDUPL_FRAME_INFO;
  DesktopResource: IDXGIResource;
  AcquiredDesktopImage: ID3D11Texture2D;
  hr: HRESULT;
  TextureDesc: D3D11_TEXTURE2D_DESC;
  StagingTexture: ID3D11Texture2D;
  MappedResource: D3D11_MAPPED_SUBRESOURCE;
  y: Integer;
  SrcPtr, DstPtr: PByte;
begin
  Result := False;

  if not FInitialized then
    if not Initialize then
      Exit;

  try
    // Try to acquire next frame
    hr := FOutputDuplication.AcquireNextFrame(100, FrameInfo, DesktopResource);

    if hr = DXGI_ERROR_WAIT_TIMEOUT then
    begin
      // No new frame, try to re-initialize
      Initialize;
      Exit;
    end;

    if Failed(hr) then
    begin
      if hr = DXGI_ERROR_ACCESS_LOST then
        Initialize;
      Exit;
    end;

    try
      // QI for ID3D11Texture2D
      hr := DesktopResource.QueryInterface(ID3D11Texture2D, AcquiredDesktopImage);
      if Failed(hr) then
        Exit;

      // Get texture description
      AcquiredDesktopImage.GetDesc(TextureDesc);

      // Create staging texture
      TextureDesc.Usage := D3D11_USAGE_STAGING;
      TextureDesc.BindFlags := 0;
      TextureDesc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_READ);
      TextureDesc.MiscFlags := 0;

      hr := FDevice.CreateTexture2D(@TextureDesc, nil, StagingTexture);
      if Failed(hr) then
        Exit;

      try
        // Copy desktop image to staging texture
        FDeviceContext.CopyResource(StagingTexture, AcquiredDesktopImage);

        // Map the staging texture
        hr := FDeviceContext.Map(StagingTexture, 0, D3D11_MAP_READ, 0, MappedResource);
        if Failed(hr) then
          Exit;

        try
          // Setup bitmap
          Bitmap.PixelFormat := pf32bit;
          Bitmap.SetSize(TextureDesc.Width, TextureDesc.Height);

          // Copy pixels
          SrcPtr := MappedResource.pData;
          for y := 0 to Integer(TextureDesc.Height) - 1 do
          begin
            DstPtr := Bitmap.ScanLine[y];
            Move(SrcPtr^, DstPtr^, TextureDesc.Width * 4);
            Inc(SrcPtr, MappedResource.RowPitch);
          end;

          Result := True;
        finally
          FDeviceContext.Unmap(StagingTexture, 0);
        end;
      finally
        StagingTexture := nil;
      end;
    finally
      FOutputDuplication.ReleaseFrame;
    end;
  except
    on E: Exception do
    begin
      Initialize; // Try to reinitialize on error
      Result := False;
    end;
  end;
end;

function TDesktopDuplicator.CaptureScreenToJPEG(Quality: Integer = 75): TBytes;
var
  Bitmap: TBitmap;
  JPEGImage: TJPEGImage;
  Stream: TMemoryStream;
begin
  SetLength(Result, 0);
  Bitmap := TBitmap.Create;
  JPEGImage := TJPEGImage.Create;
  Stream := TMemoryStream.Create;
  try
    if CaptureScreen(Bitmap) then
    begin
      JPEGImage.Assign(Bitmap);
      JPEGImage.CompressionQuality := Quality;
      JPEGImage.SaveToStream(Stream);

      SetLength(Result, Stream.Size);
      Stream.Position := 0;
      Stream.Read(Result[0], Stream.Size);
    end;
  finally
    Stream.Free;
    JPEGImage.Free;
    Bitmap.Free;
  end;
end;

end.
