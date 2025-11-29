{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchurl,
  cmake,
  pkg-config,
  pandoc,
  gawk,
  makeWrapper,
  boehmgc,
  json_c,
  libffi,
  pcre,
  # Darwin-specific: GNU sed is required for build scripts
  gnused,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "ngs";
  version = "0.2.17";

  src = fetchFromGitHub {
    owner = "ngs-lang";
    repo = "ngs";
    rev = "v${finalAttrs.version}";
    hash = "sha256-j7OAXHADc2LlabKxVgYiKeDDtLttDVIavhQZSGyPGlE=";
  };

  # NGS requires peg 0.1.18 specifically due to custom patches in build-scripts/
  # The build scripts patch the leg output for location tracking, and this
  # patching is not compatible with newer peg versions (0.1.20+).
  # CMake will download and build peg 0.1.18 automatically when leg is not found.
  pegSrc = fetchurl {
    url = "https://www.piumarta.com/software/peg/peg-0.1.18.tar.gz";
    hash = "sha256-IBk73Wc/x0h6OJN+KX//CKpzdRtjOghqwow7NIkPkIQ=";
  };

  nativeBuildInputs =
    [
      cmake
      pkg-config
      pandoc
      gawk
      makeWrapper
    ]
    ++ lib.optionals stdenv.isDarwin [
      gnused
    ];

  buildInputs = [
    boehmgc
    json_c
    libffi
    pcre
  ];

  # The build scripts require GNU sed; on Darwin we need to ensure it's found
  # Also pre-populate the peg source to avoid network access during build
  preConfigure =
    ''
      # Create the external project download directory
      mkdir -p build/leg-prefix/src

      # Copy peg source to where CMake ExternalProject expects it
      cp ${finalAttrs.pegSrc} build/leg-prefix/src/peg-0.1.18.tar.gz
    ''
    + lib.optionalString stdenv.isDarwin ''
      export PATH="${gnused}/bin:$PATH"
    '';

  cmakeFlags = [
    "-DBUILD_MAN=ON"
  ];

  # Set NGS_PATH so the installed binary can find the standard library
  postInstall = ''
    wrapProgram $out/bin/ngs \
      --set NGS_PATH $out/lib/ngs
  '';

  meta = with lib; {
    description = "Next Generation Shell - a powerful programming language and shell designed for Ops";
    longDescription = ''
      NGS is a unique combination of select features borrowed from other
      languages and original features. NGS was built from the ground up
      focusing on daily systems engineering tasks.

      One way to think about NGS is bash plus data structures plus better
      syntax and error handling. Scripting AWS is much easier with NGS,
      there is a Declarative Primitives style library for that.
    '';
    homepage = "https://ngs-lang.org/";
    changelog = "https://github.com/ngs-lang/ngs/blob/v${finalAttrs.version}/CHANGELOG.md";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ ];
    mainProgram = "ngs";
    platforms = platforms.unix;
  };
})
