let
  base = self: super: {
    baseEnv = self.buildEnv {
      name = "env-base";
      paths = with self; [
        fzf
        pureZshPrompt
      ];
    };

    pureZshPrompt = with self; stdenv.mkDerivation rec {
      name = "zsh-pure-prompt-${version}";
      version = "v1.10.3";
      rev = "${version}";

      src = fetchFromGitHub {
        inherit rev;
        owner = "sindresorhus";
        repo = "pure";
        sha256 = "0zjgnlw01ri0brx108n6miw4y0cxd6al1bh28m8v8ygshm94p1zx";
      };

      installPhase = ''
        mkdir -p $out/share/zsh/site-functions
        install $src/pure.zsh $out/share/zsh/site-functions/prompt_pure_setup
        install $src/async.zsh $out/share/zsh/site-functions/async
      '';
    };
  };

in [
  base
]
