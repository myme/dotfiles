# Dracula theme

{
  colors = rec {
    dracula0 = "#282a36"; # background
    dracula1 = "#3b4252"; # background
    dracula2 = "#434c5e"; # background
    dracula3 = "#4c566a"; # background
    dracula4 = "#f8f8f2"; # foreground
    dracula5 = "#e5e9f0"; # foreground
    dracula6 = "#eceff4"; # foreground
    dracula7 = "#5af78e"; # bright green
    dracula8 = "#1ef956"; # dim green
    dracula9 = "#4d5b86";  # dim blue
    dracula10 = "#bd93f9"; # blue
    dracula11 = "#ff5555"; # red
    dracula12 = "#d08770"; # orange
    dracula13 = "#f1fa8c"; # yellow
    dracula14 = "#50fa7b"; # green
    dracula15 = "#ff79c6"; # purple

    # Color map
    background = dracula0;
    background-alt = dracula1;
    foreground = dracula4;
    foreground-alt = dracula5;
    buffer = dracula3;
    urgent = dracula11;
    warning = dracula12;
    notify = dracula13;
    info = dracula15;
    success = dracula14;
    function = dracula10;

    primary = dracula12;
    secondary = dracula13;
    alert = dracula11;
  };
}
