# Nord theme

{
  colors = rec {
    nord0 = "#2e3440";  # Polar night
    nord1 = "#3b4252";  # Polar night
    nord2 = "#434c5e";  # Polar night
    nord3 = "#4c566a";  # Polar night
    nord4 = "#d8dee9";  # Snow storm
    nord5 = "#e5e9f0";  # Snow storm
    nord6 = "#eceff4";  # Snow storm
    nord7 = "#8fbcbb";  # Frost: "polar water"
    nord8 = "#88c0d0";  # Frost: "clear ice"
    nord9 = "#81a1c1";  # Frost: "arctic water"
    nord10 = "#5e81ac"; # Frost: "arctic ocean"
    nord11 = "#bf616a"; # Aurora: red
    nord12 = "#d08770"; # Aurora: orange
    nord13 = "#ebcb8b"; # Aurora: yellow
    nord14 = "#a3be8c"; # Aurora: green
    nord15 = "#b48ead"; # Aurora: purple

    # Color map
    background = nord0;
    background-alt = nord1;
    foreground = nord4;
    foreground-alt = nord5;
    buffer = nord3;
    urgent = nord11;
    warning = nord12;
    notify = nord13;
    info = nord10;
    success = nord14;
    function = nord15;

    primary = nord11;
    secondary = nord12;
    alert = nord11;
  };
}
