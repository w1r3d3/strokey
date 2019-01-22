onerror=err
function err(msg,url,l)
{
  alert("There was an error on this page.\n\n"+
    "Error: " + msg + "\nURL: " + url + "\nLine: " + l + "\n\n");
  return true;
}

var l_shift_key = -1;
var r_shift_key = -1;
var l_ctrl_key  = -1;
var r_ctrl_key  = -1;
var alt_key     = -1;
var altgr_key   = -1;

var l_shift_state = false;
var r_shift_state = false;
var l_ctrl_state  = false;
var r_ctrl_state  = false;
var alt_state     = false;
var altgr_state   = false;

var l_shift_prefix  = "";
var r_shift_prefix  = "";
var l_ctrl_prefix   = "";
var r_ctrl_prefix   = "";
var alt_prefix      = "";
var altgr_prefix    = "";

var desc_tbl  = new Array();
var shift_tbl = new Array();
var altgr_tbl = new Array();

var keycode     = new Array();
var keycode_pos = 0;

var hex_tbl = new Array('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

function img(i) {return "<img src=\"img/" + i + ".gif\">";}
function txt(t,s) {return "<font style=\"" + s + "\">" + t + "</font>";}
function div(t,s) {return "<div style=\"" + s + "\">" + t + "</div>";}

function load_german_tbls()
{
  l_shift_key = 0x12;
  r_shift_key = 0x59;
  l_ctrl_key  = 0x14;
  r_ctrl_key  = 0xE014;
  alt_key     = 0x11;
  altgr_key   = 0xE011;

  l_shift_prefix  = img("shift_l");
  r_shift_prefix  = img("shift_r");
  l_ctrl_prefix   = img("ctrl_l");
  r_ctrl_prefix   = img("ctrl_r");
  alt_prefix      = img("alt");
  altgr_prefix    = img("alt_gr");

  desc_tbl[0x58] = img("caps");
  desc_tbl[0x76] = img("esc");
  desc_tbl[0x0D] = img("tab");
  desc_tbl[0x5A] = img("return");
  desc_tbl[0x29] = img("space");
  desc_tbl[0x66] = img("bksp");

  desc_tbl[0xE012] = "";  //my keyboard fires this one bevore any cursor-key, ignore it!

  desc_tbl[0xE01F] = img("lgui"); //left windows key
  desc_tbl[0xE027] = img("rgui"); //right windows key

  desc_tbl[0xE070] = img("ins");
  desc_tbl[0xE071] = img("del");
  desc_tbl[0xE06C] = img("home");
  desc_tbl[0xE069] = img("end");
  desc_tbl[0xE07D] = img("pgup");
  desc_tbl[0xE07A] = img("pgdn");
  desc_tbl[0xE06B] = img("left");
  desc_tbl[0xE075] = img("up");
  desc_tbl[0xE072] = img("down");
  desc_tbl[0xE074] = img("right");

  desc_tbl[0x05] = img("f1");
  desc_tbl[0x06] = img("f2");
  desc_tbl[0x04] = img("f3");
  desc_tbl[0x0C] = img("f4");
  desc_tbl[0x03] = img("f5");
  desc_tbl[0x0B] = img("f6");
  desc_tbl[0x83] = img("f7");
  desc_tbl[0x0A] = img("f8");
  desc_tbl[0x01] = img("f9");
  desc_tbl[0x09] = img("f10");
  desc_tbl[0x78] = img("f11");
  desc_tbl[0x07] = img("f12");

  desc_tbl[0x41] = ","; shift_tbl[0x41] = ";";
  desc_tbl[0x49] = "."; shift_tbl[0x49] = ":";
  desc_tbl[0x54] = "ü"; shift_tbl[0x54] = "Ü";
  desc_tbl[0x4C] = "ö"; shift_tbl[0x4C] = "Ö";
  desc_tbl[0x52] = "ä"; shift_tbl[0x52] = "Ä";
  desc_tbl[0x61] = "&lt;"; shift_tbl[0x61] = "&gt;"; altgr_tbl[0x61] = "|";
  desc_tbl[0x4A] = "-"; shift_tbl[0x4A] = "_";
  desc_tbl[0x5D] = "#"; shift_tbl[0x5D] = "'";
  desc_tbl[0x5B] = "+"; shift_tbl[0x5B] = "*"; altgr_tbl[0x5B] = "~";
  desc_tbl[0x55] = "´"; shift_tbl[0x55] = "`";
  desc_tbl[0x4E] = "ß"; shift_tbl[0x4E] = "?"; altgr_tbl[0x4E] = "";
  desc_tbl[0x0E] = "^"; shift_tbl[0x0E] = "°";

  desc_tbl[0x16] = "1"; shift_tbl[0x16] = "!";
  desc_tbl[0x1e] = "2"; shift_tbl[0x1e] = "\""; altgr_tbl[0x1e] = "²";
  desc_tbl[0x26] = "3"; shift_tbl[0x26] = "§"; altgr_tbl[0x26] = "³";
  desc_tbl[0x25] = "4"; shift_tbl[0x25] = "$";
  desc_tbl[0x2e] = "5"; shift_tbl[0x2e] = "%";
  desc_tbl[0x36] = "6"; shift_tbl[0x36] = "&amp;";
  desc_tbl[0x3d] = "7"; shift_tbl[0x3d] = "/"; altgr_tbl[0x3d] = "{";
  desc_tbl[0x3e] = "8"; shift_tbl[0x3e] = "("; altgr_tbl[0x3e] = "[";
  desc_tbl[0x46] = "9"; shift_tbl[0x46] = ")"; altgr_tbl[0x46] = "]";
  desc_tbl[0x45] = "0"; shift_tbl[0x45] = "="; altgr_tbl[0x45] = "}";

  desc_tbl[0x15] = "q"; shift_tbl[0x15] = "Q"; altgr_tbl[0x00] = "@";
  desc_tbl[0x1d] = "w"; shift_tbl[0x1d] = "W";
  desc_tbl[0x24] = "e"; shift_tbl[0x24] = "E"; altgr_tbl[0x24] = "?";
  desc_tbl[0x2d] = "r"; shift_tbl[0x2d] = "R";
  desc_tbl[0x2c] = "t"; shift_tbl[0x2c] = "T";
  desc_tbl[0x35] = "z"; shift_tbl[0x35] = "Z";
  desc_tbl[0x3c] = "u"; shift_tbl[0x3c] = "U";
  desc_tbl[0x43] = "i"; shift_tbl[0x43] = "I";
  desc_tbl[0x44] = "o"; shift_tbl[0x44] = "O";
  desc_tbl[0x4d] = "p"; shift_tbl[0x4d] = "P";

  desc_tbl[0x1c] = "a"; shift_tbl[0x1c] = "A";
  desc_tbl[0x1b] = "s"; shift_tbl[0x1b] = "S";
  desc_tbl[0x23] = "d"; shift_tbl[0x23] = "D";
  desc_tbl[0x2b] = "f"; shift_tbl[0x2b] = "F";
  desc_tbl[0x34] = "g"; shift_tbl[0x34] = "G";
  desc_tbl[0x33] = "h"; shift_tbl[0x33] = "H";
  desc_tbl[0x3b] = "j"; shift_tbl[0x3b] = "J";
  desc_tbl[0x42] = "k"; shift_tbl[0x42] = "K";
  desc_tbl[0x4b] = "l"; shift_tbl[0x4b] = "L";

  desc_tbl[0x1a] = "y"; shift_tbl[0x1a] = "Y";
  desc_tbl[0x22] = "x"; shift_tbl[0x22] = "X";
  desc_tbl[0x21] = "c"; shift_tbl[0x21] = "C";
  desc_tbl[0x2a] = "v"; shift_tbl[0x2a] = "V";
  desc_tbl[0x32] = "b"; shift_tbl[0x32] = "B";
  desc_tbl[0x31] = "n"; shift_tbl[0x31] = "N";
  desc_tbl[0x3a] = "m"; shift_tbl[0x3a] = "M"; altgr_tbl[0x3a] = "µ";
}


function reset_state()
{
  l_shift_state = false;
  r_shift_state = false;
  l_ctrl_state  = false;
  r_ctrl_state  = false;
  alt_state     = false;
  altgr_state   = false;
}


function hex(v)
{
  if(v > 0xFFFF) return hex_tbl[(v>>20)&15] + hex_tbl[(v>>16)&15] + hex_tbl[(v>>12)&15] + hex_tbl[(v>>8)&15] + hex_tbl[(v>>4)&15] + hex_tbl[v&15];
  else if(v > 0xFF) return hex_tbl[v>>12] + hex_tbl[(v>>8)&15] + hex_tbl[(v>>4)&15] + hex_tbl[v&15];
  else return hex_tbl[v>>4] + hex_tbl[v&15];
}


function on_key(code, pressed)
{
  if(code == l_shift_key) l_shift_state = pressed;
  else if(code == r_shift_key) r_shift_state = pressed;
  else if(code == l_ctrl_key) l_ctrl_state = pressed;
  else if(code == r_ctrl_key) r_ctrl_state = pressed;
  else if(code == alt_key) alt_state = pressed;
  else if(code == altgr_key) altgr_state = pressed;
  else if(pressed)
  {
    prefix = "";
    desc = null;

    if(l_ctrl_state) prefix += l_ctrl_prefix;
    if(r_ctrl_state) prefix += r_ctrl_prefix;
    if(alt_state) prefix += alt_prefix;

    if(altgr_state)
    {
      if(altgr_tbl[code] != null) desc = altgr_tbl[code];
      else
      {
        prefix += altgr_prefix;
      }
      if(l_shift_state) prefix += l_shift_prefix;
      if(r_shift_state) prefix += r_shift_prefix;
    }
    else if(l_shift_state || r_shift_state)
    {
      if(shift_tbl[code] != null) desc = shift_tbl[code];
      else
      {
        if(l_shift_state) prefix += l_shift_prefix;
        if(r_shift_state) prefix += r_shift_prefix;
      }
    }

    if(desc == null) desc = desc_tbl[code];

    //output
    if(desc == null) document.write(prefix + txt(hex(code), "color:black;background:lightgray"));  //for undefined keys
    else if(desc.length == 0);  //ignore those ones
    else if(prefix.length > 0) document.write(" " + prefix + desc + " ");  //for combos
    else document.write(desc);  //for ascii
  }
}


function get_key()
{
  do
  {
    if(keycode_pos < 1) return null;
    kc = keycode[--keycode_pos];
    switch(kc)
    {
      case 0xFC:  //strokey error (logged keycode was >= 0xFC)
        document.write(" " + txt(">FB", "color:yellow;background:red") + " ");
        break;

      case 0xFF:  //error during recording
        document.write(" " + txt("ERROR", "color:yellow;background:red") + " ");
        break;

      case 0xEE:  //keyboard ECHO
        document.write(" " + txt("ECHO", "color:gray;font-size:8px") + " ");
        break;

      case 0xFA:  //keyboard ACK
        document.write(" " + txt("ACK", "color:gray;font-size:8px") + " ");
        break;

      case 0xFE:  //keyboard NACK
        document.write(" " + txt("NACK", "color:gray;font-size:8px") + " ");
        break;

      case 0xFD:  //strokey restart
        document.write(div("RESET", "color:yellow;background:blue;text-align:center"));
        reset_state();
        break;

      case 0xFE:  //strokey awake
        document.write(div("AWAKE", "color:yellow;background:blue;text-align:center"));
        reset_state();
        break;

      default: return kc;
    }
  } while(true);
}


function decode()
{
  //reset global vars
  l_shift_key = -1;
  r_shift_key = -1;
  l_ctrl_key  = -1;
  r_ctrl_key  = -1;
  alt_key     = -1;
  altgr_key   = -1;

  reset_state();

  l_shift_prefix  = "";
  r_shift_prefix  = "";
  l_ctrl_prefix   = "";
  r_ctrl_prefix   = "";
  alt_prefix      = "";
  altgr_prefix    = "";

  desc_tbl  = new Array();
  shift_tbl = new Array();
  altgr_tbl = new Array();

  //load language specific keycode tables
  load_german_tbls();

  //process keycode array here
  keycode.reverse();
  keycode_pos = keycode.length;
  document.write("<h3>decode " + keycode.length + " bytes of keycode data...</h3>" +
    "<div style=\"color:blue;background:white;font-family:courier new;font-size:14px;font-weight:bolder;text-align:justify\">");

  while(keycode_pos > 0)
  {
    p = true;
    k = get_key();

    if(k == 0xF0) //key released?
    {
      p = false;
      k = get_key();
    }
    else if((k&0xFC) == 0xE0)
    {
      j = get_key();
      p = (j != 0xF0);
      for(n = k&3; true; n--)
      {
        if(j == null) {k = null; break;}

        if(!p)
        {
          if(j != 0xF0) {k = null; break;}
          j = get_key();
          if(j == null) {k = null; break;}
        }

        k = (k<<8) | j;
        if(n == 0) break;

        j = get_key();
      }
    }

    if(k != null) on_key(k, p);
    else document.write(txt("ERROR", "color:yellow;background:red"));
  }

  document.write("</div><br><br><center><input type=\"button\" value=\"Return\" onClick=\"history.go(-1)\"><br><br></center>");
}




function digit(line, pos)
{
  n = parseInt( line.substr(1+pos*2, 2), 16 );
  if(n<0 || n>255) return NaN;
  return n;
}




function decode_text(input)
{
  errors = 0;
  lines = 0;
  lines_acc = 0;
  keycode = new Array();

  document.write("<h3>processing input data...</h3><div style=\"color:red\">");

  if(input.length > 0) do
  {
    lines++;
    e = input.indexOf("\n");
    if(e<0) l = input+"\n"; else l = input.substr(0, e);
    input = input.substr(e+1);

    if(l.charAt(0) == '.')
    {
      lines_acc++;
      if((l.length%2) != 0 || l.length > 36)
      {
        document.write("line " + lines + ": illegal syntax<br>");
        errors++;
      }
      else
      {
        c = 0;
        p = l.length/2-2;
        for(i=0; i<p; i++)
        {
          d = digit(l, i);
          if(!isNaN(d)) c = (c + d)&255;
          else
          {
            document.write("line " + lines + ": illegal syntax<br>");
            errors++;
            c = NaN;
            break;
          }
        }

        if(!isNaN(c))
        {
          if(((-c)&255) != digit(l, p))
          {
            document.write("line " + lines + ": illegal checksum<br>");
            errors++;
          }
          else
          {
            for(i=0; i<p; i++) keycode.push( digit(l, i) );
          }
        }
      }
    }

  } while(e >= 0)

  document.write("</div><b>processed " + lines + " lines<br>accepted " + lines_acc + " lines with " + errors + " errors</b><br>");

  decode();
}
