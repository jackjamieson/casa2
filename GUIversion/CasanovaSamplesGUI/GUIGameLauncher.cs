using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media.Imaging;

namespace CasanovaSamplesGUI
{
    public class GUIGameLauncher
    {
        public string Name { get; set; }
        public string Description {get;set;}
        public BitmapImage Picture { get; set; }

        public Action Launch {get;set;}

        public GUIGameLauncher(string name, string description, string picture, Action launch)
        {
            Name = name;
            Description = description;
            Picture = new BitmapImage(new Uri(Path.GetFullPath(picture)));
            Launch = launch;
        }
    }
}
