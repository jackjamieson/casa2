using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace CasanovaSamplesGUI
{
    public static class WindowsContent
    {
        public static IEnumerable<GUIGameLauncher> Games
        {
            get
            {
                #region Pong
                var pong = new GUIGameLauncher(
                    "Pong",
                    "This is a playable asteroid game created by Sean Safari, Jack Jamieson, and Glen Oakely.",
                    @"Pictures\pong.png",
                    Pong.Run);
                #endregion


                return new[]
                        {
                            pong
                        };
            }
        }
    }

}
