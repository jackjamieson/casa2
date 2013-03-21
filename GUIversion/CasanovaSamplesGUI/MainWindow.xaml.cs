using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace CasanovaSamplesGUI
{
  /// <summary>
  /// Interaction logic for MainWindow.xaml
  /// </summary>
  /// 

  public partial class MainWindow : Window
  {
    public MainWindow()
    {
      DataContext = WindowsContent.Games;
      InitializeComponent();
    }

    private void Cancel_Click(object sender, RoutedEventArgs e)
    {
      Application.Current.Shutdown();
    }

    private void StartGame_Click(object sender, RoutedEventArgs e)
    {
      (Games.SelectedItem as GUIGameLauncher).Launch();
    }

    private void Games_MouseDoubleClick_1(object sender, MouseButtonEventArgs e)
    {
      //(Games.SelectedItem as GUIGameLauncher).Launch();
    }

  }
}
