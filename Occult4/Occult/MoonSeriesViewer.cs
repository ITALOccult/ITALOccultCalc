using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class MoonSeriesViewer : Form
	{
		private readonly string AppPath;

		internal string HelpKeyword = "Lunar Polynomial";

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem editToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem contentsToolStripMenuItem;

		private ToolStripMenuItem indexToolStripMenuItem;

		private ToolStripMenuItem searchToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator5;

		private ToolStripMenuItem aboutToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ListBox lstSeries;

		public MoonSeriesViewer()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void MoonSeriesViewer_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
		}

		public void GetMoonSeries(double JD_at_0hrs)
		{
			double[] RA = new double[6];
			double[] Dec = new double[6];
			double[] Parallax = new double[6];
			float LongCorrection_arcsec1e = 0f;
			float LatCorrection_arcsec1e = 0f;
			int Source = 0;
			new StringBuilder();
			Utilities.MoonSeries_Retrieve(JD_at_0hrs, out RA, out Dec, out Parallax, out LongCorrection_arcsec1e, out LatCorrection_arcsec1e, out Source);
			lstSeries.get_Items().Clear();
			lstSeries.get_Items().Add((object)("Lunar polynomial for " + Utilities.Date_from_JD(JD_at_0hrs, 0)));
			lstSeries.get_Items().Add((object)"");
			lstSeries.get_Items().Add((object)string.Format("a0  {0,12:F7} {1,12:F7} {2,12:F8}", RA[0], Dec[0], Parallax[0]));
			lstSeries.get_Items().Add((object)string.Format("a1  {0,12:F7} {1,12:F7} {2,12:F8}", RA[1], Dec[1], Parallax[1]));
			lstSeries.get_Items().Add((object)string.Format("a2  {0,12:F0} {1,12:F0} {2,12:F0}", RA[2] * 10000000.0, Dec[2] * 10000000.0, Parallax[2] * 100000000.0));
			lstSeries.get_Items().Add((object)string.Format("a3  {0,12:F0} {1,12:F0} {2,12:F0}", RA[3] * 10000000.0, Dec[3] * 10000000.0, Parallax[3] * 100000000.0));
			lstSeries.get_Items().Add((object)string.Format("a4  {0,12:F0} {1,12:F0} {2,12:F0}", RA[4] * 10000000.0, Dec[4] * 10000000.0, Parallax[4] * 100000000.0));
			lstSeries.get_Items().Add((object)string.Format("a5  {0,12:F0} {1,12:F0}", RA[5] * 10000000.0, Dec[5] * 10000000.0));
			lstSeries.get_Items().Add((object)"");
			lstSeries.get_Items().Add((object)string.Format("Longitude correction ={0,5:F2}\"", (double)LongCorrection_arcsec1e / 10000.0));
			lstSeries.get_Items().Add((object)string.Format(" Latitude correction ={0,5:F2}\"", (double)LatCorrection_arcsec1e / 10000.0));
			lstSeries.get_Items().Add((object)"");
			if (Source > 100)
			{
				lstSeries.get_Items().Add((object)string.Format("JPL DE{0,1:f0}", Source));
			}
			else
			{
				lstSeries.get_Items().Add((object)"VSOP 87A");
			}
			HelpKeyword = "Lunar Polynomial";
		}

		internal void ShowElements(string[] Lines)
		{
			int upperBound = Lines.GetUpperBound(0);
			lstSeries.get_Items().Clear();
			for (int i = 0; i <= upperBound; i++)
			{
				lstSeries.get_Items().Add((object)Lines[i]);
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)HelpKeyword);
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private string CollectEvents()
		{
			if (lstSeries.get_Items().get_Count() < 0)
			{
				return "";
			}
			StringBuilder stringBuilder = new StringBuilder();
			for (int i = 0; i < lstSeries.get_Items().get_Count(); i++)
			{
				stringBuilder.AppendLine(lstSeries.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Expected O, but got Unknown
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Expected O, but got Unknown
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Expected O, but got Unknown
			//IL_0074: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Expected O, but got Unknown
			//IL_007f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Expected O, but got Unknown
			//IL_04f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_04fe: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(MoonSeriesViewer));
			menuStrip1 = new MenuStrip();
			editToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			contentsToolStripMenuItem = new ToolStripMenuItem();
			indexToolStripMenuItem = new ToolStripMenuItem();
			searchToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			aboutToolStripMenuItem = new ToolStripMenuItem();
			lstSeries = new ListBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)editToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(377, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)editToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem
			});
			((ToolStripItem)editToolStripMenuItem).set_Name("editToolStripMenuItem");
			((ToolStripItem)editToolStripMenuItem).set_Size(new Size(84, 20));
			((ToolStripItem)editToolStripMenuItem).set_Text("with Series...");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)componentResourceManager.GetObject("copyToolStripMenuItem.Image"));
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripDropDownItem)helpToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)contentsToolStripMenuItem,
				(ToolStripItem)indexToolStripMenuItem,
				(ToolStripItem)searchToolStripMenuItem,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)aboutToolStripMenuItem
			});
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)contentsToolStripMenuItem).set_Name("contentsToolStripMenuItem");
			((ToolStripItem)contentsToolStripMenuItem).set_Size(new Size(122, 22));
			((ToolStripItem)contentsToolStripMenuItem).set_Text("&Contents");
			((ToolStripItem)indexToolStripMenuItem).set_Name("indexToolStripMenuItem");
			((ToolStripItem)indexToolStripMenuItem).set_Size(new Size(122, 22));
			((ToolStripItem)indexToolStripMenuItem).set_Text("&Index");
			((ToolStripItem)searchToolStripMenuItem).set_Name("searchToolStripMenuItem");
			((ToolStripItem)searchToolStripMenuItem).set_Size(new Size(122, 22));
			((ToolStripItem)searchToolStripMenuItem).set_Text("&Search");
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(119, 6));
			((ToolStripItem)aboutToolStripMenuItem).set_Name("aboutToolStripMenuItem");
			((ToolStripItem)aboutToolStripMenuItem).set_Size(new Size(122, 22));
			((ToolStripItem)aboutToolStripMenuItem).set_Text("&About...");
			((Control)lstSeries).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstSeries).set_FormattingEnabled(true);
			lstSeries.set_ItemHeight(14);
			((Control)lstSeries).set_Location(new Point(0, 27));
			((Control)lstSeries).set_Name("lstSeries");
			((Control)lstSeries).set_Size(new Size(377, 298));
			((Control)lstSeries).set_TabIndex(1);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(377, 326));
			((Control)this).get_Controls().Add((Control)(object)lstSeries);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationMoonSeriesViewer", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationMoonSeriesViewer);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("MoonSeriesViewer");
			((Control)this).set_Text("Lunar ephemeris - series viewer");
			((Form)this).add_Load((EventHandler)MoonSeriesViewer_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
