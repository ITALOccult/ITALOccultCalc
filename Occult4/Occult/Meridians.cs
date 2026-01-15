using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class Meridians : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private readonly string AppPath;

		private static int Planet;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ListBox lstMeridians;

		private Panel panel2;

		private RadioButton optSaturn;

		private RadioButton optJupiter;

		private RadioButton optMars;

		private Label label1;

		private NumericUpDown updnYear;

		private Button cmdCompute;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public Meridians()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void Meridians_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			updnYear.set_Value((decimal)DateTime.Now.Year);
		}

		private void Meridians_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 500) | (((Control)this).get_Height() < 500)))
			{
				((Control)lstMeridians).set_Width(((Control)this).get_Width() - 30);
				((Control)lstMeridians).set_Height(((Control)this).get_Height() - 139);
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(), "Central meridian of " + Utilities.Planets[Planet] + " " + updnYear.get_Value(), Settings.Default.Save_EphemerisData);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstMeridians.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstMeridians.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			double[,] array = new double[32, 13];
			double[,] array2 = new double[32, 13];
			double day = 0.0;
			int Year = 0;
			int Month = 0;
			lstMeridians.get_Items().Clear();
			for (int i = 1; i <= 31; i++)
			{
				for (int j = 1; j <= 12; j++)
				{
					array[i, j] = (array2[i, j] = -1.0);
				}
			}
			string text = ", System I";
			if (optJupiter.get_Checked())
			{
				Planet = 5;
			}
			else if (optSaturn.get_Checked())
			{
				Planet = 6;
			}
			else
			{
				Planet = 4;
				text = "";
			}
			double num = Utilities.JD_from_Date((int)updnYear.get_Value(), 1, 1.0);
			double num2 = Utilities.JD_from_Date((int)updnYear.get_Value() + 1, 1, 1.0) - num - 1.0;
			double num3 = Utilities.delta_T(num + 183.0) / 86400.0;
			for (double num4 = 0.0; num4 <= num2; num4 += 1.0)
			{
				double num5 = num + num4;
				Utilities.Date_from_JD(num5, out Year, out Month, out day);
				Utilities.Meridians(num5 + num3, Planet, out array[(int)day, Month], out array2[(int)day, Month], out var _, out var _);
			}
			lstMeridians.get_Items().Add((object)("".PadRight(20) + "Central Meridian of " + Utilities.Planets[Planet] + text + ", " + updnYear.get_Value()));
			lstMeridians.get_Items().Add((object)"");
			lstMeridians.get_Items().Add((object)"Date  Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec");
			for (int k = 1; k <= 31; k++)
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.AppendFormat(" {0,2:f0} ", k);
				for (int l = 1; l <= 12; l++)
				{
					if (array[k, l] >= 0.0)
					{
						stringBuilder.AppendFormat("{0,6:F1}", array[k, l]);
					}
					else
					{
						stringBuilder.Append("      ");
					}
				}
				lstMeridians.get_Items().Add((object)stringBuilder.ToString());
			}
			MeridianTable(Planet);
			lstMeridians.get_Items().Add((object)"");
			lstMeridians.get_Items().Add((object)"");
			if (Planet != 5)
			{
				return;
			}
			lstMeridians.get_Items().Add((object)("".PadRight(20) + "Central Meridian of " + Utilities.Planets[Planet] + text + "I, " + updnYear.get_Value()));
			lstMeridians.get_Items().Add((object)"");
			lstMeridians.get_Items().Add((object)"Date  Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec");
			for (int m = 1; m <= 31; m++)
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.AppendFormat(" {0,2:f0} ", m);
				for (int n = 1; n <= 12; n++)
				{
					if (array2[m, n] >= 0.0)
					{
						stringBuilder.AppendFormat("{0,6:F1}", array2[m, n]);
					}
					else
					{
						stringBuilder.Append("      ");
					}
				}
				lstMeridians.get_Items().Add((object)stringBuilder.ToString());
			}
			MeridianTable(7);
			lstMeridians.get_Items().Add((object)"");
		}

		private void MeridianTable(int PlanetNo)
		{
			double num = 0.0;
			lstMeridians.get_Items().Add((object)"");
			lstMeridians.get_Items().Add((object)("".PadRight(21) + "Motion of the Central Meridian"));
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("   ");
			for (int i = 0; i < 12; i++)
			{
				stringBuilder.AppendFormat("{0,5}h", i);
			}
			lstMeridians.get_Items().Add((object)stringBuilder.ToString());
			stringBuilder = new StringBuilder();
			stringBuilder.Append("  m");
			for (int j = 0; j < 12; j++)
			{
				stringBuilder.Append("     o");
			}
			lstMeridians.get_Items().Add((object)stringBuilder.ToString());
			switch (PlanetNo)
			{
			case 4:
				num = 350.892;
				break;
			case 5:
				num = 877.9;
				break;
			case 6:
				num = 844.3;
				break;
			case 7:
				num = 870.27;
				break;
			}
			for (int k = 0; k <= 60; k += 10)
			{
				stringBuilder = new StringBuilder();
				stringBuilder.AppendFormat("{0,3} ", k);
				for (int l = 0; l < 12; l++)
				{
					double num2 = ((double)l / 24.0 + (double)k / 1440.0) * num % 360.0;
					if (num2 < 0.0)
					{
						num2 += 360.0;
					}
					stringBuilder.AppendFormat("{0,6:F1}", num2);
				}
				lstMeridians.get_Items().Add((object)stringBuilder.ToString());
			}
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 10);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Central Meridians - Mars, Jupiter & Saturn");
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
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0094: Expected O, but got Unknown
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009f: Expected O, but got Unknown
			//IL_00a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00aa: Expected O, but got Unknown
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Expected O, but got Unknown
			//IL_00b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c0: Expected O, but got Unknown
			//IL_0992: Unknown result type (might be due to invalid IL or missing references)
			//IL_099c: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Meridians));
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstMeridians = new ListBox();
			panel2 = new Panel();
			optSaturn = new RadioButton();
			optJupiter = new RadioButton();
			optMars = new RadioButton();
			label1 = new Label();
			updnYear = new NumericUpDown();
			cmdCompute = new Button();
			((Control)menuStrip1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(586, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(117, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...   ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lstMeridians).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstMeridians).set_FormattingEnabled(true);
			lstMeridians.set_ItemHeight(14);
			((Control)lstMeridians).set_Location(new Point(11, 98));
			((Control)lstMeridians).set_Name("lstMeridians");
			((Control)lstMeridians).set_Size(new Size(564, 508));
			((Control)lstMeridians).set_TabIndex(1);
			((Control)panel2).set_Anchor((AnchorStyles)1);
			((Control)panel2).get_Controls().Add((Control)(object)optSaturn);
			((Control)panel2).get_Controls().Add((Control)(object)optJupiter);
			((Control)panel2).get_Controls().Add((Control)(object)optMars);
			((Control)panel2).set_Location(new Point(12, 36));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(167, 56));
			((Control)panel2).set_TabIndex(2);
			((Control)optSaturn).set_AutoSize(true);
			((Control)optSaturn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optSaturn).set_Location(new Point(3, 37));
			((Control)optSaturn).set_Name("optSaturn");
			((Control)optSaturn).set_Size(new Size(122, 17));
			((Control)optSaturn).set_TabIndex(1);
			((Control)optSaturn).set_Text("Saturn (System I)");
			((ButtonBase)optSaturn).set_UseVisualStyleBackColor(true);
			((Control)optJupiter).set_AutoSize(true);
			((Control)optJupiter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optJupiter).set_Location(new Point(3, 20));
			((Control)optJupiter).set_Name("optJupiter");
			((Control)optJupiter).set_Size(new Size(152, 17));
			((Control)optJupiter).set_TabIndex(0);
			((Control)optJupiter).set_Text("Jupiter (Systems I && II)");
			((ButtonBase)optJupiter).set_UseVisualStyleBackColor(true);
			((Control)optMars).set_AutoSize(true);
			optMars.set_Checked(true);
			((Control)optMars).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optMars).set_Location(new Point(3, 3));
			((Control)optMars).set_Name("optMars");
			((Control)optMars).set_Size(new Size(52, 17));
			((Control)optMars).set_TabIndex(2);
			optMars.set_TabStop(true);
			((Control)optMars).set_Text("Mars");
			((ButtonBase)optMars).set_UseVisualStyleBackColor(true);
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(232, 58));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("Year");
			((Control)updnYear).set_Anchor((AnchorStyles)1);
			((Control)updnYear).set_Location(new Point(267, 56));
			updnYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(55, 20));
			((Control)updnYear).set_TabIndex(4);
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((Control)cmdCompute).set_Anchor((AnchorStyles)1);
			((Control)cmdCompute).set_Location(new Point(461, 52));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(67, 24));
			((Control)cmdCompute).set_TabIndex(10);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(586, 613));
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)lstMeridians);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemMeridians", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemMeridians);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("Meridians");
			((Control)this).set_Text("Central meridians of Mars, Jupiter and Saturn");
			((Form)this).add_Load((EventHandler)Meridians_Load);
			((Control)this).add_Resize((EventHandler)Meridians_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((ISupportInitialize)updnYear).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
