using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class LightCurveData : Form
	{
		private IContainer components;

		internal TextBox txtNumber;

		internal TextBox txtName;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label label1;

		private Label label2;

		private Button cmdSearch;

		private ListBox lstResults;

		private ToolStripMenuItem withLightCurveDataToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private Label label3;

		private Label label4;

		public LightCurveData()
		{
			InitializeComponent();
		}

		private void cmdSearch_Click(object sender, EventArgs e)
		{
			Search(ReportErrors: true);
		}

		internal void Search(bool ReportErrors)
		{
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0083: Unknown result type (might be due to invalid IL or missing references)
			int num = 148;
			int num2 = 153;
			int num3 = 115;
			int num4 = 107;
			bool flag = true;
			bool flag2 = false;
			if (!File.Exists(Utilities.AppPath + "\\Downloaded Files\\LC_Summary_Pub.txt"))
			{
				MessageBox.Show("File of Asteroid Light Curve data is not present.\r\nGo to the Downloads page and download the file", "Light Curve Data not present", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			DateTime value = new DateTime(2011, 1, 27);
			if (File.GetLastWriteTime(Utilities.AppPath + "\\DownLoaded Files\\LC_Summary_Pub.txt").CompareTo(value) < 0)
			{
				MessageBox.Show("Your file of Asteroid Light Curve data uses an old format, and must be updated.\r\nGo to the Downloads page and download the file", "Invalid file of Light Curve Data", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			Utilities.ReSetCALL_URL();
			if (!int.TryParse(((Control)txtNumber).get_Text(), out var result))
			{
				result = 0;
			}
			string text = ((Control)txtName).get_Text().TrimStart(Array.Empty<char>()).PadRight(16)
				.Substring(0, 16)
				.Trim()
				.ToUpper();
			if ((result == 0) & (text == ""))
			{
				return;
			}
			flag = result > 0;
			lstResults.get_Items().Clear();
			if (flag)
			{
				lstResults.get_Items().Add((object)("Light Curve Database results for " + result));
			}
			else
			{
				lstResults.get_Items().Add((object)("Light Curve Database results for " + text));
			}
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Downloaded Files\\LC_Summary_Pub.txt");
			string text2;
			do
			{
				text2 = streamReader.ReadLine()!.PadRight(6);
			}
			while (!(text2.Substring(0, 6).ToUpper() == "NUMBER") && !streamReader.EndOfStream);
			if (text2.Contains("ALBEDO"))
			{
				num4 = text2.IndexOf("ALBEDO");
				num3 = text2.IndexOf("PERIOD");
			}
			else
			{
				num4 = text2.IndexOf("ALB.") - 1;
				num3 = text2.IndexOf("PERIOD") - 4;
			}
			num = text2.IndexOf("AMIN");
			num2 = text2.IndexOf("AMAX");
			do
			{
				text2 = streamReader.ReadLine();
				if (text2.Length < 50 || text2.Substring(0, 5) == "-----")
				{
					continue;
				}
				if (flag)
				{
					if (!int.TryParse(text2.Substring(0, 7), out var result2) || result2 > result)
					{
						break;
					}
					if (result2 == result)
					{
						flag2 = true;
						break;
					}
					continue;
				}
				if (text2.Substring(10, 16).Trim().ToUpper() == text)
				{
					flag2 = true;
					break;
				}
				if (text2.Substring(41, 12).Trim().ToUpper() == text)
				{
					flag2 = true;
					break;
				}
			}
			while (!streamReader.EndOfStream);
			if (!flag2)
			{
				lstResults.get_Items().Add((object)"No record in Asteroid Lightcurve Database (LCDB)");
				return;
			}
			if ("!@".Contains(text2.Substring(8, 1)))
			{
				lstResults.get_Items().Add((object)"     [data not yet validated]");
			}
			else if ("*".Contains(text2.Substring(8, 1)))
			{
				lstResults.get_Items().Add((object)"     [new entry]");
			}
			else
			{
				lstResults.get_Items().Add((object)"");
			}
			if (!double.TryParse(text2.Substring(num3, 12), out var result3))
			{
				result3 = 0.0;
			}
			if (!double.TryParse(text2.Substring(num2, 4), out var result4))
			{
				result4 = 0.0;
			}
			lstResults.get_Items().Add((object)("Asteroid                  " + text2.Substring(0, 30).Remove(8, 2).Trim()));
			lstResults.get_Items().Add((object)("Albedo                    " + text2.Substring(num4, 6)));
			lstResults.get_Items().Add((object)"");
			if (result3 > 0.0)
			{
				lstResults.get_Items().Add((object)("Rotation period           " + text2.Substring(num3, 12).Trim() + " hours"));
				lstResults.get_Items().Add((object)(".. rotation in 10 mins    " + string.Format("{0,2:f1} deg.", 360.0 / result3 / 6.0)));
				lstResults.get_Items().Add((object)(".. time to rotate 5 deg   " + string.Format("{0,2:f1} mins.", result3 * 60.0 / 72.0)));
			}
			else
			{
				lstResults.get_Items().Add((object)"Rotation period           not known");
			}
			lstResults.get_Items().Add((object)"");
			lstResults.get_Items().Add((object)("Maximum mag variation     " + text2.Substring(num2, 4)));
			lstResults.get_Items().Add((object)(" => maximum axis ratio    " + string.Format("{0,2:f2} : 1", Math.Pow(10.0, result4 / 2.5))));
			lstResults.get_Items().Add((object)"");
			lstResults.get_Items().Add((object)("Minimum mag variation     " + text2.Substring(num, 4).Replace("    ", "....")));
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstResults.get_Items().get_Count() >= 3)
			{
				Settings.Default.Save_AsteroidObservations = Output.SavePredictionText(CollectEvents(), lstResults.get_Items().get_Item(0).ToString(), Settings.Default.Save_AsteroidObservations);
			}
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstResults.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstResults.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Light curve data");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void LightCurveData_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(LightCurveData));
			txtNumber = new TextBox();
			txtName = new TextBox();
			menuStrip1 = new MenuStrip();
			withLightCurveDataToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label1 = new Label();
			label2 = new Label();
			cmdSearch = new Button();
			lstResults = new ListBox();
			label3 = new Label();
			label4 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)txtNumber).set_Location(new Point(93, 35));
			((Control)txtNumber).set_Name("txtNumber");
			((Control)txtNumber).set_Size(new Size(53, 20));
			((Control)txtNumber).set_TabIndex(0);
			((Control)txtName).set_Location(new Point(93, 61));
			((Control)txtName).set_Name("txtName");
			((Control)txtName).set_Size(new Size(142, 20));
			((Control)txtName).set_TabIndex(1);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withLightCurveDataToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(413, 24));
			((Control)menuStrip1).set_TabIndex(2);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withLightCurveDataToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withLightCurveDataToolStripMenuItem).set_Name("withLightCurveDataToolStripMenuItem");
			((ToolStripItem)withLightCurveDataToolStripMenuItem).set_Size(new Size(151, 20));
			((ToolStripItem)withLightCurveDataToolStripMenuItem).set_Text("with Light Curve Data...   ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(9, 39));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(83, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("Asteroid number");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(2, 63));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(92, 13));
			((Control)label2).set_TabIndex(4);
			((Control)label2).set_Text("OR asteroid name");
			((Control)cmdSearch).set_Location(new Point(334, 46));
			((Control)cmdSearch).set_Name("cmdSearch");
			((Control)cmdSearch).set_Size(new Size(54, 29));
			((Control)cmdSearch).set_TabIndex(5);
			((Control)cmdSearch).set_Text("Search");
			((ButtonBase)cmdSearch).set_UseVisualStyleBackColor(true);
			((Control)cmdSearch).add_Click((EventHandler)cmdSearch_Click);
			((Control)lstResults).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstResults).set_FormattingEnabled(true);
			lstResults.set_ItemHeight(14);
			((Control)lstResults).set_Location(new Point(8, 105));
			((Control)lstResults).set_Name("lstResults");
			((Control)lstResults).set_Size(new Size(399, 200));
			((Control)lstResults).set_TabIndex(6);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(169, 32));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(148, 26));
			((Control)label3).set_TabIndex(7);
			((Control)label3).set_Text("Search is by number (if given),\r\nor Name if no number");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(81, 90));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(252, 13));
			((Control)label4).set_TabIndex(8);
			((Control)label4).set_Text("Results from Asteroid Light Curve Data Base [LCDB]");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(413, 310));
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)lstResults);
			((Control)this).get_Controls().Add((Control)(object)cmdSearch);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtName);
			((Control)this).get_Controls().Add((Control)(object)txtNumber);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("LightCurveData");
			((Control)this).set_Text("Asteroid light curve data");
			((Form)this).add_Load((EventHandler)LightCurveData_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
