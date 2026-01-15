using System;
using System.ComponentModel;
using System.Drawing;
using System.Globalization;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class Calendar : Form
	{
		private readonly string AppPath;

		private static string Prediction;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withCalendarToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private TextBox txtCalendar;

		private NumericUpDown updnYear;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private Label label1;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public Calendar()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			updnYear.set_Value((decimal)DateTime.Now.Year);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void updnYear_ValueChanged(object sender, EventArgs e)
		{
			MakeCalendar();
		}

		private void MakeCalendar()
		{
			int[] array = new int[13]
			{
				0, 31, 28, 31, 30, 31, 30, 31, 31, 30,
				31, 30, 31
			};
			string[] array2 = new string[13];
			string[] array3 = new string[13];
			for (int i = 1; i <= 12; i++)
			{
				string monthName = CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName(i);
				int length = monthName.Length;
				array2[i] = monthName.PadLeft(11 + length / 2).PadRight(26 - 5 * Convert.ToInt16(i % 3 == 0));
				array3[i] = "".PadLeft(126);
			}
			int num = (int)updnYear.get_Value();
			for (int i = 1; i <= 12; i++)
			{
				int num2 = (int)(Utilities.JD_from_Date(num, i, 1.0) + 1.5) % 7;
				int num3 = array[i];
				if ((i == 2) & (num % 4 == 0))
				{
					if (num % 4 == 0)
					{
						num3 = 29;
					}
					if (num % 100 == 0)
					{
						num3 = 28;
					}
					if (num % 400 == 0)
					{
						num3 = 29;
					}
				}
				array3[i] = "".PadLeft(3 * num2) + "  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31                        ".Substring(0, 3 * num3);
				array3[i] = array3[i].PadRight(126);
			}
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendLine("".PadLeft(22) + "C A L E N D A R   F O R  " + num + "\r\n");
			for (int i = 1; i <= 12; i += 3)
			{
				stringBuilder.AppendLine(array2[i] + array2[i + 1] + array2[i + 2]);
				stringBuilder.AppendLine(" Su Mo Tu We Th Fr Sa" + "".PadLeft(5) + " Su Mo Tu We Th Fr Sa" + "".PadLeft(5) + " Su Mo Tu We Th Fr Sa");
				for (int j = 0; j < 6; j++)
				{
					stringBuilder.Append(array3[i].Substring(21 * j, 21) + "".PadLeft(5));
					stringBuilder.Append(array3[i + 1].Substring(21 * j, 21) + "".PadLeft(5));
					stringBuilder.AppendLine(array3[i + 2].Substring(21 * j, 21));
				}
			}
			Utilities.DateOfEasterSunday(num, out var Month, out var Day);
			stringBuilder.Append("\r\nEaster Sunday: " + num + " " + Utilities.ShortMonths[Month] + Day.ToString().PadLeft(2, '0').PadLeft(3));
			Prediction = stringBuilder.ToString();
			((Control)txtCalendar).set_Text(Prediction);
			((TextBoxBase)txtCalendar).set_SelectionStart(0);
			((TextBoxBase)txtCalendar).Select(0, 0);
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(Prediction);
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(Prediction);
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(Prediction);
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(Prediction, "Calendar for " + updnYear.get_Value(), Settings.Default.Save_EphemerisData);
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 10);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Calendar");
		}

		private void Calendar_Load(object sender, EventArgs e)
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
			//IL_0652: Unknown result type (might be due to invalid IL or missing references)
			//IL_065c: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Calendar));
			menuStrip1 = new MenuStrip();
			withCalendarToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			txtCalendar = new TextBox();
			updnYear = new NumericUpDown();
			label1 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withCalendarToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(565, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withCalendarToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withCalendarToolStripMenuItem).set_Name("withCalendarToolStripMenuItem");
			((ToolStripItem)withCalendarToolStripMenuItem).set_Size(new Size(110, 20));
			((ToolStripItem)withCalendarToolStripMenuItem).set_Text("with Calendar...   ");
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
			((Control)txtCalendar).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCalendar).set_Location(new Point(15, 64));
			((TextBoxBase)txtCalendar).set_Multiline(true);
			((Control)txtCalendar).set_Name("txtCalendar");
			((Control)txtCalendar).set_Size(new Size(536, 514));
			((Control)txtCalendar).set_TabIndex(3);
			((Control)updnYear).set_Location(new Point(277, 38));
			updnYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(55, 20));
			((Control)updnYear).set_TabIndex(2);
			updnYear.add_ValueChanged((EventHandler)updnYear_ValueChanged);
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(233, 41));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Year");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(565, 587));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)txtCalendar);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemCalendar", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemCalendar);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("Calendar");
			((Control)this).set_Text("Calendar");
			((Form)this).add_Load((EventHandler)Calendar_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)updnYear).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
