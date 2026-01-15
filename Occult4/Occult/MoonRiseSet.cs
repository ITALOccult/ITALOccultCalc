using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class MoonRiseSet : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private static double pLongitude;

		private static double pLatitude;

		private static double pTimeZone;

		private IContainer components;

		private GroupBox groupBox1;

		private Label label5;

		private Label label4;

		private Label label6;

		private MenuStrip menuStrip1;

		private ListBox lstMoonRise;

		private Label label1;

		private NumericUpDown updnYear;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Button cmdCompute;

		private NumericUpDown updnLatitude;

		private NumericUpDown updnLongitude;

		private NumericUpDown updnTimeZone;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public MoonRiseSet()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void MoonRiseSet_Load(object sender, EventArgs e)
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
			if (!decimal.TryParse(Settings.Default.Site_Longitude_dd_d, out var result))
			{
				result = default(decimal);
			}
			updnLongitude.set_Value(result);
			if (!decimal.TryParse(Settings.Default.Site_Latitude_dd_d, out result))
			{
				result = default(decimal);
			}
			updnLatitude.set_Value(result);
			if (!decimal.TryParse(Settings.Default.Site_TimeZone_Hrs, out result))
			{
				result = default(decimal);
			}
			updnTimeZone.set_Value(result);
		}

		private void MoonRiseSet_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Height() < 200) | (((Control)this).get_Width() < 100)))
			{
				((Control)lstMoonRise).set_Height(((Control)this).get_Height() - 134);
				((Control)lstMoonRise).set_Width(((Control)this).get_Width() - 38);
			}
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
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
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(), "MoonRise and MoonSet in " + updnYear.get_Value(), Settings.Default.Save_EphemerisData);
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstMoonRise.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstMoonRise.get_Items().get_Item(i).ToString());
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
			int RiseDay = 0;
			int SetDay = 0;
			for (int i = 1; i <= 31; i++)
			{
				for (int j = 1; j <= 12; j++)
				{
					array[i, j] = (array2[i, j] = -9.0);
				}
			}
			int num = (int)updnYear.get_Value();
			double num2 = Utilities.JD_from_Date((int)updnYear.get_Value(), 1, 1.0);
			int num3 = (int)(Utilities.JD_from_Date((int)updnYear.get_Value(), 12, 31.0) - num2);
			double num4 = Utilities.delta_T(num, 7, 1.0) / 86400.0;
			pLongitude = (double)updnLongitude.get_Value();
			pLatitude = (double)updnLatitude.get_Value();
			pTimeZone = (double)updnTimeZone.get_Value();
			lstMoonRise.get_Items().Clear();
			for (int k = 0; k <= num3; k++)
			{
				double num5 = num2 + (double)k;
				Utilities.Date_from_JD(num5, out Year, out Month, out day);
				double siderialTime = Utilities.SiderealTime_deg(num5, Apparent: false);
				Utilities.QuickMoon(num5 + num4, out var RA, out var Dec, out var Parallax, out var MoonLongitude, out var MoonLatitude);
				RiseSetMoonCalc(RA, Dec, siderialTime, 0.0, out var trise, out var tset, out var RiseDay2, out var SetDay2, firstTimeFlag: true, riseFlag: true);
				int num6;
				int num7;
				int num8;
				int num9;
				if (trise != tset)
				{
					Utilities.QuickMoon(num5 + num4 + trise / 24.0, out RA, out Dec, out Parallax, out MoonLongitude, out MoonLatitude);
					RiseSetMoonCalc(RA, Dec, siderialTime, 1.037 * trise, out var trise2, out var tset2, out RiseDay, out SetDay2, firstTimeFlag: false, riseFlag: true);
					Utilities.QuickMoon(num5 + num4 + tset / 24.0, out RA, out Dec, out Parallax, out MoonLongitude, out MoonLatitude);
					RiseSetMoonCalc(RA, Dec, siderialTime, 1.037 * tset, out trise, out tset2, out RiseDay2, out SetDay, firstTimeFlag: false, riseFlag: false);
					num6 = (int)trise2;
					num7 = (int)(60.0 * (trise2 - (double)num6));
					num8 = (int)tset2;
					num9 = (int)(60.0 * (tset2 - (double)num8));
				}
				else
				{
					num6 = ((trise != 0.0) ? (-2) : (-1));
					num8 = num6;
					num7 = (num9 = 0);
				}
				Utilities.Date_from_JD(num5 + (double)RiseDay, out Year, out Month, out day);
				if (Year == num)
				{
					array[(int)day, Month] = (double)num6 + (double)num7 / 100.0;
				}
				Utilities.Date_from_JD(num5 + (double)SetDay, out Year, out Month, out day);
				if (Year == num)
				{
					array2[(int)day, Month] = (double)num8 + (double)num9 / 100.0;
				}
			}
			lstMoonRise.get_Items().Add((object)("".PadRight(25) + "Local Time of MOONRISE " + num));
			lstMoonRise.get_Items().Add((object)"");
			lstMoonRise.get_Items().Add((object)"Date  Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec");
			for (int i = 1; i <= 31; i++)
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.AppendFormat(" {0,2:F0} ", i);
				for (int l = 1; l < 13; l++)
				{
					if (array[i, l] == -9.0)
					{
						stringBuilder.Append(" .....");
					}
					else if (array[i, l] == -2.0)
					{
						stringBuilder.Append(" -----");
					}
					else if (array[i, l] == -1.0)
					{
						stringBuilder.Append(" *****");
					}
					else
					{
						stringBuilder.AppendFormat(" {0,5:F2}", array[i, l]);
					}
				}
				lstMoonRise.get_Items().Add((object)stringBuilder.ToString());
				if (i % 5 == 0)
				{
					lstMoonRise.get_Items().Add((object)"");
				}
			}
			lstMoonRise.get_Items().Add((object)"");
			lstMoonRise.get_Items().Add((object)"    .....  No phenomena occurs on this date");
			lstMoonRise.get_Items().Add((object)"    -----  Moon continuously below the horizon");
			lstMoonRise.get_Items().Add((object)"    *****  Moon continuously above the horizon");
			lstMoonRise.get_Items().Add((object)"");
			lstMoonRise.get_Items().Add((object)"");
			lstMoonRise.get_Items().Add((object)("".PadRight(25) + "Local Time of MOONSET " + num));
			lstMoonRise.get_Items().Add((object)"");
			lstMoonRise.get_Items().Add((object)"Date  Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec");
			for (int i = 1; i <= 31; i++)
			{
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.AppendFormat(" {0,2:F0} ", i);
				for (int m = 1; m < 13; m++)
				{
					if (array2[i, m] == -9.0)
					{
						stringBuilder.Append(" .....");
					}
					else if (array2[i, m] == -2.0)
					{
						stringBuilder.Append(" -----");
					}
					else if (array2[i, m] == -1.0)
					{
						stringBuilder.Append(" *****");
					}
					else
					{
						stringBuilder.AppendFormat(" {0,5:F2}", array2[i, m]);
					}
				}
				lstMoonRise.get_Items().Add((object)stringBuilder.ToString());
				if (i % 5 == 0)
				{
					lstMoonRise.get_Items().Add((object)"");
				}
			}
			lstMoonRise.get_Items().Add((object)"");
			lstMoonRise.get_Items().Add((object)"    .....  No phenomena occurs on this date");
			lstMoonRise.get_Items().Add((object)"    -----  Moon continuously below the horizon");
			lstMoonRise.get_Items().Add((object)"    *****  Moon continuously above the horizon");
		}

		private void RiseSetMoonCalc(double RAMoon, double DecMoon, double SiderialTime, double PartDay, out double trise, out double tset, out int RiseDay, out int SetDay, bool firstTimeFlag, bool riseFlag)
		{
			RiseDay = (SetDay = 0);
			double num = (0.0 - Math.Tan(pLatitude / (180.0 / Math.PI))) * Math.Tan(DecMoon) + 0.0023 / Math.Cos(DecMoon) / Math.Cos(pLatitude / (180.0 / Math.PI));
			double num2;
			for (num2 = (0.0 - (SiderialTime + pLongitude + (0.0 - RAMoon + 0.0175 * PartDay / 24.0) * (180.0 / Math.PI))) / 15.0; num2 > 12.0; num2 -= 24.0)
			{
			}
			for (; num2 < -12.0; num2 += 24.0)
			{
			}
			if (num >= 1.0)
			{
				trise = 12.0;
				tset = 12.0;
				return;
			}
			if (num <= -1.0)
			{
				trise = 0.0;
				tset = 0.0;
				return;
			}
			double num3 = Math.Atan(Math.Sqrt(1.0 - num * num) / num) * 3.81971864;
			if (num < 0.0)
			{
				num3 += 12.0;
			}
			trise = num2 - num3;
			tset = num2 + num3;
			if (firstTimeFlag)
			{
				return;
			}
			if (riseFlag)
			{
				RiseDay = 0;
				if (PartDay - trise > 20.0)
				{
					trise += 24.0;
				}
				if (PartDay - trise < -20.0)
				{
					trise -= 24.0;
				}
				if (PartDay < 0.0)
				{
					RiseDay = -1;
					trise += 24.0;
				}
				else if (PartDay > 24.0)
				{
					RiseDay = 1;
					trise -= 24.0;
				}
				trise += pTimeZone;
				while (trise < 0.0)
				{
					RiseDay--;
					trise += 24.0;
				}
				while (trise > 24.0)
				{
					RiseDay++;
					trise -= 24.0;
				}
			}
			else
			{
				SetDay = 0;
				if (PartDay - tset > 20.0)
				{
					tset += 24.0;
				}
				if (PartDay - tset < -20.0)
				{
					tset -= 24.0;
				}
				if (PartDay < 0.0)
				{
					SetDay = -1;
					tset += 24.0;
				}
				else if (PartDay > 1.0)
				{
					SetDay = 1;
					tset -= 24.0;
				}
				tset += pTimeZone;
				while (tset < 0.0)
				{
					SetDay--;
					tset += 24.0;
				}
				while (tset > 24.0)
				{
					SetDay++;
					tset -= 24.0;
				}
			}
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 10);
		}

		private void updnLongitude_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLongitude).Select(0, 10);
		}

		private void updnLatitude_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnLatitude).Select(0, 10);
		}

		private void updnTimeZone_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnTimeZone).Select(0, 10);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Moon Rise Set times");
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
			//IL_00c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cb: Expected O, but got Unknown
			//IL_00cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d6: Expected O, but got Unknown
			//IL_00d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Expected O, but got Unknown
			//IL_0bc5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bcf: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(MoonRiseSet));
			groupBox1 = new GroupBox();
			updnTimeZone = new NumericUpDown();
			updnLatitude = new NumericUpDown();
			updnLongitude = new NumericUpDown();
			label5 = new Label();
			label4 = new Label();
			label6 = new Label();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lstMoonRise = new ListBox();
			label1 = new Label();
			updnYear = new NumericUpDown();
			cmdCompute = new Button();
			((Control)groupBox1).SuspendLayout();
			((ISupportInitialize)updnTimeZone).BeginInit();
			((ISupportInitialize)updnLatitude).BeginInit();
			((ISupportInitialize)updnLongitude).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)this).SuspendLayout();
			((Control)groupBox1).set_Anchor((AnchorStyles)1);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnTimeZone);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnLatitude);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnLongitude);
			((Control)groupBox1).get_Controls().Add((Control)(object)label5);
			((Control)groupBox1).get_Controls().Add((Control)(object)label4);
			((Control)groupBox1).get_Controls().Add((Control)(object)label6);
			((Control)groupBox1).set_Location(new Point(185, 28));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(225, 52));
			((Control)groupBox1).set_TabIndex(2);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Location");
			updnTimeZone.set_DecimalPlaces(1);
			updnTimeZone.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnTimeZone).set_Location(new Point(144, 29));
			updnTimeZone.set_Maximum(new decimal(new int[4] { 14, 0, 0, 0 }));
			updnTimeZone.set_Minimum(new decimal(new int[4] { 12, 0, 0, -2147483648 }));
			((Control)updnTimeZone).set_Name("updnTimeZone");
			((Control)updnTimeZone).set_Size(new Size(48, 20));
			((Control)updnTimeZone).set_TabIndex(5);
			((Control)updnTimeZone).add_Enter((EventHandler)updnTimeZone_Enter);
			updnLatitude.set_DecimalPlaces(1);
			((Control)updnLatitude).set_Location(new Point(79, 29));
			updnLatitude.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatitude.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatitude).set_Name("updnLatitude");
			((Control)updnLatitude).set_Size(new Size(51, 20));
			((Control)updnLatitude).set_TabIndex(3);
			((Control)updnLatitude).add_Enter((EventHandler)updnLatitude_Enter);
			updnLongitude.set_DecimalPlaces(1);
			((Control)updnLongitude).set_Location(new Point(9, 30));
			updnLongitude.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongitude.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongitude).set_Name("updnLongitude");
			((Control)updnLongitude).set_Size(new Size(54, 20));
			((Control)updnLongitude).set_TabIndex(1);
			((Control)updnLongitude).add_Enter((EventHandler)updnLongitude_Enter);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(76, 14));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(45, 13));
			((Control)label5).set_TabIndex(2);
			((Control)label5).set_Text("Latitude");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(6, 14));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(54, 13));
			((Control)label4).set_TabIndex(0);
			((Control)label4).set_Text("Longitude");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(137, 14));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(81, 13));
			((Control)label6).set_TabIndex(4);
			((Control)label6).set_Text("Time Zone [hrs]");
			label6.set_TextAlign(ContentAlignment.MiddleCenter);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(598, 24));
			((Control)menuStrip1).set_TabIndex(5);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(111, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction... ");
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
			((Control)lstMoonRise).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstMoonRise).set_FormattingEnabled(true);
			lstMoonRise.set_ItemHeight(14);
			((Control)lstMoonRise).set_Location(new Point(14, 92));
			((Control)lstMoonRise).set_Name("lstMoonRise");
			((Control)lstMoonRise).set_Size(new Size(570, 508));
			((Control)lstMoonRise).set_TabIndex(4);
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(18, 48));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Year");
			((Control)updnYear).set_Anchor((AnchorStyles)1);
			((Control)updnYear).set_Location(new Point(62, 44));
			updnYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(55, 20));
			((Control)updnYear).set_TabIndex(1);
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((Control)cmdCompute).set_Anchor((AnchorStyles)1);
			((Control)cmdCompute).set_Location(new Point(473, 42));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(67, 24));
			((Control)cmdCompute).set_TabIndex(3);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(598, 578));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)lstMoonRise);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemMoonRiseSet", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemMoonRiseSet);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(600, 594));
			((Control)this).set_Name("MoonRiseSet");
			((Control)this).set_Text("Local times of MoonRise and MoonSet");
			((Form)this).add_Load((EventHandler)MoonRiseSet_Load);
			((Control)this).add_Resize((EventHandler)MoonRiseSet_Resize);
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((ISupportInitialize)updnTimeZone).EndInit();
			((ISupportInitialize)updnLatitude).EndInit();
			((ISupportInitialize)updnLongitude).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)updnYear).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
