using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Ephemerides;
using Occult.File_Actions;
using Occult.Properties;

namespace Occult
{
	public class Comets : Form
	{
		private readonly string AppPath;

		private static ArrayList Elements;

		private List<CometLastObs> CometList = new List<CometLastObs>();

		private CometLastObs CometLast;

		private static double TDay;

		private static double TJD;

		private static double q;

		private static double ecc;

		private static double Perihelion;

		private static double Node;

		private static double Incl;

		private static double H0;

		private static double RCoeff;

		private static int TYear;

		private static int TMonth;

		private static string CometID;

		private static string ElementsSource;

		private static string LastObserved = "";

		private static string WhenLastObserved = "";

		private const double Radian = 180.0 / Math.PI;

		private bool CancelFlag;

		private IContainer components;

		private MenuStrip menuStrip1;

		private ComboBox cmbComet;

		private NumericUpDown updnYear;

		private NumericUpDown updnMonth;

		private NumericUpDown updnDay;

		private NumericUpDown updnHour;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private NumericUpDown updnDuration;

		private Label label5;

		private Label label6;

		private NumericUpDown updnInterval;

		private Label label7;

		private ListBox lstComet;

		private Button cmdCompute;

		private Button cmdCancel;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label label8;

		private Button cmdDownloadComet;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private CheckBox chkShowElements;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Label lblCometFileDate;

		public Comets()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void Comets_Load(object sender, EventArgs e)
		{
			//IL_0134: Unknown result type (might be due to invalid IL or missing references)
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			updnYear.set_Value((decimal)DateTime.Now.ToUniversalTime().Year);
			updnMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().Month);
			updnDay.set_Value((decimal)DateTime.Now.ToUniversalTime().Day);
			updnHour.set_Value((decimal)DateTime.Now.ToUniversalTime().Hour);
			if (!File.Exists(AppPath + "\\Downloaded Files\\Comet.dat"))
			{
				if (!Utilities.InternetIsAvailable())
				{
					MessageBox.Show("This option requires the file COMET.dat from the Minor Planet Center.\r\nWhen you have the Internet connected, you can download it using the Downloads form.", "Missing COMET.dat file", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return;
				}
				DownloadComet();
			}
			SetCometFileDate();
			CreateCometObsList();
			InitialiseCometList();
		}

		private void InitialiseCometList()
		{
			if (File.Exists(AppPath + "\\Downloaded Files\\Comet.dat"))
			{
				Elements = new ArrayList();
				cmbComet.get_Items().Clear();
				StreamReader streamReader = new StreamReader(AppPath + "\\Downloaded Files\\Comet.dat");
				while (!streamReader.EndOfStream)
				{
					string text = streamReader.ReadLine()!.PadRight(165);
					Elements.Add(text);
					cmbComet.get_Items().Add((object)text.Substring(102, 57).Trim());
				}
				streamReader.Close();
				if (cmbComet.get_Items().get_Count() >= 0)
				{
					((ListControl)cmbComet).set_SelectedIndex(cmbComet.get_Items().get_Count() - 1);
				}
			}
		}

		internal bool CreateCometObsList()
		{
			CometList.Clear();
			if (!File.Exists(Utilities.AppPath + "/Downloaded Files/CometLastObs.txt"))
			{
				WhenLastObserved = "";
				return false;
			}
			if (new FileInfo(Utilities.AppPath + "/Downloaded Files/CometLastObs.txt").Length < 100)
			{
				File.Delete(Utilities.AppPath + "/Downloaded Files/CometLastObs.txt");
				WhenLastObserved = "";
				return false;
			}
			DateTime lastWriteTime = Directory.GetLastWriteTime(Utilities.AppPath + "/Downloaded Files/CometLastObs.txt");
			WhenLastObserved = lastWriteTime.Year + " " + Utilities.ShortMonths[lastWriteTime.Month] + " " + lastWriteTime.Day.ToString().PadLeft(2);
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "/Downloaded Files/CometLastObs.txt"))
			{
				streamReader.ReadLine();
				do
				{
					string text = streamReader.ReadLine();
					if (text.Length > 54)
					{
						CometLast = new CometLastObs();
						CometLast.DecodeCometLastObs(text);
						CometList.Add(CometLast);
					}
				}
				while (!streamReader.EndOfStream);
			}
			CometList.Sort();
			return true;
		}

		private void cmdDownloadComet_Click(object sender, EventArgs e)
		{
			//IL_0018: Unknown result type (might be due to invalid IL or missing references)
			if (Utilities.InternetIsAvailable())
			{
				DownloadComet();
			}
			else
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
		}

		private void DownloadComet()
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(Settings.Default.Comet_Server, Settings.Default.Comet_file, Utilities.AppPath + "\\Downloaded Files\\Comet.dat", unzip: false, gunzip: false, ShowMessages: false);
			http.GetCometLastObservations();
			((Control)this).set_Cursor(Cursors.get_Default());
			SetCometFileDate();
			CreateCometObsList();
			InitialiseCometList();
		}

		private void Comets_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 600) | (((Control)this).get_Height() < 340)))
			{
				((Control)lstComet).set_Width(((Control)this).get_Width() - 26);
				((Control)lstComet).set_Height(((Control)this).get_Height() - 169);
			}
		}

		private void cmbComet_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)cmbComet).get_SelectedIndex() >= 0)
			{
				string text = Elements[((ListControl)cmbComet).get_SelectedIndex()]!.ToString()!.PadRight(165);
				LastObserved = LastObservedDate(text.Substring(102));
				CometID = text.Substring(102, 57).Trim();
				ElementsSource = text.Substring(159).Trim();
				TYear = int.Parse(text.Substring(14, 4));
				TMonth = int.Parse(text.Substring(19, 2));
				TDay = double.Parse(text.Substring(22, 7));
				TJD = Utilities.JD_from_Date(TYear, TMonth, TDay);
				q = double.Parse(text.Substring(30, 9));
				ecc = double.Parse(text.Substring(40, 9));
				Perihelion = double.Parse(text.Substring(50, 9));
				Node = double.Parse(text.Substring(60, 9));
				Incl = double.Parse(text.Substring(70, 9));
				H0 = double.Parse(text.Substring(90, 5));
				RCoeff = double.Parse(text.Substring(95, 5));
				Compute();
			}
		}

		private string LastObservedDate(string InLine)
		{
			if (CometList.Count < 1)
			{
				return "";
			}
			bool flag = false;
			int num = 0;
			int num2 = 0;
			num = InLine.IndexOf("P-");
			if (num > 0)
			{
				num += 2;
			}
			if (num < 0)
			{
				num = InLine.IndexOf("P/");
			}
			if (num < 0)
			{
				num = InLine.IndexOf("P ");
			}
			string text;
			if (num > 0 && num < 7)
			{
				text = InLine.Substring(0, num + 1).PadLeft(4);
			}
			else
			{
				num2 = InLine.IndexOf("(");
				text = ((num2 <= 0) ? InLine.Substring(2, 33).Trim() : InLine.Substring(2, num2 - 2).Trim());
			}
			int num3 = 0;
			int num4 = CometList.Count - 1;
			int num5 = 0;
			flag = false;
			do
			{
				num5 = (num3 + num4) / 2;
				if (text == CometList[num5].ID)
				{
					flag = true;
					break;
				}
				if (text.CompareTo(CometList[num5].ID) < 1)
				{
					num4 = num5 - 1;
				}
				else
				{
					num3 = num5 + 1;
				}
			}
			while (num3 <= num4);
			if (flag)
			{
				return CometList[num5].LastObsDate.Year + " " + Utilities.ShortMonths[CometList[num5].LastObsDate.Month] + " " + CometList[num5].LastObsDate.Day;
			}
			return "";
		}

		private void SetCometFileDate()
		{
			((Control)lblCometFileDate).set_Text("File date : " + Downloads.DownloadDate(Downloads.CometFile));
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			Compute();
		}

		private void Compute()
		{
			if (((ListControl)cmbComet).get_SelectedIndex() < 0)
			{
				return;
			}
			int num = (int)updnInterval.get_Value();
			int num2 = (int)updnDuration.get_Value();
			int num3 = 0;
			double num4 = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value());
			double num5 = (double)updnHour.get_Value();
			CancelFlag = false;
			((Control)cmdCompute).set_Visible(false);
			lstComet.get_Items().Clear();
			lstComet.get_Items().Add((object)("Ephemeris of " + CometID + num5.ToString(" at #0 hrs UTC") + "   (J2000)"));
			if (WhenLastObserved != "")
			{
				if (LastObserved.Length > 0)
				{
					lstComet.get_Items().Add((object)("   last astrometry was on " + LastObserved + "  (as at " + WhenLastObserved + ")"));
				}
				else
				{
					lstComet.get_Items().Add((object)("   no recent astrometry  (as at " + WhenLastObserved + ")"));
				}
			}
			lstComet.get_Items().Add((object)"");
			if (chkShowElements.get_Checked())
			{
				lstComet.get_Items().Add((object)string.Format("T {0, 4:F0} {1,3} {2,7:F4} TT", TYear, Utilities.ShortMonths[TMonth], TDay));
				lstComet.get_Items().Add((object)string.Format("q {0,9:F6}    Peri. {1,8:F4}", q, Perihelion));
				if (ecc < 0.95)
				{
					lstComet.get_Items().Add((object)string.Format("a {0,9:F6}    Node  {1,8:F4}", q / (1.0 - ecc), Node));
				}
				else
				{
					lstComet.get_Items().Add((object)string.Format("z {0,9:F6}    Node  {1,8:F4}", (1.0 - ecc) / q, Node));
				}
				lstComet.get_Items().Add((object)string.Format("e {0,9:F6}    Incl. {1,8:F4}", ecc, Incl));
				lstComet.get_Items().Add((object)("Ref: " + ElementsSource));
				lstComet.get_Items().Add((object)"");
			}
			lstComet.get_Items().Add((object)"   y  m   d   h  m   s     o  '  \"    delta  RSun   Elong  Phase    Mag   \"/min  PA");
			for (int i = 0; i <= num2; i += num)
			{
				Application.DoEvents();
				if (CancelFlag)
				{
					break;
				}
				Utilities.PositionfromElements(num4 + (double)i + num5 / 24.0, 0.0, 0.0, 0.0, TJD, TJD, 0.0, q, ecc, Perihelion / (180.0 / Math.PI), Node / (180.0 / Math.PI), Incl / (180.0 / Math.PI), H0, 0.0, 2.5 * RCoeff, 0.0, out var RA, out var Dec, out var RadiusVector, out var AstrometricGeocentricDistance, out var Magnitude, out var Elongation, out var PhaseAngle);
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append(Utilities.Date_from_JD(num4 + (double)i, 0) + "  ");
				stringBuilder.Append(Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 1, MinutesOnly: false) + "  ");
				stringBuilder.Append(Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 0, MinutesOnly: false) + "  ");
				stringBuilder.AppendFormat("{0,7:F3}", AstrometricGeocentricDistance);
				stringBuilder.AppendFormat("{0,7:F3}", RadiusVector);
				stringBuilder.AppendFormat("{0,7:F1}", Elongation * (180.0 / Math.PI));
				stringBuilder.AppendFormat("{0,7:F1}", PhaseAngle * (180.0 / Math.PI));
				stringBuilder.AppendFormat("{0,7:F1}", Magnitude);
				Utilities.PositionfromElements(num4 + (double)i + num5 / 24.0 + 1.0 / 144.0, 0.0, 0.0, 0.0, TJD, TJD, 0.0, q, ecc, Perihelion / (180.0 / Math.PI), Node / (180.0 / Math.PI), Incl / (180.0 / Math.PI), H0, 0.0, 2.5 * RCoeff, 0.0, out var RA2, out var Dec2, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle);
				Utilities.Distance(RA, Dec, RA2, Dec2, out var Distance, out var PA_atOrigin);
				stringBuilder.AppendFormat(" {0,7:F2}", Distance * (180.0 / Math.PI) * 360.0);
				stringBuilder.AppendFormat(" {0,5:F1}", PA_atOrigin * (180.0 / Math.PI));
				lstComet.get_Items().Add((object)stringBuilder.ToString());
				num3++;
				if (num3 % 5 == 0)
				{
					lstComet.get_Items().Add((object)"");
				}
			}
			CancelFlag = false;
			((Control)cmdCompute).set_Visible(true);
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			CancelFlag = true;
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
			if (((ListControl)cmbComet).get_SelectedIndex() >= 0)
			{
				Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectEvents(), "Ephemeris of " + Elements[((ListControl)cmbComet).get_SelectedIndex()]!.ToString()!.Substring(102).Trim(), Settings.Default.Save_EphemerisData);
			}
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstComet.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstComet.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 10);
		}

		private void updnMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMonth).Select(0, 10);
		}

		private void updnDay_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDay).Select(0, 10);
		}

		private void updnHour_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnHour).Select(0, 10);
		}

		private void updnDuration_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDuration).Select(0, 10);
		}

		private void updnInterval_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnInterval).Select(0, 10);
		}

		private void chkShowElements_CheckedChanged(object sender, EventArgs e)
		{
			Compute();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Comet ephemeris");
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
			//IL_00e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Expected O, but got Unknown
			//IL_00ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Expected O, but got Unknown
			//IL_00f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0102: Expected O, but got Unknown
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Expected O, but got Unknown
			//IL_010e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0118: Expected O, but got Unknown
			//IL_0119: Unknown result type (might be due to invalid IL or missing references)
			//IL_0123: Expected O, but got Unknown
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Expected O, but got Unknown
			//IL_012f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Expected O, but got Unknown
			//IL_013a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0144: Expected O, but got Unknown
			//IL_0145: Unknown result type (might be due to invalid IL or missing references)
			//IL_014f: Expected O, but got Unknown
			//IL_0e45: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e4f: Expected O, but got Unknown
			//IL_0ef2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0efc: Expected O, but got Unknown
			//IL_0fb2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fbc: Expected O, but got Unknown
			//IL_1299: Unknown result type (might be due to invalid IL or missing references)
			//IL_12a3: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Comets));
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmbComet = new ComboBox();
			updnYear = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnDay = new NumericUpDown();
			updnHour = new NumericUpDown();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			lstComet = new ListBox();
			cmdCompute = new Button();
			cmdCancel = new Button();
			label8 = new Label();
			cmdDownloadComet = new Button();
			chkShowElements = new CheckBox();
			updnInterval = new NumericUpDown();
			updnDuration = new NumericUpDown();
			lblCometFileDate = new Label();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)updnYear).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnHour).BeginInit();
			((ISupportInitialize)updnInterval).BeginInit();
			((ISupportInitialize)updnDuration).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(652, 24));
			((Control)menuStrip1).set_TabIndex(17);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(108, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...");
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
			((Control)cmbComet).set_Anchor((AnchorStyles)1);
			cmbComet.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbComet).set_FormattingEnabled(true);
			((Control)cmbComet).set_Location(new Point(52, 81));
			cmbComet.set_MaxDropDownItems(20);
			((Control)cmbComet).set_Name("cmbComet");
			((Control)cmbComet).set_Size(new Size(226, 21));
			((Control)cmbComet).set_TabIndex(13);
			cmbComet.add_SelectedIndexChanged((EventHandler)cmbComet_SelectedIndexChanged);
			((Control)updnYear).set_Anchor((AnchorStyles)1);
			((Control)updnYear).set_Location(new Point(105, 43));
			updnYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(55, 20));
			((Control)updnYear).set_TabIndex(2);
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((Control)updnMonth).set_Anchor((AnchorStyles)1);
			((Control)updnMonth).set_Location(new Point(168, 43));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(35, 20));
			((Control)updnMonth).set_TabIndex(4);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).add_Enter((EventHandler)updnMonth_Enter);
			((Control)updnDay).set_Anchor((AnchorStyles)1);
			((Control)updnDay).set_Location(new Point(211, 43));
			updnDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			updnDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(35, 20));
			((Control)updnDay).set_TabIndex(6);
			updnDay.set_Value(new decimal(new int[4] { 28, 0, 0, 0 }));
			((Control)updnDay).add_Enter((EventHandler)updnDay_Enter);
			((Control)updnHour).set_Anchor((AnchorStyles)1);
			((Control)updnHour).set_Location(new Point(254, 43));
			updnHour.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(35, 20));
			((Control)updnHour).set_TabIndex(8);
			((Control)updnHour).add_Enter((EventHandler)updnHour_Enter);
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(112, 29));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Year");
			((Control)label2).set_Anchor((AnchorStyles)1);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(168, 29));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(37, 13));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("Month");
			((Control)label3).set_Anchor((AnchorStyles)1);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(211, 29));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(26, 13));
			((Control)label3).set_TabIndex(5);
			((Control)label3).set_Text("Day");
			((Control)label4).set_Anchor((AnchorStyles)1);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(254, 29));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(30, 13));
			((Control)label4).set_TabIndex(7);
			((Control)label4).set_Text("Hour");
			((Control)label5).set_Anchor((AnchorStyles)1);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(62, 45));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(41, 13));
			((Control)label5).set_TabIndex(0);
			((Control)label5).set_Text("Start at");
			((Control)label6).set_Anchor((AnchorStyles)1);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(324, 47));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(78, 13));
			((Control)label6).set_TabIndex(9);
			((Control)label6).set_Text("Duration (days)");
			((Control)label7).set_Anchor((AnchorStyles)1);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(481, 47));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(73, 13));
			((Control)label7).set_TabIndex(11);
			((Control)label7).set_Text("Interval (days)");
			((Control)lstComet).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstComet).set_FormattingEnabled(true);
			lstComet.set_ItemHeight(14);
			((Control)lstComet).set_Location(new Point(9, 123));
			((Control)lstComet).set_Name("lstComet");
			((Control)lstComet).set_Size(new Size(634, 340));
			((Control)lstComet).set_TabIndex(16);
			((Control)cmdCompute).set_Anchor((AnchorStyles)1);
			((Control)cmdCompute).set_Location(new Point(424, 79));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(72, 28));
			((Control)cmdCompute).set_TabIndex(14);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)cmdCancel).set_Anchor((AnchorStyles)1);
			((Control)cmdCancel).set_Location(new Point(425, 79));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(72, 28));
			((Control)cmdCancel).set_TabIndex(15);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)label8).set_Anchor((AnchorStyles)1);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(13, 84));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(37, 13));
			((Control)label8).set_TabIndex(18);
			((Control)label8).set_Text("Comet");
			((Control)cmdDownloadComet).set_Anchor((AnchorStyles)1);
			((ButtonBase)cmdDownloadComet).set_FlatStyle((FlatStyle)1);
			((Control)cmdDownloadComet).set_Location(new Point(531, 76));
			((Control)cmdDownloadComet).set_Name("cmdDownloadComet");
			((Control)cmdDownloadComet).set_Size(new Size(83, 35));
			((Control)cmdDownloadComet).set_TabIndex(19);
			((Control)cmdDownloadComet).set_Text("&Download\r\nComet.dat");
			((ButtonBase)cmdDownloadComet).set_UseVisualStyleBackColor(true);
			((Control)cmdDownloadComet).add_Click((EventHandler)cmdDownloadComet_Click);
			((Control)chkShowElements).set_Anchor((AnchorStyles)1);
			((Control)chkShowElements).set_AutoSize(true);
			chkShowElements.set_Checked(Settings.Default.ShowCometElements);
			((Control)chkShowElements).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ShowCometElements", true, (DataSourceUpdateMode)1));
			((Control)chkShowElements).set_Location(new Point(300, 85));
			((Control)chkShowElements).set_Name("chkShowElements");
			((Control)chkShowElements).set_Size(new Size(98, 17));
			((Control)chkShowElements).set_TabIndex(20);
			((Control)chkShowElements).set_Text("Show elements");
			((ButtonBase)chkShowElements).set_UseVisualStyleBackColor(true);
			chkShowElements.add_CheckedChanged((EventHandler)chkShowElements_CheckedChanged);
			((Control)updnInterval).set_Anchor((AnchorStyles)1);
			((Control)updnInterval).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CometInterval", true, (DataSourceUpdateMode)1));
			((Control)updnInterval).set_Location(new Point(555, 43));
			updnInterval.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnInterval).set_Name("updnInterval");
			((Control)updnInterval).set_Size(new Size(35, 20));
			((Control)updnInterval).set_TabIndex(12);
			updnInterval.set_Value(Settings.Default.CometInterval);
			((Control)updnInterval).add_Enter((EventHandler)updnInterval_Enter);
			((Control)updnDuration).set_Anchor((AnchorStyles)1);
			((Control)updnDuration).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CometDuration", true, (DataSourceUpdateMode)1));
			((Control)updnDuration).set_Location(new Point(404, 43));
			updnDuration.set_Maximum(new decimal(new int[4] { 10000, 0, 0, 0 }));
			((Control)updnDuration).set_Name("updnDuration");
			((Control)updnDuration).set_Size(new Size(51, 20));
			((Control)updnDuration).set_TabIndex(10);
			updnDuration.set_Value(Settings.Default.CometDuration);
			((Control)updnDuration).add_Enter((EventHandler)updnDuration_Enter);
			((Control)lblCometFileDate).set_Anchor((AnchorStyles)1);
			((Control)lblCometFileDate).set_AutoSize(true);
			((Control)lblCometFileDate).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCometFileDate).set_Location(new Point(516, 110));
			((Control)lblCometFileDate).set_Name("lblCometFileDate");
			((Control)lblCometFileDate).set_Size(new Size(56, 13));
			((Control)lblCometFileDate).set_TabIndex(21);
			((Control)lblCometFileDate).set_Text("File date : ");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(652, 475));
			((Control)this).get_Controls().Add((Control)(object)chkShowElements);
			((Control)this).get_Controls().Add((Control)(object)cmdDownloadComet);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)lstComet);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)updnInterval);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)updnDuration);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnHour);
			((Control)this).get_Controls().Add((Control)(object)updnDay);
			((Control)this).get_Controls().Add((Control)(object)updnMonth);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)cmbComet);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)lblCometFileDate);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemComet", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemComet);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(660, 300));
			((Control)this).set_Name("Comets");
			((Control)this).set_Text("Comets");
			((Form)this).add_Load((EventHandler)Comets_Load);
			((Control)this).add_Resize((EventHandler)Comets_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)updnYear).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnHour).EndInit();
			((ISupportInitialize)updnInterval).EndInit();
			((ISupportInitialize)updnDuration).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
