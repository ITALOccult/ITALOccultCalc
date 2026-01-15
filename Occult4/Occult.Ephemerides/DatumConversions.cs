using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Mapping;
using Occult.Properties;

namespace Occult.Ephemerides
{
	public class DatumConversions : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private double LongDegForAlt;

		private double LatDegForAlt;

		private IContainer components;

		private ComboBox cmbDatum;

		private Panel panelDMS;

		private TextBox txtLongDeg;

		private TextBox txtLatSec;

		private TextBox txtLatMin;

		private TextBox txtLatDeg;

		private TextBox txtLongSec;

		private TextBox txtLongMin;

		private Label label18;

		private Label label17;

		private TextBox txtdLat;

		private TextBox txtdLong;

		private GroupBox groupBox5;

		private Label label34;

		private Label label33;

		private TextBox txtGeoid;

		private Label label32;

		private Label label31;

		private NumericUpDown updnLatitude;

		private NumericUpDown updnLongitude;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private GroupBox groupBox1;

		private GroupBox groupBox2;

		private TextBox txtLatDeg_Alt;

		private TextBox txtLatMin_Alt;

		private TextBox txtLongDeg_Alt;

		private TextBox txtLongMin_Alt;

		private Label label6;

		private Label label7;

		private TextBox txtAlt;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private Label label13;

		public DatumConversions()
		{
			InitializeComponent();
			((ListControl)cmbDatum).set_SelectedIndex(0);
		}

		private void DatumConversions_Load(object sender, EventArgs e)
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
			decimal.TryParse(Settings.Default.Site_Longitude_dd_d, out var result);
			updnLongitude.set_Value(result);
			((Control)txtLongDeg).set_Text(string.Format("{0,1:f0}", result));
			decimal.TryParse(Settings.Default.Site_Latitude_dd_d, out result);
			updnLatitude.set_Value(result);
			((Control)txtLatDeg).set_Text(string.Format("{0,1:f0}", result));
			TextBox obj = txtLongMin;
			TextBox obj2 = txtLongSec;
			TextBox obj3 = txtLatMin;
			string text;
			((Control)txtLatSec).set_Text(text = "0");
			string text2;
			((Control)obj3).set_Text(text2 = text);
			string text3;
			((Control)obj2).set_Text(text3 = text2);
			((Control)obj).set_Text(text3);
		}

		private void cmbDatum_SelectedIndexChanged(object sender, EventArgs e)
		{
			GetConversion();
		}

		private void txtLongDeg_TextChanged(object sender, EventArgs e)
		{
			GetConversion();
		}

		private void txtLongMin_TextChanged(object sender, EventArgs e)
		{
			GetConversion();
		}

		private void txtLongSec_TextChanged(object sender, EventArgs e)
		{
			GetConversion();
		}

		private void txtLatDeg_TextChanged(object sender, EventArgs e)
		{
			GetConversion();
		}

		private void txtLatMin_TextChanged(object sender, EventArgs e)
		{
			GetConversion();
		}

		private void txtLatSec_TextChanged(object sender, EventArgs e)
		{
			GetConversion();
		}

		private void GetConversion()
		{
			int datumNumber = int.Parse(cmbDatum.get_Items().get_Item(((ListControl)cmbDatum).get_SelectedIndex()).ToString()!.Substring(0, 3));
			if (!int.TryParse(((Control)txtLongDeg).get_Text().Replace("-", ""), out var result))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtLongMin).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txtLongSec).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			double num = (double)result + (double)result2 / 60.0 + result3 / 3600.0;
			if (((Control)txtLongDeg).get_Text().Contains("-"))
			{
				num = 0.0 - num;
			}
			if (!int.TryParse(((Control)txtLatDeg).get_Text().Replace("-", ""), out var result4))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtLatMin).get_Text(), out var result5))
			{
				result5 = 0;
			}
			if (!double.TryParse(((Control)txtLatSec).get_Text(), out var result6))
			{
				result6 = 0.0;
			}
			double num2 = (double)result4 + (double)result5 / 60.0 + result6 / 3600.0;
			if (((Control)txtLatDeg).get_Text().Contains("-"))
			{
				num2 = 0.0 - num2;
			}
			Utilities.GetDatumToWGS84Corrections(Utilities.RGOdatum_to_ILOCdatum(datumNumber, num, num2), num / (180.0 / Math.PI), num2 / (180.0 / Math.PI), 0.0, out var DLongitude_arcsec, out var DLatitude_arcsec);
			((Control)txtdLong).set_Text(string.Format("{0,1:F1}", DLongitude_arcsec));
			((Control)txtdLat).set_Text(string.Format("{0,1:F1}", DLatitude_arcsec));
		}

		private void updnLongitude_ValueChanged(object sender, EventArgs e)
		{
			DisplayGeoid();
		}

		private void updnLatitude_ValueChanged(object sender, EventArgs e)
		{
			DisplayGeoid();
		}

		private void DisplayGeoid()
		{
			double longitude_deg = (double)updnLongitude.get_Value();
			double latitude_deg = (double)updnLatitude.get_Value();
			((Control)txtGeoid).set_Text(string.Format("{0,2:F0}", Utilities.GeoidHeight(longitude_deg, latitude_deg)));
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Datum Conversions");
		}

		private void txtLongDeg_Alt_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()) && e.get_KeyChar() != '-' && e.get_KeyChar() != '.')
			{
				e.set_Handled(true);
			}
		}

		private void txtLongMin_Alt_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()) && e.get_KeyChar() != '.')
			{
				e.set_Handled(true);
			}
		}

		private void txtLatDeg_Alt_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()) && e.get_KeyChar() != '-' && e.get_KeyChar() != '.')
			{
				e.set_Handled(true);
			}
		}

		private void txtLatMin_Alt_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()) && e.get_KeyChar() != '.')
			{
				e.set_Handled(true);
			}
		}

		private void txtLongDeg_Alt_TextChanged(object sender, EventArgs e)
		{
			GetAltitude();
		}

		private void txtLongMin_Alt_TextChanged(object sender, EventArgs e)
		{
			GetAltitude();
		}

		private void txtLatDeg_Alt_TextChanged(object sender, EventArgs e)
		{
			GetAltitude();
		}

		private void txtLatMin_Alt_TextChanged(object sender, EventArgs e)
		{
			GetAltitude();
		}

		private void txtLongDeg_Alt_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLongDeg_Alt).SelectAll();
		}

		private void txtLongMin_Alt_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLongMin_Alt).SelectAll();
		}

		private void txtLatDeg_Alt_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatDeg_Alt).SelectAll();
		}

		private void txtLatMin_Alt_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatMin_Alt).SelectAll();
		}

		internal void GetAltitude()
		{
			int.TryParse(((Control)txtLongDeg_Alt).get_Text().Replace("-", ""), out var result);
			double.TryParse(((Control)txtLongMin_Alt).get_Text(), out var result2);
			int.TryParse(((Control)txtLatDeg_Alt).get_Text().Replace("-", ""), out var result3);
			double.TryParse(((Control)txtLatMin_Alt).get_Text().Replace("-", ""), out var result4);
			LongDegForAlt = (double)result + result2 / 60.0;
			if (((Control)txtLongDeg_Alt).get_Text().Contains("-"))
			{
				LongDegForAlt = 0.0 - LongDegForAlt;
			}
			LatDegForAlt = (double)result3 + result4 / 60.0;
			if (((Control)txtLatDeg_Alt).get_Text().Contains("-"))
			{
				LatDegForAlt = 0.0 - LatDegForAlt;
			}
			if ((LongDegForAlt <= -180.0) | (LongDegForAlt >= 180.0) | (LatDegForAlt <= -90.0) | (LatDegForAlt >= 90.0))
			{
				((Control)txtAlt).set_Text("Err");
				return;
			}
			EarthTopography.OpenEarthFile();
			((Control)txtAlt).set_Text(EarthTopography.Elevation_Above_MSL(LongDegForAlt, LatDegForAlt).ToString());
			EarthTopography.CloseEarthFile();
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
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fd: Expected O, but got Unknown
			//IL_00fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Expected O, but got Unknown
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Expected O, but got Unknown
			//IL_0114: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Expected O, but got Unknown
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Expected O, but got Unknown
			//IL_012a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0134: Expected O, but got Unknown
			//IL_0135: Unknown result type (might be due to invalid IL or missing references)
			//IL_013f: Expected O, but got Unknown
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			//IL_014a: Expected O, but got Unknown
			//IL_014b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0155: Expected O, but got Unknown
			//IL_0156: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Expected O, but got Unknown
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_016b: Expected O, but got Unknown
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0176: Expected O, but got Unknown
			//IL_0177: Unknown result type (might be due to invalid IL or missing references)
			//IL_0181: Expected O, but got Unknown
			//IL_0182: Unknown result type (might be due to invalid IL or missing references)
			//IL_018c: Expected O, but got Unknown
			//IL_018d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0197: Expected O, but got Unknown
			//IL_0198: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a2: Expected O, but got Unknown
			//IL_01a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ad: Expected O, but got Unknown
			//IL_01ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b8: Expected O, but got Unknown
			//IL_01b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c3: Expected O, but got Unknown
			//IL_01c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ce: Expected O, but got Unknown
			//IL_01cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d9: Expected O, but got Unknown
			//IL_1854: Unknown result type (might be due to invalid IL or missing references)
			//IL_185e: Expected O, but got Unknown
			//IL_190e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1918: Expected O, but got Unknown
			//IL_19c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_19d2: Expected O, but got Unknown
			//IL_1a82: Unknown result type (might be due to invalid IL or missing references)
			//IL_1a8c: Expected O, but got Unknown
			//IL_1ca5: Unknown result type (might be due to invalid IL or missing references)
			//IL_1caf: Expected O, but got Unknown
			cmbDatum = new ComboBox();
			panelDMS = new Panel();
			txtLongDeg = new TextBox();
			txtLatSec = new TextBox();
			txtLatMin = new TextBox();
			txtLatDeg = new TextBox();
			txtLongSec = new TextBox();
			txtLongMin = new TextBox();
			label18 = new Label();
			label17 = new Label();
			txtdLat = new TextBox();
			txtdLong = new TextBox();
			groupBox5 = new GroupBox();
			label34 = new Label();
			label33 = new Label();
			txtGeoid = new TextBox();
			label32 = new Label();
			label31 = new Label();
			updnLatitude = new NumericUpDown();
			updnLongitude = new NumericUpDown();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			groupBox1 = new GroupBox();
			groupBox2 = new GroupBox();
			label6 = new Label();
			label7 = new Label();
			txtAlt = new TextBox();
			label8 = new Label();
			label9 = new Label();
			txtLongDeg_Alt = new TextBox();
			txtLongMin_Alt = new TextBox();
			txtLatDeg_Alt = new TextBox();
			txtLatMin_Alt = new TextBox();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			label13 = new Label();
			((Control)panelDMS).SuspendLayout();
			((Control)groupBox5).SuspendLayout();
			((ISupportInitialize)updnLatitude).BeginInit();
			((ISupportInitialize)updnLongitude).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)this).SuspendLayout();
			cmbDatum.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbDatum).set_FormattingEnabled(true);
			cmbDatum.get_Items().AddRange(new object[79]
			{
				" 0  WGS84", " 1  RGO code 1", " 2  RGO code 2", " 3  RGO code 3", " 4  RGO code 4", " 5  RGO code 5", "10  GoogleEarth (WGS84)", "12  Christmas Island Astro 1967", "13  Chua Astro (Brazil Geodetic)", "14  Corrego Alegre (Brazil)",
				"15  Easter Island Astro 1967", "16  European 1950  ", "17  Graciosa Island (Azores)", "18  Gizo, Provisional DOS  ", "19  Guam", "20  Heard Astro 1969", "21  Iben Astro, Navy 1947 (Truk) ", "22  Indian", "23  Isla Socorro Astro", "24  Johnston Island 1961 ",
				"25  Kusaie Astro 1962,1965", "26  Luzon 1911 (Philippines) ", "27  Midway Astro 1961", "28  New Zealand 1949 ", "29  North American 1927  ", "30 *Cape Canaveral ", "31 *White Sands", "32  Old Bavarian ", "33  Old Hawaiian ", "34  Ordnance Survey of Great Britain 1936 ",
				"35  Pico de las Nieves (Canaries) ", "36  Pitcairn Island Astro ", "37  Potsdam", "38  Provisional South American 1956", "39  Provisional South Chile 1963  ", "40  Pulkovo 1942  ", "41  South American 1969", "42  Southeast Island (Mahe)", "43  South Georgia Astro", "44  Swallow Islands (Solomons)",
				"45  Tananarive", "46  Tokyo ", "47  Tristan Astro 1968", "48  Viti Levu 1916 (Fiji) ", "49  Wake Island Astro 1952", "50  Yof Astro 1967 (Dakar)", "51  Palmer Astro 1969 (Antarctica)", "52  Efate (New Hebrides)", "53  Marcus Island 1965", "54  Canton Astro 1966  ",
				"56  Yap Island", "58  Kourou (French Guiana)", "59  Ordnance Survey of Great Britain 1970 ", "60  Qornoq (Greenland)", "61  Adindan (Ethiopia) ", "62  American Samoa 1962", "63  Arc-Cape (South Africa)", "64  Argentine  ", "65  Ascension Island 1958  ", "66  Australian Geodetic",
				"67  Bermuda 1957", "68  Berne 1898 ", "69  Betio Island 1966  ", "70  Camp Area Astro 1961-62 USGS", "71  Batavia (Java)", "72  Palestine (Israel,Jordan)", "73  Hermannskogel (Austria,Czech.,Yugoslavia)", "74  Kandawala (Ceylon)", "80  ETRS89 (European Terrestial Reference System)", "81  Amersfoort 1885 (Netherlands)",
				"82  NAD83/NAD1983 (=WGS84)", "84  WGS84", "85  JGD2000 (=WGS84)", "86  GDA94 (=WGS84)", "87  NZGD2000 (=WGS84)", "88  NGRF2000 (=WGS84)", "89  KDG2000 (=WGS84)", "90  Hartebeesthoek94 (=WGS84)", "91  TWD94 (=WGS84)"
			});
			((Control)cmbDatum).set_Location(new Point(84, 29));
			cmbDatum.set_MaxDropDownItems(20);
			((Control)cmbDatum).set_Name("cmbDatum");
			((Control)cmbDatum).set_Size(new Size(266, 21));
			((Control)cmbDatum).set_TabIndex(0);
			cmbDatum.add_SelectedIndexChanged((EventHandler)cmbDatum_SelectedIndexChanged);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongDeg);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatSec);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatMin);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatDeg);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongSec);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongMin);
			((Control)panelDMS).set_Location(new Point(84, 65));
			((Control)panelDMS).set_Name("panelDMS");
			((Control)panelDMS).set_Size(new Size(113, 52));
			((Control)panelDMS).set_TabIndex(14);
			((Control)txtLongDeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDeg).set_Location(new Point(4, 2));
			((Control)txtLongDeg).set_Name("txtLongDeg");
			((Control)txtLongDeg).set_Size(new Size(34, 20));
			((Control)txtLongDeg).set_TabIndex(0);
			((Control)txtLongDeg).add_TextChanged((EventHandler)txtLongDeg_TextChanged);
			((Control)txtLatSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatSec).set_Location(new Point(71, 28));
			((Control)txtLatSec).set_Name("txtLatSec");
			((Control)txtLatSec).set_Size(new Size(39, 20));
			((Control)txtLatSec).set_TabIndex(5);
			((Control)txtLatSec).add_TextChanged((EventHandler)txtLatSec_TextChanged);
			((Control)txtLatMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatMin).set_Location(new Point(44, 28));
			((Control)txtLatMin).set_Name("txtLatMin");
			((Control)txtLatMin).set_Size(new Size(21, 20));
			((Control)txtLatMin).set_TabIndex(4);
			((Control)txtLatMin).add_TextChanged((EventHandler)txtLatMin_TextChanged);
			((Control)txtLatDeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDeg).set_Location(new Point(4, 28));
			((Control)txtLatDeg).set_Name("txtLatDeg");
			((Control)txtLatDeg).set_Size(new Size(34, 20));
			((Control)txtLatDeg).set_TabIndex(3);
			((Control)txtLatDeg).add_TextChanged((EventHandler)txtLatDeg_TextChanged);
			((Control)txtLongSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongSec).set_Location(new Point(71, 2));
			((Control)txtLongSec).set_Name("txtLongSec");
			((Control)txtLongSec).set_Size(new Size(40, 20));
			((Control)txtLongSec).set_TabIndex(2);
			((Control)txtLongSec).add_TextChanged((EventHandler)txtLongSec_TextChanged);
			((Control)txtLongMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongMin).set_Location(new Point(44, 2));
			((Control)txtLongMin).set_Name("txtLongMin");
			((Control)txtLongMin).set_Size(new Size(21, 20));
			((Control)txtLongMin).set_TabIndex(1);
			((Control)txtLongMin).add_TextChanged((EventHandler)txtLongMin_TextChanged);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(36, 96));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(45, 13));
			((Control)label18).set_TabIndex(15);
			((Control)label18).set_Text("Latitude");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(14, 70));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(67, 13));
			((Control)label17).set_TabIndex(13);
			((Control)label17).set_Text("E. Longitude");
			((Control)txtdLat).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtdLat).set_Location(new Point(340, 93));
			((Control)txtdLat).set_Name("txtdLat");
			((Control)txtdLat).set_Size(new Size(40, 20));
			((Control)txtdLat).set_TabIndex(16);
			txtdLat.set_TextAlign((HorizontalAlignment)2);
			((Control)txtdLong).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtdLong).set_Location(new Point(340, 67));
			((Control)txtdLong).set_Name("txtdLong");
			((Control)txtdLong).set_Size(new Size(40, 20));
			((Control)txtdLong).set_TabIndex(17);
			txtdLong.set_TextAlign((HorizontalAlignment)2);
			((Control)groupBox5).get_Controls().Add((Control)(object)label34);
			((Control)groupBox5).get_Controls().Add((Control)(object)label33);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtGeoid);
			((Control)groupBox5).get_Controls().Add((Control)(object)label32);
			((Control)groupBox5).get_Controls().Add((Control)(object)label31);
			((Control)groupBox5).get_Controls().Add((Control)(object)updnLatitude);
			((Control)groupBox5).get_Controls().Add((Control)(object)updnLongitude);
			((Control)groupBox5).set_Location(new Point(12, 194));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(455, 58));
			((Control)groupBox5).set_TabIndex(18);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("Height of Mean Sea Level [ MSL ]  above WGS84 geoid");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Location(new Point(426, 28));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(15, 13));
			((Control)label34).set_TabIndex(57);
			((Control)label34).set_Text("m");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Location(new Point(292, 28));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(100, 13));
			((Control)label33).set_TabIndex(56);
			((Control)label33).set_Text("Height above geoid");
			((Control)txtGeoid).set_Location(new Point(394, 25));
			((Control)txtGeoid).set_Name("txtGeoid");
			((Control)txtGeoid).set_Size(new Size(30, 20));
			((Control)txtGeoid).set_TabIndex(55);
			((Control)txtGeoid).set_Text("0");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Location(new Point(6, 28));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(74, 13));
			((Control)label32).set_TabIndex(54);
			((Control)label32).set_Text("East longitude");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Location(new Point(166, 28));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(45, 13));
			((Control)label31).set_TabIndex(53);
			((Control)label31).set_Text("Latitude");
			updnLatitude.set_DecimalPlaces(1);
			((Control)updnLatitude).set_Location(new Point(213, 24));
			updnLatitude.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatitude.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatitude).set_Name("updnLatitude");
			((Control)updnLatitude).set_Size(new Size(52, 20));
			((Control)updnLatitude).set_TabIndex(1);
			updnLatitude.add_ValueChanged((EventHandler)updnLatitude_ValueChanged);
			updnLongitude.set_DecimalPlaces(1);
			((Control)updnLongitude).set_Location(new Point(82, 24));
			updnLongitude.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongitude.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongitude).set_Name("updnLongitude");
			((Control)updnLongitude).set_Size(new Size(60, 20));
			((Control)updnLongitude).set_TabIndex(0);
			updnLongitude.add_ValueChanged((EventHandler)updnLongitude_ValueChanged);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(483, 24));
			((Control)menuStrip1).set_TabIndex(19);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(43, 32));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(38, 13));
			((Control)label1).set_TabIndex(20);
			((Control)label1).set_Text("Datum");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(238, 96));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(95, 13));
			((Control)label2).set_TabIndex(21);
			((Control)label2).set_Text("Latitude correction");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(230, 70));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(104, 13));
			((Control)label3).set_TabIndex(22);
			((Control)label3).set_Text("Longitude correction");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(382, 67));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(12, 13));
			((Control)label4).set_TabIndex(23);
			((Control)label4).set_Text("\"");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(382, 94));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(12, 13));
			((Control)label5).set_TabIndex(24);
			((Control)label5).set_Text("\"");
			((Control)groupBox1).get_Controls().Add((Control)(object)label5);
			((Control)groupBox1).get_Controls().Add((Control)(object)label4);
			((Control)groupBox1).get_Controls().Add((Control)(object)label3);
			((Control)groupBox1).get_Controls().Add((Control)(object)label2);
			((Control)groupBox1).get_Controls().Add((Control)(object)label1);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtdLong);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtdLat);
			((Control)groupBox1).get_Controls().Add((Control)(object)panelDMS);
			((Control)groupBox1).get_Controls().Add((Control)(object)label18);
			((Control)groupBox1).get_Controls().Add((Control)(object)label17);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmbDatum);
			((Control)groupBox1).set_Location(new Point(12, 45));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(455, 132));
			((Control)groupBox1).set_TabIndex(25);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Correction to apply to Longitude and Latitude to convert to WGS84");
			((Control)groupBox2).get_Controls().Add((Control)(object)label11);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtLatDeg_Alt);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtLatMin_Alt);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtLongDeg_Alt);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtLongMin_Alt);
			((Control)groupBox2).get_Controls().Add((Control)(object)label6);
			((Control)groupBox2).get_Controls().Add((Control)(object)label7);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtAlt);
			((Control)groupBox2).get_Controls().Add((Control)(object)label8);
			((Control)groupBox2).get_Controls().Add((Control)(object)label9);
			((Control)groupBox2).get_Controls().Add((Control)(object)label10);
			((Control)groupBox2).get_Controls().Add((Control)(object)label12);
			((Control)groupBox2).get_Controls().Add((Control)(object)label13);
			((Control)groupBox2).set_Location(new Point(13, 272));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(453, 51));
			((Control)groupBox2).set_TabIndex(26);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Topographic altitude above Mean Sea Level [ MSL ] ");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(434, 26));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(15, 13));
			((Control)label6).set_TabIndex(64);
			((Control)label6).set_Text("m");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(292, 26));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(100, 13));
			((Control)label7).set_TabIndex(63);
			((Control)label7).set_Text("Altitude above MSL");
			((Control)txtAlt).set_Location(new Point(394, 23));
			((Control)txtAlt).set_Name("txtAlt");
			((Control)txtAlt).set_Size(new Size(39, 20));
			((Control)txtAlt).set_TabIndex(62);
			((Control)txtAlt).set_Text("0");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(9, 26));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(74, 13));
			((Control)label8).set_TabIndex(61);
			((Control)label8).set_Text("East longitude");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(169, 26));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(45, 13));
			((Control)label9).set_TabIndex(60);
			((Control)label9).set_Text("Latitude");
			((Control)txtLongDeg_Alt).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDeg_Alt).set_Location(new Point(88, 23));
			((Control)txtLongDeg_Alt).set_Name("txtLongDeg_Alt");
			((Control)txtLongDeg_Alt).set_Size(new Size(34, 20));
			((Control)txtLongDeg_Alt).set_TabIndex(65);
			((Control)txtLongDeg_Alt).set_Text("0");
			((Control)txtLongDeg_Alt).add_TextChanged((EventHandler)txtLongDeg_Alt_TextChanged);
			((Control)txtLongDeg_Alt).add_Enter((EventHandler)txtLongDeg_Alt_Enter);
			((Control)txtLongDeg_Alt).add_KeyPress(new KeyPressEventHandler(txtLongDeg_Alt_KeyPress));
			((Control)txtLongMin_Alt).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongMin_Alt).set_Location(new Point(128, 23));
			((Control)txtLongMin_Alt).set_Name("txtLongMin_Alt");
			((Control)txtLongMin_Alt).set_Size(new Size(27, 20));
			((Control)txtLongMin_Alt).set_TabIndex(66);
			((Control)txtLongMin_Alt).set_Text("0");
			((Control)txtLongMin_Alt).add_TextChanged((EventHandler)txtLongMin_Alt_TextChanged);
			((Control)txtLongMin_Alt).add_Enter((EventHandler)txtLongMin_Alt_Enter);
			((Control)txtLongMin_Alt).add_KeyPress(new KeyPressEventHandler(txtLongMin_Alt_KeyPress));
			((Control)txtLatDeg_Alt).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDeg_Alt).set_Location(new Point(215, 23));
			((Control)txtLatDeg_Alt).set_Name("txtLatDeg_Alt");
			((Control)txtLatDeg_Alt).set_Size(new Size(34, 20));
			((Control)txtLatDeg_Alt).set_TabIndex(67);
			((Control)txtLatDeg_Alt).set_Text("0");
			((Control)txtLatDeg_Alt).add_TextChanged((EventHandler)txtLatDeg_Alt_TextChanged);
			((Control)txtLatDeg_Alt).add_Enter((EventHandler)txtLatDeg_Alt_Enter);
			((Control)txtLatDeg_Alt).add_KeyPress(new KeyPressEventHandler(txtLatDeg_Alt_KeyPress));
			((Control)txtLatMin_Alt).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatMin_Alt).set_Location(new Point(255, 23));
			((Control)txtLatMin_Alt).set_Name("txtLatMin_Alt");
			((Control)txtLatMin_Alt).set_Size(new Size(27, 20));
			((Control)txtLatMin_Alt).set_TabIndex(68);
			((Control)txtLatMin_Alt).set_Text("0");
			((Control)txtLatMin_Alt).add_TextChanged((EventHandler)txtLatMin_Alt_TextChanged);
			((Control)txtLatMin_Alt).add_Enter((EventHandler)txtLatMin_Alt_Enter);
			((Control)txtLatMin_Alt).add_KeyPress(new KeyPressEventHandler(txtLatMin_Alt_KeyPress));
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(281, 16));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(9, 13));
			((Control)label10).set_TabIndex(69);
			((Control)label10).set_Text("'");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(156, 19));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(9, 13));
			((Control)label11).set_TabIndex(70);
			((Control)label11).set_Text("'");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(117, 11));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(13, 13));
			((Control)label12).set_TabIndex(70);
			((Control)label12).set_Text("o");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(242, 11));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(13, 13));
			((Control)label13).set_TabIndex(71);
			((Control)label13).set_Text("o");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(483, 337));
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)groupBox5);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemDatums", true, (DataSourceUpdateMode)1));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationEphemDatums);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("DatumConversions");
			((Control)this).set_Text("Datum conversion ;  MSL above Geoid ;   Altitude above MSL");
			((Form)this).add_Load((EventHandler)DatumConversions_Load);
			((Control)panelDMS).ResumeLayout(false);
			((Control)panelDMS).PerformLayout();
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((ISupportInitialize)updnLatitude).EndInit();
			((ISupportInitialize)updnLongitude).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
