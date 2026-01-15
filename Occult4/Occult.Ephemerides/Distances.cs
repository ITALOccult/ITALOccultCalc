using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Ephemerides
{
	public class Distances : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private bool Dec1North = true;

		private bool Dec2North = true;

		private bool Long1East = true;

		private bool Long2East = true;

		private bool Lat1North = true;

		private bool Lat2North = true;

		private bool Long3East = true;

		private bool Lat3North = true;

		private IContainer components;

		private RadioButton optDMS;

		private RadioButton optDeg;

		private Label label8;

		private Label label7;

		private TextBox txtPA;

		private TextBox txtSep;

		private Label label6;

		private Label label5;

		private Label label4;

		private Label label3;

		private Label label2;

		private Label label1;

		private NumericUpDown DS2;

		private NumericUpDown DM2;

		private NumericUpDown DD2;

		private NumericUpDown RAS2;

		private NumericUpDown RAM2;

		private NumericUpDown RAH2;

		private NumericUpDown DS1;

		private NumericUpDown DM1;

		private NumericUpDown DD1;

		private NumericUpDown RAS1;

		private NumericUpDown RAM1;

		private NumericUpDown RAH1;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private Button cmdHelp;

		private PictureBox pictureBox1;

		private Panel panel1;

		private Label label13;

		private Panel panel2;

		private Label label22;

		private Label label24;

		private Label label26;

		private Label label14;

		private TextBox txtDistance;

		private PictureBox pictureBox2;

		private NumericUpDown LongDeg1;

		private Button cmdHelp2;

		private NumericUpDown LongMin1;

		private Label label15;

		private NumericUpDown LongSec1;

		private Label label16;

		private NumericUpDown LatDeg1;

		private Label label17;

		private NumericUpDown LatMin1;

		private NumericUpDown LatSec1;

		private NumericUpDown LongDeg2;

		private NumericUpDown LongMin2;

		private NumericUpDown LongSec2;

		private Label label20;

		private NumericUpDown LatDeg2;

		private NumericUpDown LatMin2;

		private NumericUpDown LatSec2;

		private Label label21;

		private Label label23;

		private Label label25;

		private Label label18;

		private Label lblEarthFlattening;

		private Label lblEarthRadius;

		private Button cmdDecNS1;

		private Button cmdDecNS2;

		private Button cmdLatNS2;

		private Button cmdLatNS1;

		private Button cmdLongEW2;

		private Button cmdLongEW1;

		private Label label19;

		private Label lblConverged;

		private Label label27;

		private Panel panel3;

		private TextBox txtAzimuth;

		private Label label44;

		private Label label28;

		private Label label29;

		private Button cmdLatNS3;

		private Button cmdLongEW3;

		private Label label30;

		private Label label31;

		private Label label32;

		private Label label33;

		private Label label34;

		private TextBox txtDistanceTo;

		private NumericUpDown LongDeg3;

		private NumericUpDown LongMin3;

		private NumericUpDown LongSec3;

		private Label label36;

		private NumericUpDown LatDeg3;

		private Label label37;

		private NumericUpDown LatMin3;

		private NumericUpDown LatSec3;

		private Label label38;

		private Label label39;

		private Label label40;

		private Label label41;

		private Label label42;

		private Label label43;

		private PictureBox pictureBox3;

		private Button cmdHelp3;

		private Label lblEndLat;

		private Label lblEndLong;

		private Label lblAZ2;

		private Label lblAZ1;

		private TextBox txtEndLat;

		private TextBox txtEndLong;

		public Distances()
		{
			InitializeComponent();
		}

		private void Distances_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			((Control)lblEarthRadius).set_Text(string.Format("Earth's radius = {0,6:f0}m", 6378137.0));
			((Control)lblEarthFlattening).set_Text(string.Format("Earth's flattening = 1/{0,6:f3}", 298.257223563));
		}

		private void ComputeSeparation()
		{
			double rA_OfOrigin = ((double)RAH1.get_Value() + (double)RAM1.get_Value() / 60.0 + (double)RAS1.get_Value() / 3600.0) * 15.0 / (180.0 / Math.PI);
			double rA_OfTarget = ((double)RAH2.get_Value() + (double)RAM2.get_Value() / 60.0 + (double)RAS2.get_Value() / 3600.0) * 15.0 / (180.0 / Math.PI);
			double num = ((double)DD1.get_Value() + (double)DM1.get_Value() / 60.0 + (double)DS1.get_Value() / 3600.0) / (180.0 / Math.PI);
			if (!Dec1North)
			{
				num = 0.0 - num;
			}
			double num2 = ((double)DD2.get_Value() + (double)DM2.get_Value() / 60.0 + (double)DS2.get_Value() / 3600.0) / (180.0 / Math.PI);
			if (!Dec2North)
			{
				num2 = 0.0 - num2;
			}
			Utilities.Distance(rA_OfOrigin, num, rA_OfTarget, num2, out var Distance, out var PA_atOrigin);
			if (optDMS.get_Checked())
			{
				((Control)txtSep).set_Text(Utilities.DEGtoDMS(Distance * (180.0 / Math.PI), 3, 2, MinutesOnly: false));
				((Control)txtPA).set_Text(Utilities.DEGtoDMS(PA_atOrigin * (180.0 / Math.PI), 3, 2, MinutesOnly: true));
			}
			else
			{
				((Control)txtSep).set_Text(string.Format("{0,1:f6}", Distance * (180.0 / Math.PI)));
				((Control)txtPA).set_Text(string.Format("{0,1:f5}", PA_atOrigin * (180.0 / Math.PI)));
			}
		}

		private void optDeg_CheckedChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void optDMS_CheckedChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void RAH1_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void RAM1_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void RAS1_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void DD1_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void DM1_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void DS1_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void RAH2_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void RAM2_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void RAS2_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void DD2_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void DM2_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void DS2_ValueChanged(object sender, EventArgs e)
		{
			ComputeSeparation();
		}

		private void cmdDecNS1_Click(object sender, EventArgs e)
		{
			if (Dec1North)
			{
				Dec1North = false;
				((Control)cmdDecNS1).set_Text("-");
			}
			else
			{
				Dec1North = true;
				((Control)cmdDecNS1).set_Text("+");
			}
			ComputeSeparation();
		}

		private void cmdDecNS2_Click(object sender, EventArgs e)
		{
			if (Dec2North)
			{
				Dec2North = false;
				((Control)cmdDecNS2).set_Text("-");
			}
			else
			{
				Dec2North = true;
				((Control)cmdDecNS2).set_Text("+");
			}
			ComputeSeparation();
		}

		private void cmdLongEW1_Click(object sender, EventArgs e)
		{
			if (Long1East)
			{
				Long1East = false;
				((Control)cmdLongEW1).set_Text("-");
			}
			else
			{
				Long1East = true;
				((Control)cmdLongEW1).set_Text("+");
			}
			ComputeEarthSeparation();
		}

		private void LongDeg1_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void LongMin1_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void LongSec1_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void cmdLongEW2_Click(object sender, EventArgs e)
		{
			if (Long2East)
			{
				Long2East = false;
				((Control)cmdLongEW2).set_Text("-");
			}
			else
			{
				Long2East = true;
				((Control)cmdLongEW2).set_Text("+");
			}
			ComputeEarthSeparation();
		}

		private void LongDeg2_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void LongMin2_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void LongSec2_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void cmdLatNS1_Click(object sender, EventArgs e)
		{
			if (Lat1North)
			{
				Lat1North = false;
				((Control)cmdLatNS1).set_Text("-");
			}
			else
			{
				Lat1North = true;
				((Control)cmdLatNS1).set_Text("+");
			}
			ComputeEarthSeparation();
		}

		private void LatDeg1_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void LatMin1_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void LatSec1_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void cmdLatNS2_Click(object sender, EventArgs e)
		{
			if (Lat2North)
			{
				Lat2North = false;
				((Control)cmdLatNS2).set_Text("-");
			}
			else
			{
				Lat2North = true;
				((Control)cmdLatNS2).set_Text("+");
			}
			ComputeEarthSeparation();
		}

		private void LatDeg2_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void LatMin2_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void LatSec2_ValueChanged(object sender, EventArgs e)
		{
			ComputeEarthSeparation();
		}

		private void ComputeEarthSeparation()
		{
			bool flag = false;
			double DistanceKm = 0.0;
			double AZ = 0.0;
			double AZ2 = 0.0;
			double num = ((double)LongDeg1.get_Value() + (double)LongMin1.get_Value() / 60.0 + (double)LongSec1.get_Value() / 3600.0) / (180.0 / Math.PI);
			if (!Long1East)
			{
				num = 0.0 - num;
			}
			double num2 = ((double)LongDeg2.get_Value() + (double)LongMin2.get_Value() / 60.0 + (double)LongSec2.get_Value() / 3600.0) / (180.0 / Math.PI);
			if (!Long2East)
			{
				num2 = 0.0 - num2;
			}
			double num3 = ((double)LatDeg1.get_Value() + (double)LatMin1.get_Value() / 60.0 + (double)LatSec1.get_Value() / 3600.0) / (180.0 / Math.PI);
			if (!Lat1North)
			{
				num3 = 0.0 - num3;
			}
			double num4 = ((double)LatDeg2.get_Value() + (double)LatMin2.get_Value() / 60.0 + (double)LatSec2.get_Value() / 3600.0) / (180.0 / Math.PI);
			if (!Lat2North)
			{
				num4 = 0.0 - num4;
			}
			flag = Utilities.GetGeoidDistances(num, num3, num2, num4, out DistanceKm, out AZ, out AZ2);
			((Control)txtDistance).set_Text(string.Format("{0,2:f3} km", DistanceKm));
			((Control)lblAZ1).set_Text(string.Format("{0,6:f3}", AZ * (180.0 / Math.PI)));
			((Control)lblAZ2).set_Text(string.Format("{0,6:f3}", AZ2 * (180.0 / Math.PI)));
			((Control)lblConverged).set_Visible(!flag);
		}

		private void LongDeg3_ValueChanged(object sender, EventArgs e)
		{
			ComputeLocation();
		}

		private void LongMin3_ValueChanged(object sender, EventArgs e)
		{
			ComputeLocation();
		}

		private void LongSec3_ValueChanged(object sender, EventArgs e)
		{
			ComputeLocation();
		}

		private void LatDeg3_ValueChanged(object sender, EventArgs e)
		{
			ComputeLocation();
		}

		private void LatMin3_ValueChanged(object sender, EventArgs e)
		{
			ComputeLocation();
		}

		private void LatSec3_ValueChanged(object sender, EventArgs e)
		{
			ComputeLocation();
		}

		private void txtDistanceTo_TextChanged(object sender, EventArgs e)
		{
			ComputeLocation();
		}

		private void txtAzimuth_TextChanged(object sender, EventArgs e)
		{
			ComputeLocation();
		}

		private void cmdLongEW3_Click(object sender, EventArgs e)
		{
			if (Long3East)
			{
				Long3East = false;
				((Control)cmdLongEW3).set_Text("-");
			}
			else
			{
				Long3East = true;
				((Control)cmdLongEW3).set_Text("+");
			}
			ComputeLocation();
		}

		private void cmdLatNS3_Click(object sender, EventArgs e)
		{
			if (Lat3North)
			{
				Lat3North = false;
				((Control)cmdLatNS3).set_Text("-");
			}
			else
			{
				Lat3North = true;
				((Control)cmdLatNS3).set_Text("+");
			}
			ComputeLocation();
		}

		private void ComputeLocation()
		{
			double num = ((double)LongDeg3.get_Value() + (double)LongMin3.get_Value() / 60.0 + (double)LongSec3.get_Value() / 3600.0) / (180.0 / Math.PI);
			if (!Long3East)
			{
				num = 0.0 - num;
			}
			double num2 = ((double)LatDeg3.get_Value() + (double)LatMin3.get_Value() / 60.0 + (double)LatSec3.get_Value() / 3600.0) / (180.0 / Math.PI);
			if (!Lat3North)
			{
				num2 = 0.0 - num2;
			}
			if (!double.TryParse(((Control)txtDistanceTo).get_Text(), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtAzimuth).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			Utilities.GetGeoidLocation(num, num2, result, result2 / (180.0 / Math.PI), out var LongEnd, out var LatEnd);
			((Control)txtEndLong).set_Text(Utilities.DEGtoDMS(LongEnd * (180.0 / Math.PI), 4, 2, MinutesOnly: false));
			((Control)txtEndLat).set_Text(Utilities.DEGtoDMS(LatEnd * (180.0 / Math.PI), 3, 2, MinutesOnly: false));
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Distances");
		}

		private void cmdHelp2_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Distances");
		}

		private void cmdHelp3_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Distances");
		}

		private void RAH1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)RAH1).Select(0, 10);
		}

		private void RAM1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)RAM1).Select(0, 10);
		}

		private void RAS1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)RAS1).Select(0, 10);
		}

		private void DD1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)DD1).Select(0, 10);
		}

		private void DM1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)DM1).Select(0, 10);
		}

		private void DS1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)DS1).Select(0, 10);
		}

		private void RAH2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)RAH2).Select(0, 10);
		}

		private void RAM2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)RAM2).Select(0, 10);
		}

		private void RAS2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)RAS2).Select(0, 10);
		}

		private void DD2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)DD2).Select(0, 10);
		}

		private void DM2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)DM2).Select(0, 10);
		}

		private void DS2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)DS2).Select(0, 10);
		}

		private void LongDeg1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LongDeg1).Select(0, 10);
		}

		private void LongMin1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LongMin1).Select(0, 10);
		}

		private void LongSec1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LongSec1).Select(0, 10);
		}

		private void LatDeg1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LatDeg1).Select(0, 10);
		}

		private void LatMin1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LatMin1).Select(0, 10);
		}

		private void LatSec1_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LatSec1).Select(0, 10);
		}

		private void LongDeg2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LongDeg2).Select(0, 10);
		}

		private void LongMin2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LongMin2).Select(0, 10);
		}

		private void LongSec2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LongSec2).Select(0, 10);
		}

		private void LatDeg2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LatDeg2).Select(0, 10);
		}

		private void LatMin2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LatMin2).Select(0, 10);
		}

		private void LatSec2_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LatSec2).Select(0, 10);
		}

		private void LongDeg3_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LongDeg3).Select(0, 10);
		}

		private void LongMin3_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LongMin3).Select(0, 10);
		}

		private void LongSec3_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LongSec3).Select(0, 10);
		}

		private void LatDeg3_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LatDeg3).Select(0, 10);
		}

		private void LatMin3_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LatMin3).Select(0, 10);
		}

		private void LatSec3_Enter(object sender, EventArgs e)
		{
			((UpDownBase)LatSec3).Select(0, 10);
		}

		private void txtDistanceTo_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDistanceTo).SelectAll();
		}

		private void txtAzimuth_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtAzimuth).SelectAll();
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
			//IL_01da: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e4: Expected O, but got Unknown
			//IL_01e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ef: Expected O, but got Unknown
			//IL_01f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fa: Expected O, but got Unknown
			//IL_01fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0205: Expected O, but got Unknown
			//IL_0206: Unknown result type (might be due to invalid IL or missing references)
			//IL_0210: Expected O, but got Unknown
			//IL_0211: Unknown result type (might be due to invalid IL or missing references)
			//IL_021b: Expected O, but got Unknown
			//IL_021c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0226: Expected O, but got Unknown
			//IL_0227: Unknown result type (might be due to invalid IL or missing references)
			//IL_0231: Expected O, but got Unknown
			//IL_0232: Unknown result type (might be due to invalid IL or missing references)
			//IL_023c: Expected O, but got Unknown
			//IL_023d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0247: Expected O, but got Unknown
			//IL_0248: Unknown result type (might be due to invalid IL or missing references)
			//IL_0252: Expected O, but got Unknown
			//IL_0253: Unknown result type (might be due to invalid IL or missing references)
			//IL_025d: Expected O, but got Unknown
			//IL_025e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0268: Expected O, but got Unknown
			//IL_0269: Unknown result type (might be due to invalid IL or missing references)
			//IL_0273: Expected O, but got Unknown
			//IL_0274: Unknown result type (might be due to invalid IL or missing references)
			//IL_027e: Expected O, but got Unknown
			//IL_027f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0289: Expected O, but got Unknown
			//IL_028a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0294: Expected O, but got Unknown
			//IL_0295: Unknown result type (might be due to invalid IL or missing references)
			//IL_029f: Expected O, but got Unknown
			//IL_02a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02aa: Expected O, but got Unknown
			//IL_02ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b5: Expected O, but got Unknown
			//IL_02b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c0: Expected O, but got Unknown
			//IL_02c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02cb: Expected O, but got Unknown
			//IL_02cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d6: Expected O, but got Unknown
			//IL_02d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e1: Expected O, but got Unknown
			//IL_02e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ec: Expected O, but got Unknown
			//IL_02ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f7: Expected O, but got Unknown
			//IL_02f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0302: Expected O, but got Unknown
			//IL_0303: Unknown result type (might be due to invalid IL or missing references)
			//IL_030d: Expected O, but got Unknown
			//IL_030e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0318: Expected O, but got Unknown
			//IL_0319: Unknown result type (might be due to invalid IL or missing references)
			//IL_0323: Expected O, but got Unknown
			//IL_0324: Unknown result type (might be due to invalid IL or missing references)
			//IL_032e: Expected O, but got Unknown
			//IL_032f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0339: Expected O, but got Unknown
			//IL_033a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0344: Expected O, but got Unknown
			//IL_0345: Unknown result type (might be due to invalid IL or missing references)
			//IL_034f: Expected O, but got Unknown
			//IL_0350: Unknown result type (might be due to invalid IL or missing references)
			//IL_035a: Expected O, but got Unknown
			//IL_035b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0365: Expected O, but got Unknown
			//IL_0366: Unknown result type (might be due to invalid IL or missing references)
			//IL_0370: Expected O, but got Unknown
			//IL_0371: Unknown result type (might be due to invalid IL or missing references)
			//IL_037b: Expected O, but got Unknown
			//IL_037c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0386: Expected O, but got Unknown
			//IL_0387: Unknown result type (might be due to invalid IL or missing references)
			//IL_0391: Expected O, but got Unknown
			//IL_0392: Unknown result type (might be due to invalid IL or missing references)
			//IL_039c: Expected O, but got Unknown
			//IL_039d: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a7: Expected O, but got Unknown
			//IL_03a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b2: Expected O, but got Unknown
			//IL_03b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03bd: Expected O, but got Unknown
			//IL_03be: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c8: Expected O, but got Unknown
			//IL_03c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d3: Expected O, but got Unknown
			//IL_03d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03de: Expected O, but got Unknown
			//IL_03df: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e9: Expected O, but got Unknown
			//IL_03ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f4: Expected O, but got Unknown
			//IL_03f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ff: Expected O, but got Unknown
			//IL_0400: Unknown result type (might be due to invalid IL or missing references)
			//IL_040a: Expected O, but got Unknown
			//IL_040b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0415: Expected O, but got Unknown
			//IL_0416: Unknown result type (might be due to invalid IL or missing references)
			//IL_0420: Expected O, but got Unknown
			//IL_0421: Unknown result type (might be due to invalid IL or missing references)
			//IL_042b: Expected O, but got Unknown
			//IL_042c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0436: Expected O, but got Unknown
			//IL_0437: Unknown result type (might be due to invalid IL or missing references)
			//IL_0441: Expected O, but got Unknown
			//IL_0442: Unknown result type (might be due to invalid IL or missing references)
			//IL_044c: Expected O, but got Unknown
			//IL_044d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0457: Expected O, but got Unknown
			//IL_0458: Unknown result type (might be due to invalid IL or missing references)
			//IL_0462: Expected O, but got Unknown
			//IL_0463: Unknown result type (might be due to invalid IL or missing references)
			//IL_046d: Expected O, but got Unknown
			//IL_046e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0478: Expected O, but got Unknown
			//IL_0479: Unknown result type (might be due to invalid IL or missing references)
			//IL_0483: Expected O, but got Unknown
			//IL_0484: Unknown result type (might be due to invalid IL or missing references)
			//IL_048e: Expected O, but got Unknown
			optDMS = new RadioButton();
			optDeg = new RadioButton();
			label8 = new Label();
			label7 = new Label();
			txtPA = new TextBox();
			txtSep = new TextBox();
			label6 = new Label();
			label5 = new Label();
			label4 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			DS2 = new NumericUpDown();
			DM2 = new NumericUpDown();
			DD2 = new NumericUpDown();
			RAS2 = new NumericUpDown();
			RAM2 = new NumericUpDown();
			RAH2 = new NumericUpDown();
			DS1 = new NumericUpDown();
			DM1 = new NumericUpDown();
			DD1 = new NumericUpDown();
			RAS1 = new NumericUpDown();
			RAM1 = new NumericUpDown();
			RAH1 = new NumericUpDown();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			cmdHelp = new Button();
			pictureBox1 = new PictureBox();
			panel1 = new Panel();
			cmdDecNS2 = new Button();
			cmdDecNS1 = new Button();
			label13 = new Label();
			panel2 = new Panel();
			lblAZ2 = new Label();
			lblAZ1 = new Label();
			label27 = new Label();
			label19 = new Label();
			cmdLatNS2 = new Button();
			cmdLatNS1 = new Button();
			cmdLongEW2 = new Button();
			cmdLongEW1 = new Button();
			label18 = new Label();
			lblEarthFlattening = new Label();
			lblEarthRadius = new Label();
			label26 = new Label();
			label14 = new Label();
			txtDistance = new TextBox();
			pictureBox2 = new PictureBox();
			LongDeg1 = new NumericUpDown();
			cmdHelp2 = new Button();
			LongMin1 = new NumericUpDown();
			label15 = new Label();
			LongSec1 = new NumericUpDown();
			label16 = new Label();
			LatDeg1 = new NumericUpDown();
			label17 = new Label();
			LatMin1 = new NumericUpDown();
			LatSec1 = new NumericUpDown();
			LongDeg2 = new NumericUpDown();
			LongMin2 = new NumericUpDown();
			LongSec2 = new NumericUpDown();
			label20 = new Label();
			LatDeg2 = new NumericUpDown();
			LatMin2 = new NumericUpDown();
			LatSec2 = new NumericUpDown();
			label25 = new Label();
			label21 = new Label();
			label23 = new Label();
			label22 = new Label();
			label24 = new Label();
			lblConverged = new Label();
			panel3 = new Panel();
			txtEndLat = new TextBox();
			txtEndLong = new TextBox();
			lblEndLat = new Label();
			lblEndLong = new Label();
			txtAzimuth = new TextBox();
			label44 = new Label();
			label28 = new Label();
			pictureBox3 = new PictureBox();
			cmdHelp3 = new Button();
			label29 = new Label();
			cmdLatNS3 = new Button();
			cmdLongEW3 = new Button();
			label30 = new Label();
			label31 = new Label();
			label32 = new Label();
			label33 = new Label();
			label34 = new Label();
			txtDistanceTo = new TextBox();
			LongDeg3 = new NumericUpDown();
			LongMin3 = new NumericUpDown();
			LongSec3 = new NumericUpDown();
			label36 = new Label();
			LatDeg3 = new NumericUpDown();
			label37 = new Label();
			LatMin3 = new NumericUpDown();
			LatSec3 = new NumericUpDown();
			label38 = new Label();
			label39 = new Label();
			label40 = new Label();
			label41 = new Label();
			label42 = new Label();
			label43 = new Label();
			((ISupportInitialize)DS2).BeginInit();
			((ISupportInitialize)DM2).BeginInit();
			((ISupportInitialize)DD2).BeginInit();
			((ISupportInitialize)RAS2).BeginInit();
			((ISupportInitialize)RAM2).BeginInit();
			((ISupportInitialize)RAH2).BeginInit();
			((ISupportInitialize)DS1).BeginInit();
			((ISupportInitialize)DM1).BeginInit();
			((ISupportInitialize)DD1).BeginInit();
			((ISupportInitialize)RAS1).BeginInit();
			((ISupportInitialize)RAM1).BeginInit();
			((ISupportInitialize)RAH1).BeginInit();
			((ISupportInitialize)pictureBox1).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((ISupportInitialize)pictureBox2).BeginInit();
			((ISupportInitialize)LongDeg1).BeginInit();
			((ISupportInitialize)LongMin1).BeginInit();
			((ISupportInitialize)LongSec1).BeginInit();
			((ISupportInitialize)LatDeg1).BeginInit();
			((ISupportInitialize)LatMin1).BeginInit();
			((ISupportInitialize)LatSec1).BeginInit();
			((ISupportInitialize)LongDeg2).BeginInit();
			((ISupportInitialize)LongMin2).BeginInit();
			((ISupportInitialize)LongSec2).BeginInit();
			((ISupportInitialize)LatDeg2).BeginInit();
			((ISupportInitialize)LatMin2).BeginInit();
			((ISupportInitialize)LatSec2).BeginInit();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)pictureBox3).BeginInit();
			((ISupportInitialize)LongDeg3).BeginInit();
			((ISupportInitialize)LongMin3).BeginInit();
			((ISupportInitialize)LongSec3).BeginInit();
			((ISupportInitialize)LatDeg3).BeginInit();
			((ISupportInitialize)LatMin3).BeginInit();
			((ISupportInitialize)LatSec3).BeginInit();
			((Control)this).SuspendLayout();
			((Control)optDMS).set_AutoSize(true);
			optDMS.set_Checked(true);
			((Control)optDMS).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDMS).set_Location(new Point(13, 123));
			((Control)optDMS).set_Name("optDMS");
			((Control)optDMS).set_Size(new Size(49, 17));
			((Control)optDMS).set_TabIndex(25);
			optDMS.set_TabStop(true);
			((Control)optDMS).set_Text("DMS");
			((ButtonBase)optDMS).set_UseVisualStyleBackColor(true);
			optDMS.add_CheckedChanged((EventHandler)optDMS_CheckedChanged);
			((Control)optDeg).set_AutoSize(true);
			((Control)optDeg).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDeg).set_Location(new Point(13, 137));
			((Control)optDeg).set_Name("optDeg");
			((Control)optDeg).set_Size(new Size(45, 17));
			((Control)optDeg).set_TabIndex(28);
			((Control)optDeg).set_Text("Deg");
			((ButtonBase)optDeg).set_UseVisualStyleBackColor(true);
			optDeg.add_CheckedChanged((EventHandler)optDeg_CheckedChanged);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(206, 120));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(21, 13));
			((Control)label8).set_TabIndex(27);
			((Control)label8).set_Text("PA");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(105, 120));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(49, 13));
			((Control)label7).set_TabIndex(26);
			((Control)label7).set_Text("Distance");
			((Control)txtPA).set_Location(new Point(187, 133));
			((Control)txtPA).set_Name("txtPA");
			((Control)txtPA).set_Size(new Size(59, 20));
			((Control)txtPA).set_TabIndex(30);
			txtPA.set_TextAlign((HorizontalAlignment)1);
			((Control)txtSep).set_Location(new Point(91, 133));
			((Control)txtSep).set_Name("txtSep");
			((Control)txtSep).set_Size(new Size(77, 20));
			((Control)txtSep).set_TabIndex(29);
			txtSep.set_TextAlign((HorizontalAlignment)1);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(339, 52));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(12, 13));
			((Control)label6).set_TabIndex(8);
			((Control)label6).set_Text("\"");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(293, 52));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(9, 13));
			((Control)label5).set_TabIndex(7);
			((Control)label5).set_Text("'");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(243, 48));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(13, 13));
			((Control)label4).set_TabIndex(6);
			((Control)label4).set_Text("o");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(131, 49));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(26, 13));
			((Control)label3).set_TabIndex(5);
			((Control)label3).set_Text("Sec");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(88, 49));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(24, 13));
			((Control)label2).set_TabIndex(4);
			((Control)label2).set_Text("Min");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(40, 49));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(18, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("Hr");
			DS2.set_DecimalPlaces(2);
			((Control)DS2).set_Location(new Point(331, 94));
			DS2.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)DS2).set_Name("DS2");
			((Control)DS2).set_Size(new Size(55, 20));
			((Control)DS2).set_TabIndex(24);
			((UpDownBase)DS2).set_TextAlign((HorizontalAlignment)1);
			DS2.add_ValueChanged((EventHandler)DS2_ValueChanged);
			((Control)DS2).add_Enter((EventHandler)DS2_Enter);
			((Control)DM2).set_Location(new Point(283, 94));
			DM2.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)DM2).set_Name("DM2");
			((Control)DM2).set_Size(new Size(42, 20));
			((Control)DM2).set_TabIndex(23);
			((UpDownBase)DM2).set_TextAlign((HorizontalAlignment)1);
			DM2.add_ValueChanged((EventHandler)DM2_ValueChanged);
			((Control)DM2).add_Enter((EventHandler)DM2_Enter);
			((Control)DD2).set_Location(new Point(235, 94));
			DD2.set_Maximum(new decimal(new int[4] { 89, 0, 0, 0 }));
			((Control)DD2).set_Name("DD2");
			((Control)DD2).set_Size(new Size(42, 20));
			((Control)DD2).set_TabIndex(22);
			((UpDownBase)DD2).set_TextAlign((HorizontalAlignment)1);
			DD2.add_ValueChanged((EventHandler)DD2_ValueChanged);
			((Control)DD2).add_Enter((EventHandler)DD2_Enter);
			RAS2.set_DecimalPlaces(3);
			((Control)RAS2).set_Location(new Point(128, 94));
			RAS2.set_Maximum(new decimal(new int[4] { 59999, 0, 0, 196608 }));
			((Control)RAS2).set_Name("RAS2");
			((Control)RAS2).set_Size(new Size(52, 20));
			((Control)RAS2).set_TabIndex(20);
			((UpDownBase)RAS2).set_TextAlign((HorizontalAlignment)1);
			RAS2.add_ValueChanged((EventHandler)RAS2_ValueChanged);
			((Control)RAS2).add_Enter((EventHandler)RAS2_Enter);
			((Control)RAM2).set_Location(new Point(80, 94));
			RAM2.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)RAM2).set_Name("RAM2");
			((Control)RAM2).set_Size(new Size(42, 20));
			((Control)RAM2).set_TabIndex(19);
			((UpDownBase)RAM2).set_TextAlign((HorizontalAlignment)1);
			RAM2.add_ValueChanged((EventHandler)RAM2_ValueChanged);
			((Control)RAM2).add_Enter((EventHandler)RAM2_Enter);
			((Control)RAH2).set_Location(new Point(32, 94));
			RAH2.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)RAH2).set_Name("RAH2");
			((Control)RAH2).set_Size(new Size(42, 20));
			((Control)RAH2).set_TabIndex(18);
			((UpDownBase)RAH2).set_TextAlign((HorizontalAlignment)1);
			RAH2.add_ValueChanged((EventHandler)RAH2_ValueChanged);
			((Control)RAH2).add_Enter((EventHandler)RAH2_Enter);
			DS1.set_DecimalPlaces(2);
			((Control)DS1).set_Location(new Point(331, 63));
			DS1.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)DS1).set_Name("DS1");
			((Control)DS1).set_Size(new Size(55, 20));
			((Control)DS1).set_TabIndex(16);
			((UpDownBase)DS1).set_TextAlign((HorizontalAlignment)1);
			DS1.add_ValueChanged((EventHandler)DS1_ValueChanged);
			((Control)DS1).add_Enter((EventHandler)DS1_Enter);
			((Control)DM1).set_Location(new Point(283, 63));
			DM1.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)DM1).set_Name("DM1");
			((Control)DM1).set_Size(new Size(42, 20));
			((Control)DM1).set_TabIndex(15);
			((UpDownBase)DM1).set_TextAlign((HorizontalAlignment)1);
			DM1.add_ValueChanged((EventHandler)DM1_ValueChanged);
			((Control)DM1).add_Enter((EventHandler)DM1_Enter);
			((Control)DD1).set_Location(new Point(235, 63));
			DD1.set_Maximum(new decimal(new int[4] { 89, 0, 0, 0 }));
			((Control)DD1).set_Name("DD1");
			((Control)DD1).set_Size(new Size(42, 20));
			((Control)DD1).set_TabIndex(14);
			((UpDownBase)DD1).set_TextAlign((HorizontalAlignment)1);
			DD1.add_ValueChanged((EventHandler)DD1_ValueChanged);
			((Control)DD1).add_Enter((EventHandler)DD1_Enter);
			RAS1.set_DecimalPlaces(3);
			((Control)RAS1).set_Location(new Point(128, 63));
			RAS1.set_Maximum(new decimal(new int[4] { 59999, 0, 0, 196608 }));
			((Control)RAS1).set_Name("RAS1");
			((Control)RAS1).set_Size(new Size(52, 20));
			((Control)RAS1).set_TabIndex(12);
			((UpDownBase)RAS1).set_TextAlign((HorizontalAlignment)1);
			RAS1.add_ValueChanged((EventHandler)RAS1_ValueChanged);
			((Control)RAS1).add_Enter((EventHandler)RAS1_Enter);
			((Control)RAM1).set_Location(new Point(80, 63));
			RAM1.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)RAM1).set_Name("RAM1");
			((Control)RAM1).set_Size(new Size(42, 20));
			((Control)RAM1).set_TabIndex(11);
			((UpDownBase)RAM1).set_TextAlign((HorizontalAlignment)1);
			RAM1.add_ValueChanged((EventHandler)RAM1_ValueChanged);
			((Control)RAM1).add_Enter((EventHandler)RAM1_Enter);
			((Control)RAH1).set_Location(new Point(32, 63));
			RAH1.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)RAH1).set_Name("RAH1");
			((Control)RAH1).set_Size(new Size(42, 20));
			((Control)RAH1).set_TabIndex(10);
			((UpDownBase)RAH1).set_TextAlign((HorizontalAlignment)1);
			RAH1.add_ValueChanged((EventHandler)RAH1_ValueChanged);
			((Control)RAH1).add_Enter((EventHandler)RAH1_Enter);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(58, 32));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(99, 13));
			((Control)label9).set_TabIndex(1);
			((Control)label9).set_Text("Right Ascension");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(270, 34));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(71, 13));
			((Control)label10).set_TabIndex(2);
			((Control)label10).set_Text("Declination");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(7, 66));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(22, 13));
			((Control)label11).set_TabIndex(9);
			((Control)label11).set_Text("#1");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(8, 97));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(22, 13));
			((Control)label12).set_TabIndex(17);
			((Control)label12).set_Text("#2");
			((Control)cmdHelp).set_Location(new Point(284, 129));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(53, 23));
			((Control)cmdHelp).set_TabIndex(31);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			pictureBox1.set_Image((Image)Resources.help);
			((Control)pictureBox1).set_Location(new Point(288, 133));
			((Control)pictureBox1).set_Name("pictureBox1");
			((Control)pictureBox1).set_Size(new Size(16, 15));
			pictureBox1.set_SizeMode((PictureBoxSizeMode)1);
			pictureBox1.set_TabIndex(55);
			pictureBox1.set_TabStop(false);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)cmdDecNS2);
			((Control)panel1).get_Controls().Add((Control)(object)cmdDecNS1);
			((Control)panel1).get_Controls().Add((Control)(object)label13);
			((Control)panel1).get_Controls().Add((Control)(object)txtSep);
			((Control)panel1).get_Controls().Add((Control)(object)pictureBox1);
			((Control)panel1).get_Controls().Add((Control)(object)RAH1);
			((Control)panel1).get_Controls().Add((Control)(object)cmdHelp);
			((Control)panel1).get_Controls().Add((Control)(object)RAM1);
			((Control)panel1).get_Controls().Add((Control)(object)label12);
			((Control)panel1).get_Controls().Add((Control)(object)RAS1);
			((Control)panel1).get_Controls().Add((Control)(object)label11);
			((Control)panel1).get_Controls().Add((Control)(object)DD1);
			((Control)panel1).get_Controls().Add((Control)(object)label10);
			((Control)panel1).get_Controls().Add((Control)(object)DM1);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).get_Controls().Add((Control)(object)DS1);
			((Control)panel1).get_Controls().Add((Control)(object)optDMS);
			((Control)panel1).get_Controls().Add((Control)(object)RAH2);
			((Control)panel1).get_Controls().Add((Control)(object)optDeg);
			((Control)panel1).get_Controls().Add((Control)(object)RAM2);
			((Control)panel1).get_Controls().Add((Control)(object)label8);
			((Control)panel1).get_Controls().Add((Control)(object)RAS2);
			((Control)panel1).get_Controls().Add((Control)(object)label7);
			((Control)panel1).get_Controls().Add((Control)(object)DD2);
			((Control)panel1).get_Controls().Add((Control)(object)txtPA);
			((Control)panel1).get_Controls().Add((Control)(object)DM2);
			((Control)panel1).get_Controls().Add((Control)(object)DS2);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).get_Controls().Add((Control)(object)label2);
			((Control)panel1).get_Controls().Add((Control)(object)label4);
			((Control)panel1).get_Controls().Add((Control)(object)label3);
			((Control)panel1).get_Controls().Add((Control)(object)label6);
			((Control)panel1).get_Controls().Add((Control)(object)label5);
			((Control)panel1).set_Location(new Point(9, 12));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(396, 168));
			((Control)panel1).set_TabIndex(0);
			((ButtonBase)cmdDecNS2).set_FlatStyle((FlatStyle)1);
			((Control)cmdDecNS2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDecNS2).set_Location(new Point(218, 94));
			((Control)cmdDecNS2).set_Name("cmdDecNS2");
			((Control)cmdDecNS2).set_Size(new Size(17, 20));
			((Control)cmdDecNS2).set_TabIndex(21);
			((Control)cmdDecNS2).set_Text("+");
			((ButtonBase)cmdDecNS2).set_UseVisualStyleBackColor(true);
			((Control)cmdDecNS2).add_Click((EventHandler)cmdDecNS2_Click);
			((ButtonBase)cmdDecNS1).set_FlatStyle((FlatStyle)1);
			((Control)cmdDecNS1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDecNS1).set_Location(new Point(218, 63));
			((Control)cmdDecNS1).set_Name("cmdDecNS1");
			((Control)cmdDecNS1).set_Size(new Size(17, 20));
			((Control)cmdDecNS1).set_TabIndex(13);
			((Control)cmdDecNS1).set_Text("+");
			((ButtonBase)cmdDecNS1).set_UseVisualStyleBackColor(true);
			((Control)cmdDecNS1).add_Click((EventHandler)cmdDecNS1_Click);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(10, 0));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(313, 17));
			((Control)label13).set_TabIndex(0);
			((Control)label13).set_Text("1.  Distance between two points in the sky");
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).get_Controls().Add((Control)(object)lblAZ2);
			((Control)panel2).get_Controls().Add((Control)(object)lblAZ1);
			((Control)panel2).get_Controls().Add((Control)(object)label27);
			((Control)panel2).get_Controls().Add((Control)(object)label19);
			((Control)panel2).get_Controls().Add((Control)(object)cmdLatNS2);
			((Control)panel2).get_Controls().Add((Control)(object)cmdLatNS1);
			((Control)panel2).get_Controls().Add((Control)(object)cmdLongEW2);
			((Control)panel2).get_Controls().Add((Control)(object)cmdLongEW1);
			((Control)panel2).get_Controls().Add((Control)(object)label18);
			((Control)panel2).get_Controls().Add((Control)(object)lblEarthFlattening);
			((Control)panel2).get_Controls().Add((Control)(object)lblEarthRadius);
			((Control)panel2).get_Controls().Add((Control)(object)label26);
			((Control)panel2).get_Controls().Add((Control)(object)label14);
			((Control)panel2).get_Controls().Add((Control)(object)txtDistance);
			((Control)panel2).get_Controls().Add((Control)(object)pictureBox2);
			((Control)panel2).get_Controls().Add((Control)(object)LongDeg1);
			((Control)panel2).get_Controls().Add((Control)(object)cmdHelp2);
			((Control)panel2).get_Controls().Add((Control)(object)LongMin1);
			((Control)panel2).get_Controls().Add((Control)(object)label15);
			((Control)panel2).get_Controls().Add((Control)(object)LongSec1);
			((Control)panel2).get_Controls().Add((Control)(object)label16);
			((Control)panel2).get_Controls().Add((Control)(object)LatDeg1);
			((Control)panel2).get_Controls().Add((Control)(object)label17);
			((Control)panel2).get_Controls().Add((Control)(object)LatMin1);
			((Control)panel2).get_Controls().Add((Control)(object)LatSec1);
			((Control)panel2).get_Controls().Add((Control)(object)LongDeg2);
			((Control)panel2).get_Controls().Add((Control)(object)LongMin2);
			((Control)panel2).get_Controls().Add((Control)(object)LongSec2);
			((Control)panel2).get_Controls().Add((Control)(object)label20);
			((Control)panel2).get_Controls().Add((Control)(object)LatDeg2);
			((Control)panel2).get_Controls().Add((Control)(object)LatMin2);
			((Control)panel2).get_Controls().Add((Control)(object)LatSec2);
			((Control)panel2).get_Controls().Add((Control)(object)label25);
			((Control)panel2).get_Controls().Add((Control)(object)label21);
			((Control)panel2).get_Controls().Add((Control)(object)label23);
			((Control)panel2).get_Controls().Add((Control)(object)label22);
			((Control)panel2).get_Controls().Add((Control)(object)label24);
			((Control)panel2).get_Controls().Add((Control)(object)lblConverged);
			((Control)panel2).set_Location(new Point(9, 195));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(396, 193));
			((Control)panel2).set_TabIndex(1);
			((Control)lblAZ2).set_AutoSize(true);
			((Control)lblAZ2).set_Location(new Point(257, 168));
			((Control)lblAZ2).set_Name("lblAZ2");
			((Control)lblAZ2).set_Size(new Size(61, 13));
			((Control)lblAZ2).set_TabIndex(35);
			((Control)lblAZ2).set_Text("Azimuth@2");
			((Control)lblAZ1).set_AutoSize(true);
			((Control)lblAZ1).set_Location(new Point(173, 168));
			((Control)lblAZ1).set_Name("lblAZ1");
			((Control)lblAZ1).set_Size(new Size(61, 13));
			((Control)lblAZ1).set_TabIndex(34);
			((Control)lblAZ1).set_Text("Azimuth@1");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Location(new Point(322, 44));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(53, 13));
			((Control)label27).set_TabIndex(4);
			((Control)label27).set_Text("{WGS84}");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(113, 19));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(169, 13));
			((Control)label19).set_TabIndex(1);
			((Control)label19).set_Text("{ using Vincenty's formulae }");
			((ButtonBase)cmdLatNS2).set_FlatStyle((FlatStyle)1);
			((Control)cmdLatNS2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdLatNS2).set_Location(new Point(215, 127));
			((Control)cmdLatNS2).set_Name("cmdLatNS2");
			((Control)cmdLatNS2).set_Size(new Size(17, 20));
			((Control)cmdLatNS2).set_TabIndex(27);
			((Control)cmdLatNS2).set_Text("+");
			((ButtonBase)cmdLatNS2).set_UseVisualStyleBackColor(true);
			((Control)cmdLatNS2).add_Click((EventHandler)cmdLatNS2_Click);
			((ButtonBase)cmdLatNS1).set_FlatStyle((FlatStyle)1);
			((Control)cmdLatNS1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdLatNS1).set_Location(new Point(215, 96));
			((Control)cmdLatNS1).set_Name("cmdLatNS1");
			((Control)cmdLatNS1).set_Size(new Size(17, 20));
			((Control)cmdLatNS1).set_TabIndex(18);
			((Control)cmdLatNS1).set_Text("+");
			((ButtonBase)cmdLatNS1).set_UseVisualStyleBackColor(true);
			((Control)cmdLatNS1).add_Click((EventHandler)cmdLatNS1_Click);
			((ButtonBase)cmdLongEW2).set_FlatStyle((FlatStyle)1);
			((Control)cmdLongEW2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdLongEW2).set_Location(new Point(31, 127));
			((Control)cmdLongEW2).set_Name("cmdLongEW2");
			((Control)cmdLongEW2).set_Size(new Size(17, 20));
			((Control)cmdLongEW2).set_TabIndex(23);
			((Control)cmdLongEW2).set_Text("+");
			((ButtonBase)cmdLongEW2).set_UseVisualStyleBackColor(true);
			((Control)cmdLongEW2).add_Click((EventHandler)cmdLongEW2_Click);
			((ButtonBase)cmdLongEW1).set_FlatStyle((FlatStyle)1);
			((Control)cmdLongEW1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdLongEW1).set_Location(new Point(31, 96));
			((Control)cmdLongEW1).set_Name("cmdLongEW1");
			((Control)cmdLongEW1).set_Size(new Size(17, 20));
			((Control)cmdLongEW1).set_TabIndex(14);
			((Control)cmdLongEW1).set_Text("+");
			((ButtonBase)cmdLongEW1).set_UseVisualStyleBackColor(true);
			((Control)cmdLongEW1).add_Click((EventHandler)cmdLongEW1_Click);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(35, 67));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(164, 13));
			((Control)label18).set_TabIndex(5);
			((Control)label18).set_Text("Longitude  { -180 <=> 180 }");
			((Control)lblEarthFlattening).set_AutoSize(true);
			((Control)lblEarthFlattening).set_Location(new Point(163, 44));
			((Control)lblEarthFlattening).set_Name("lblEarthFlattening");
			((Control)lblEarthFlattening).set_Size(new Size(147, 13));
			((Control)lblEarthFlattening).set_TabIndex(3);
			((Control)lblEarthFlattening).set_Text("Earth's flattening = 1/298.257");
			((Control)lblEarthRadius).set_AutoSize(true);
			((Control)lblEarthRadius).set_Location(new Point(19, 44));
			((Control)lblEarthRadius).set_Name("lblEarthRadius");
			((Control)lblEarthRadius).set_Size(new Size(132, 13));
			((Control)lblEarthRadius).set_TabIndex(2);
			((Control)lblEarthRadius).set_Text("Earth's radius = 6378137m");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Location(new Point(56, 81));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(13, 13));
			((Control)label26).set_TabIndex(7);
			((Control)label26).set_Text("o");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(10, 0));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(333, 17));
			((Control)label14).set_TabIndex(0);
			((Control)label14).set_Text("2.  Distance between two points on the Earth");
			((Control)txtDistance).set_Location(new Point(11, 166));
			((Control)txtDistance).set_Name("txtDistance");
			((Control)txtDistance).set_Size(new Size(89, 20));
			((Control)txtDistance).set_TabIndex(32);
			txtDistance.set_TextAlign((HorizontalAlignment)1);
			pictureBox2.set_Image((Image)Resources.help);
			((Control)pictureBox2).set_Location(new Point(336, 166));
			((Control)pictureBox2).set_Name("pictureBox2");
			((Control)pictureBox2).set_Size(new Size(16, 15));
			pictureBox2.set_SizeMode((PictureBoxSizeMode)1);
			pictureBox2.set_TabIndex(55);
			pictureBox2.set_TabStop(false);
			((Control)LongDeg1).set_Location(new Point(48, 96));
			LongDeg1.set_Maximum(new decimal(new int[4] { 179, 0, 0, 0 }));
			((Control)LongDeg1).set_Name("LongDeg1");
			((Control)LongDeg1).set_Size(new Size(42, 20));
			((Control)LongDeg1).set_TabIndex(15);
			((UpDownBase)LongDeg1).set_TextAlign((HorizontalAlignment)1);
			LongDeg1.set_Value(new decimal(new int[4] { 144, 0, 0, 0 }));
			LongDeg1.add_ValueChanged((EventHandler)LongDeg1_ValueChanged);
			((Control)LongDeg1).add_Enter((EventHandler)LongDeg1_Enter);
			((Control)cmdHelp2).set_Location(new Point(332, 162));
			((Control)cmdHelp2).set_Name("cmdHelp2");
			((Control)cmdHelp2).set_Size(new Size(53, 23));
			((Control)cmdHelp2).set_TabIndex(36);
			((Control)cmdHelp2).set_Text("Help");
			((ButtonBase)cmdHelp2).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp2).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp2).add_Click((EventHandler)cmdHelp2_Click);
			((Control)LongMin1).set_Location(new Point(96, 96));
			LongMin1.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)LongMin1).set_Name("LongMin1");
			((Control)LongMin1).set_Size(new Size(42, 20));
			((Control)LongMin1).set_TabIndex(16);
			((UpDownBase)LongMin1).set_TextAlign((HorizontalAlignment)1);
			LongMin1.set_Value(new decimal(new int[4] { 25, 0, 0, 0 }));
			LongMin1.add_ValueChanged((EventHandler)LongMin1_ValueChanged);
			((Control)LongMin1).add_Enter((EventHandler)LongMin1_Enter);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(8, 130));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(22, 13));
			((Control)label15).set_TabIndex(22);
			((Control)label15).set_Text("#2");
			LongSec1.set_DecimalPlaces(2);
			((Control)LongSec1).set_Location(new Point(144, 96));
			LongSec1.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)LongSec1).set_Name("LongSec1");
			((Control)LongSec1).set_Size(new Size(52, 20));
			((Control)LongSec1).set_TabIndex(17);
			((UpDownBase)LongSec1).set_TextAlign((HorizontalAlignment)1);
			LongSec1.set_Value(new decimal(new int[4] { 2952, 0, 0, 131072 }));
			LongSec1.add_ValueChanged((EventHandler)LongSec1_ValueChanged);
			((Control)LongSec1).add_Enter((EventHandler)LongSec1_Enter);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(7, 99));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(22, 13));
			((Control)label16).set_TabIndex(13);
			((Control)label16).set_Text("#1");
			((Control)LatDeg1).set_Location(new Point(232, 96));
			LatDeg1.set_Maximum(new decimal(new int[4] { 89, 0, 0, 0 }));
			((Control)LatDeg1).set_Name("LatDeg1");
			((Control)LatDeg1).set_Size(new Size(42, 20));
			((Control)LatDeg1).set_TabIndex(19);
			((UpDownBase)LatDeg1).set_TextAlign((HorizontalAlignment)1);
			LatDeg1.set_Value(new decimal(new int[4] { 37, 0, 0, 0 }));
			LatDeg1.add_ValueChanged((EventHandler)LatDeg1_ValueChanged);
			((Control)LatDeg1).add_Enter((EventHandler)LatDeg1_Enter);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(275, 67));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(53, 13));
			((Control)label17).set_TabIndex(6);
			((Control)label17).set_Text("Latitude");
			((Control)LatMin1).set_Location(new Point(280, 96));
			LatMin1.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)LatMin1).set_Name("LatMin1");
			((Control)LatMin1).set_Size(new Size(42, 20));
			((Control)LatMin1).set_TabIndex(20);
			((UpDownBase)LatMin1).set_TextAlign((HorizontalAlignment)1);
			LatMin1.set_Value(new decimal(new int[4] { 57, 0, 0, 0 }));
			LatMin1.add_ValueChanged((EventHandler)LatMin1_ValueChanged);
			((Control)LatMin1).add_Enter((EventHandler)LatMin1_Enter);
			LatSec1.set_DecimalPlaces(2);
			((Control)LatSec1).set_Location(new Point(328, 96));
			LatSec1.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)LatSec1).set_Name("LatSec1");
			((Control)LatSec1).set_Size(new Size(55, 20));
			((Control)LatSec1).set_TabIndex(21);
			((UpDownBase)LatSec1).set_TextAlign((HorizontalAlignment)1);
			LatSec1.set_Value(new decimal(new int[4] { 372, 0, 0, 131072 }));
			LatSec1.add_ValueChanged((EventHandler)LatSec1_ValueChanged);
			((Control)LatSec1).add_Enter((EventHandler)LatSec1_Enter);
			((Control)LongDeg2).set_Location(new Point(48, 127));
			LongDeg2.set_Maximum(new decimal(new int[4] { 179, 0, 0, 0 }));
			((Control)LongDeg2).set_Name("LongDeg2");
			((Control)LongDeg2).set_Size(new Size(42, 20));
			((Control)LongDeg2).set_TabIndex(24);
			((UpDownBase)LongDeg2).set_TextAlign((HorizontalAlignment)1);
			LongDeg2.set_Value(new decimal(new int[4] { 143, 0, 0, 0 }));
			LongDeg2.add_ValueChanged((EventHandler)LongDeg2_ValueChanged);
			((Control)LongDeg2).add_Enter((EventHandler)LongDeg2_Enter);
			((Control)LongMin2).set_Location(new Point(96, 127));
			LongMin2.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)LongMin2).set_Name("LongMin2");
			((Control)LongMin2).set_Size(new Size(42, 20));
			((Control)LongMin2).set_TabIndex(25);
			((UpDownBase)LongMin2).set_TextAlign((HorizontalAlignment)1);
			LongMin2.set_Value(new decimal(new int[4] { 55, 0, 0, 0 }));
			LongMin2.add_ValueChanged((EventHandler)LongMin2_ValueChanged);
			((Control)LongMin2).add_Enter((EventHandler)LongMin2_Enter);
			LongSec2.set_DecimalPlaces(2);
			((Control)LongSec2).set_Location(new Point(144, 127));
			LongSec2.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)LongSec2).set_Name("LongSec2");
			((Control)LongSec2).set_Size(new Size(52, 20));
			((Control)LongSec2).set_TabIndex(26);
			((UpDownBase)LongSec2).set_TextAlign((HorizontalAlignment)1);
			LongSec2.set_Value(new decimal(new int[4] { 3538, 0, 0, 131072 }));
			LongSec2.add_ValueChanged((EventHandler)LongSec2_ValueChanged);
			((Control)LongSec2).add_Enter((EventHandler)LongSec2_Enter);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(22, 153));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(72, 13));
			((Control)label20).set_TabIndex(31);
			((Control)label20).set_Text("Distance (km)");
			((Control)LatDeg2).set_Location(new Point(232, 127));
			LatDeg2.set_Maximum(new decimal(new int[4] { 89, 0, 0, 0 }));
			((Control)LatDeg2).set_Name("LatDeg2");
			((Control)LatDeg2).set_Size(new Size(42, 20));
			((Control)LatDeg2).set_TabIndex(28);
			((UpDownBase)LatDeg2).set_TextAlign((HorizontalAlignment)1);
			LatDeg2.set_Value(new decimal(new int[4] { 37, 0, 0, 0 }));
			LatDeg2.add_ValueChanged((EventHandler)LatDeg2_ValueChanged);
			((Control)LatDeg2).add_Enter((EventHandler)LatDeg2_Enter);
			((Control)LatMin2).set_Location(new Point(280, 127));
			LatMin2.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)LatMin2).set_Name("LatMin2");
			((Control)LatMin2).set_Size(new Size(42, 20));
			((Control)LatMin2).set_TabIndex(29);
			((UpDownBase)LatMin2).set_TextAlign((HorizontalAlignment)1);
			LatMin2.set_Value(new decimal(new int[4] { 39, 0, 0, 0 }));
			LatMin2.add_ValueChanged((EventHandler)LatMin2_ValueChanged);
			((Control)LatMin2).add_Enter((EventHandler)LatMin2_Enter);
			LatSec2.set_DecimalPlaces(2);
			((Control)LatSec2).set_Location(new Point(328, 127));
			LatSec2.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)LatSec2).set_Name("LatSec2");
			((Control)LatSec2).set_Size(new Size(55, 20));
			((Control)LatSec2).set_TabIndex(30);
			((UpDownBase)LatSec2).set_TextAlign((HorizontalAlignment)1);
			LatSec2.set_Value(new decimal(new int[4] { 1016, 0, 0, 131072 }));
			LatSec2.add_ValueChanged((EventHandler)LatSec2_ValueChanged);
			((Control)LatSec2).add_Enter((EventHandler)LatSec2_Enter);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Location(new Point(240, 81));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(13, 13));
			((Control)label25).set_TabIndex(10);
			((Control)label25).set_Text("o");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Location(new Point(336, 85));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(12, 13));
			((Control)label21).set_TabIndex(12);
			((Control)label21).set_Text("\"");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(290, 85));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(9, 13));
			((Control)label23).set_TabIndex(11);
			((Control)label23).set_Text("'");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(152, 85));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(12, 13));
			((Control)label22).set_TabIndex(9);
			((Control)label22).set_Text("\"");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Location(new Point(106, 85));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(9, 13));
			((Control)label24).set_TabIndex(8);
			((Control)label24).set_Text("'");
			((Control)lblConverged).set_AutoSize(true);
			((Control)lblConverged).set_Location(new Point(99, 170));
			((Control)lblConverged).set_Name("lblConverged");
			((Control)lblConverged).set_Size(new Size(73, 13));
			((Control)lblConverged).set_TabIndex(33);
			((Control)lblConverged).set_Text("(not accurate)");
			((Control)lblConverged).set_Visible(false);
			panel3.set_BorderStyle((BorderStyle)1);
			((Control)panel3).get_Controls().Add((Control)(object)txtEndLat);
			((Control)panel3).get_Controls().Add((Control)(object)txtEndLong);
			((Control)panel3).get_Controls().Add((Control)(object)lblEndLat);
			((Control)panel3).get_Controls().Add((Control)(object)lblEndLong);
			((Control)panel3).get_Controls().Add((Control)(object)txtAzimuth);
			((Control)panel3).get_Controls().Add((Control)(object)label44);
			((Control)panel3).get_Controls().Add((Control)(object)label28);
			((Control)panel3).get_Controls().Add((Control)(object)pictureBox3);
			((Control)panel3).get_Controls().Add((Control)(object)cmdHelp3);
			((Control)panel3).get_Controls().Add((Control)(object)label29);
			((Control)panel3).get_Controls().Add((Control)(object)cmdLatNS3);
			((Control)panel3).get_Controls().Add((Control)(object)cmdLongEW3);
			((Control)panel3).get_Controls().Add((Control)(object)label30);
			((Control)panel3).get_Controls().Add((Control)(object)label31);
			((Control)panel3).get_Controls().Add((Control)(object)label32);
			((Control)panel3).get_Controls().Add((Control)(object)label33);
			((Control)panel3).get_Controls().Add((Control)(object)label34);
			((Control)panel3).get_Controls().Add((Control)(object)txtDistanceTo);
			((Control)panel3).get_Controls().Add((Control)(object)LongDeg3);
			((Control)panel3).get_Controls().Add((Control)(object)LongMin3);
			((Control)panel3).get_Controls().Add((Control)(object)LongSec3);
			((Control)panel3).get_Controls().Add((Control)(object)label36);
			((Control)panel3).get_Controls().Add((Control)(object)LatDeg3);
			((Control)panel3).get_Controls().Add((Control)(object)label37);
			((Control)panel3).get_Controls().Add((Control)(object)LatMin3);
			((Control)panel3).get_Controls().Add((Control)(object)LatSec3);
			((Control)panel3).get_Controls().Add((Control)(object)label38);
			((Control)panel3).get_Controls().Add((Control)(object)label39);
			((Control)panel3).get_Controls().Add((Control)(object)label40);
			((Control)panel3).get_Controls().Add((Control)(object)label41);
			((Control)panel3).get_Controls().Add((Control)(object)label42);
			((Control)panel3).get_Controls().Add((Control)(object)label43);
			((Control)panel3).set_Location(new Point(9, 403));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(396, 193));
			((Control)panel3).set_TabIndex(2);
			((Control)txtEndLat).set_Location(new Point(176, 158));
			((Control)txtEndLat).set_Name("txtEndLat");
			((Control)txtEndLat).set_Size(new Size(81, 20));
			((Control)txtEndLat).set_TabIndex(29);
			((Control)txtEndLat).set_Text("54.927");
			txtEndLat.set_TextAlign((HorizontalAlignment)1);
			((Control)txtEndLong).set_Location(new Point(59, 158));
			((Control)txtEndLong).set_Name("txtEndLong");
			((Control)txtEndLong).set_Size(new Size(78, 20));
			((Control)txtEndLong).set_TabIndex(28);
			((Control)txtEndLong).set_Text("54.927");
			txtEndLong.set_TextAlign((HorizontalAlignment)1);
			((Control)lblEndLat).set_AutoSize(true);
			((Control)lblEndLat).set_Location(new Point(183, 143));
			((Control)lblEndLat).set_Name("lblEndLat");
			((Control)lblEndLat).set_Size(new Size(63, 13));
			((Control)lblEndLat).set_TabIndex(27);
			((Control)lblEndLat).set_Text("End latitude");
			((Control)lblEndLong).set_AutoSize(true);
			((Control)lblEndLong).set_Location(new Point(59, 143));
			((Control)lblEndLong).set_Name("lblEndLong");
			((Control)lblEndLong).set_Size(new Size(76, 13));
			((Control)lblEndLong).set_TabIndex(26);
			((Control)lblEndLong).set_Text("End Longitude");
			((Control)txtAzimuth).set_Location(new Point(280, 122));
			((Control)txtAzimuth).set_Name("txtAzimuth");
			((Control)txtAzimuth).set_Size(new Size(66, 20));
			((Control)txtAzimuth).set_TabIndex(25);
			((Control)txtAzimuth).set_Text("306.868");
			txtAzimuth.set_TextAlign((HorizontalAlignment)1);
			((Control)txtAzimuth).add_TextChanged((EventHandler)txtAzimuth_TextChanged);
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Location(new Point(202, 123));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(73, 13));
			((Control)label44).set_TabIndex(24);
			((Control)label44).set_Text("Azimuth (Deg)");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Location(new Point(322, 44));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(53, 13));
			((Control)label28).set_TabIndex(4);
			((Control)label28).set_Text("{WGS84}");
			pictureBox3.set_Image((Image)Resources.help);
			((Control)pictureBox3).set_Location(new Point(326, 172));
			((Control)pictureBox3).set_Name("pictureBox3");
			((Control)pictureBox3).set_Size(new Size(16, 15));
			pictureBox3.set_SizeMode((PictureBoxSizeMode)1);
			pictureBox3.set_TabIndex(55);
			pictureBox3.set_TabStop(false);
			((Control)cmdHelp3).set_Location(new Point(322, 168));
			((Control)cmdHelp3).set_Name("cmdHelp3");
			((Control)cmdHelp3).set_Size(new Size(53, 23));
			((Control)cmdHelp3).set_TabIndex(30);
			((Control)cmdHelp3).set_Text("Help");
			((ButtonBase)cmdHelp3).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp3).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp3).add_Click((EventHandler)cmdHelp3_Click);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(113, 19));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(169, 13));
			((Control)label29).set_TabIndex(1);
			((Control)label29).set_Text("{ using Vincenty's formulae }");
			((ButtonBase)cmdLatNS3).set_FlatStyle((FlatStyle)1);
			((Control)cmdLatNS3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdLatNS3).set_Location(new Point(215, 96));
			((Control)cmdLatNS3).set_Name("cmdLatNS3");
			((Control)cmdLatNS3).set_Size(new Size(17, 20));
			((Control)cmdLatNS3).set_TabIndex(18);
			((Control)cmdLatNS3).set_Text("+");
			((ButtonBase)cmdLatNS3).set_UseVisualStyleBackColor(true);
			((Control)cmdLatNS3).add_Click((EventHandler)cmdLatNS3_Click);
			((ButtonBase)cmdLongEW3).set_FlatStyle((FlatStyle)1);
			((Control)cmdLongEW3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdLongEW3).set_Location(new Point(31, 96));
			((Control)cmdLongEW3).set_Name("cmdLongEW3");
			((Control)cmdLongEW3).set_Size(new Size(17, 20));
			((Control)cmdLongEW3).set_TabIndex(14);
			((Control)cmdLongEW3).set_Text("+");
			((ButtonBase)cmdLongEW3).set_UseVisualStyleBackColor(true);
			((Control)cmdLongEW3).add_Click((EventHandler)cmdLongEW3_Click);
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(35, 67));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(164, 13));
			((Control)label30).set_TabIndex(5);
			((Control)label30).set_Text("Longitude  { -180 <=> 180 }");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Location(new Point(163, 44));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(147, 13));
			((Control)label31).set_TabIndex(3);
			((Control)label31).set_Text("Earth's flattening = 1/298.257");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Location(new Point(19, 44));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(132, 13));
			((Control)label32).set_TabIndex(2);
			((Control)label32).set_Text("Earth's radius = 6378137m");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Location(new Point(56, 81));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(13, 13));
			((Control)label33).set_TabIndex(7);
			((Control)label33).set_Text("o");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(10, 0));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(337, 17));
			((Control)label34).set_TabIndex(0);
			((Control)label34).set_Text("3.  Location of a point at a specified distance");
			((Control)txtDistanceTo).set_Location(new Point(105, 119));
			((Control)txtDistanceTo).set_Name("txtDistanceTo");
			((Control)txtDistanceTo).set_Size(new Size(70, 20));
			((Control)txtDistanceTo).set_TabIndex(23);
			((Control)txtDistanceTo).set_Text("54.972");
			txtDistanceTo.set_TextAlign((HorizontalAlignment)1);
			((Control)txtDistanceTo).add_TextChanged((EventHandler)txtDistanceTo_TextChanged);
			((Control)LongDeg3).set_Location(new Point(48, 96));
			LongDeg3.set_Maximum(new decimal(new int[4] { 179, 0, 0, 0 }));
			((Control)LongDeg3).set_Name("LongDeg3");
			((Control)LongDeg3).set_Size(new Size(42, 20));
			((Control)LongDeg3).set_TabIndex(15);
			((UpDownBase)LongDeg3).set_TextAlign((HorizontalAlignment)1);
			LongDeg3.set_Value(new decimal(new int[4] { 144, 0, 0, 0 }));
			LongDeg3.add_ValueChanged((EventHandler)LongDeg3_ValueChanged);
			((Control)LongDeg3).add_Enter((EventHandler)LongDeg3_Enter);
			((Control)LongMin3).set_Location(new Point(96, 96));
			LongMin3.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)LongMin3).set_Name("LongMin3");
			((Control)LongMin3).set_Size(new Size(42, 20));
			((Control)LongMin3).set_TabIndex(16);
			((UpDownBase)LongMin3).set_TextAlign((HorizontalAlignment)1);
			LongMin3.set_Value(new decimal(new int[4] { 25, 0, 0, 0 }));
			LongMin3.add_ValueChanged((EventHandler)LongMin3_ValueChanged);
			((Control)LongMin3).add_Enter((EventHandler)LongMin3_Enter);
			LongSec3.set_DecimalPlaces(2);
			((Control)LongSec3).set_Location(new Point(144, 96));
			LongSec3.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)LongSec3).set_Name("LongSec3");
			((Control)LongSec3).set_Size(new Size(52, 20));
			((Control)LongSec3).set_TabIndex(17);
			((UpDownBase)LongSec3).set_TextAlign((HorizontalAlignment)1);
			LongSec3.set_Value(new decimal(new int[4] { 2952, 0, 0, 131072 }));
			LongSec3.add_ValueChanged((EventHandler)LongSec3_ValueChanged);
			((Control)LongSec3).add_Enter((EventHandler)LongSec3_Enter);
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(7, 99));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(22, 13));
			((Control)label36).set_TabIndex(13);
			((Control)label36).set_Text("#1");
			((Control)LatDeg3).set_Location(new Point(232, 96));
			LatDeg3.set_Maximum(new decimal(new int[4] { 89, 0, 0, 0 }));
			((Control)LatDeg3).set_Name("LatDeg3");
			((Control)LatDeg3).set_Size(new Size(42, 20));
			((Control)LatDeg3).set_TabIndex(19);
			((UpDownBase)LatDeg3).set_TextAlign((HorizontalAlignment)1);
			LatDeg3.set_Value(new decimal(new int[4] { 37, 0, 0, 0 }));
			LatDeg3.add_ValueChanged((EventHandler)LatDeg3_ValueChanged);
			((Control)LatDeg3).add_Enter((EventHandler)LatDeg3_Enter);
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(275, 67));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(53, 13));
			((Control)label37).set_TabIndex(6);
			((Control)label37).set_Text("Latitude");
			((Control)LatMin3).set_Location(new Point(280, 96));
			LatMin3.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)LatMin3).set_Name("LatMin3");
			((Control)LatMin3).set_Size(new Size(42, 20));
			((Control)LatMin3).set_TabIndex(20);
			((UpDownBase)LatMin3).set_TextAlign((HorizontalAlignment)1);
			LatMin3.set_Value(new decimal(new int[4] { 57, 0, 0, 0 }));
			LatMin3.add_ValueChanged((EventHandler)LatMin3_ValueChanged);
			((Control)LatMin3).add_Enter((EventHandler)LatMin3_Enter);
			LatSec3.set_DecimalPlaces(2);
			((Control)LatSec3).set_Location(new Point(328, 96));
			LatSec3.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)LatSec3).set_Name("LatSec3");
			((Control)LatSec3).set_Size(new Size(55, 20));
			((Control)LatSec3).set_TabIndex(21);
			((UpDownBase)LatSec3).set_TextAlign((HorizontalAlignment)1);
			LatSec3.set_Value(new decimal(new int[4] { 372, 0, 0, 131072 }));
			LatSec3.add_ValueChanged((EventHandler)LatSec3_ValueChanged);
			((Control)LatSec3).add_Enter((EventHandler)LatSec3_Enter);
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Location(new Point(27, 123));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(72, 13));
			((Control)label38).set_TabIndex(22);
			((Control)label38).set_Text("Distance (km)");
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Location(new Point(240, 81));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(13, 13));
			((Control)label39).set_TabIndex(10);
			((Control)label39).set_Text("o");
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Location(new Point(336, 85));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(12, 13));
			((Control)label40).set_TabIndex(12);
			((Control)label40).set_Text("\"");
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Location(new Point(290, 85));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(9, 13));
			((Control)label41).set_TabIndex(11);
			((Control)label41).set_Text("'");
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Location(new Point(152, 85));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(12, 13));
			((Control)label42).set_TabIndex(9);
			((Control)label42).set_Text("\"");
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Location(new Point(106, 85));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(9, 13));
			((Control)label43).set_TabIndex(8);
			((Control)label43).set_Text("'");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(414, 598));
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("Distances");
			((Control)this).set_Text("Distance between two points");
			((Form)this).add_Load((EventHandler)Distances_Load);
			((ISupportInitialize)DS2).EndInit();
			((ISupportInitialize)DM2).EndInit();
			((ISupportInitialize)DD2).EndInit();
			((ISupportInitialize)RAS2).EndInit();
			((ISupportInitialize)RAM2).EndInit();
			((ISupportInitialize)RAH2).EndInit();
			((ISupportInitialize)DS1).EndInit();
			((ISupportInitialize)DM1).EndInit();
			((ISupportInitialize)DD1).EndInit();
			((ISupportInitialize)RAS1).EndInit();
			((ISupportInitialize)RAM1).EndInit();
			((ISupportInitialize)RAH1).EndInit();
			((ISupportInitialize)pictureBox1).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((ISupportInitialize)pictureBox2).EndInit();
			((ISupportInitialize)LongDeg1).EndInit();
			((ISupportInitialize)LongMin1).EndInit();
			((ISupportInitialize)LongSec1).EndInit();
			((ISupportInitialize)LatDeg1).EndInit();
			((ISupportInitialize)LatMin1).EndInit();
			((ISupportInitialize)LatSec1).EndInit();
			((ISupportInitialize)LongDeg2).EndInit();
			((ISupportInitialize)LongMin2).EndInit();
			((ISupportInitialize)LongSec2).EndInit();
			((ISupportInitialize)LatDeg2).EndInit();
			((ISupportInitialize)LatMin2).EndInit();
			((ISupportInitialize)LatSec2).EndInit();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)pictureBox3).EndInit();
			((ISupportInitialize)LongDeg3).EndInit();
			((ISupportInitialize)LongMin3).EndInit();
			((ISupportInitialize)LongSec3).EndInit();
			((ISupportInitialize)LatDeg3).EndInit();
			((ISupportInitialize)LatMin3).EndInit();
			((ISupportInitialize)LatSec3).EndInit();
			((Control)this).ResumeLayout(false);
		}
	}
}
