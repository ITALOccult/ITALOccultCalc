using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class Conversion : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private bool Changing;

		private bool LatLonChanging;

		private bool FormCreated;

		private Sites S;

		private Sites CurrentSite = new Sites();

		private List<Sites> AllSites = new List<Sites>();

		private IContainer components;

		private GroupBox groupBox1;

		private TextBox txtDay1;

		private TextBox txtMonth1;

		private TextBox txtYear1;

		private TextBox txtJD1;

		private GroupBox groupBox2;

		private TextBox txtDay2;

		private TextBox txtMonth2;

		private TextBox txtYear2;

		private TextBox txtJD2;

		private GroupBox groupBox3;

		private Label label2;

		private Label label1;

		private TextBox txtRS1;

		private TextBox txtRM1;

		private TextBox txtRH1;

		private TextBox txtDS1;

		private TextBox txtDM1;

		private TextBox txtDD1;

		private Label label4;

		private Label label3;

		private Label label5;

		private Label label6;

		private Label label16;

		private Label label15;

		private Label label13;

		private Label label7;

		private Label label8;

		private Label label9;

		private Label label10;

		private TextBox txtRS2;

		private TextBox txtRM2;

		private TextBox txtRH2;

		private TextBox txtDS2;

		private TextBox txtDM2;

		private TextBox txtDD2;

		private Label label11;

		private Label label12;

		private Label label18;

		private Label label17;

		private TextBox txtTo;

		private TextBox txtFrom;

		private GroupBox groupBox4;

		private TextBox txtRevolutions;

		private TextBox txtRadians;

		private TextBox txtDegrees;

		private TextBox txtHours;

		private Label label20;

		private Label label21;

		private Label label22;

		private Label label23;

		private TextBox txtHS;

		private TextBox txtHM;

		private TextBox txtHH;

		private TextBox txtDS3;

		private TextBox txtDM3;

		private TextBox txtDD3;

		private Label label24;

		private Label label25;

		private Label label19;

		private Label label30;

		private Label label29;

		private Label label28;

		private Label label27;

		private Label label26;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private GroupBox groupBox6;

		private TextBox txtUT1;

		private TextBox txtY;

		private TextBox txtX;

		private TextBox txtDay3;

		private TextBox txtMonth3;

		private TextBox txtYear3;

		private Label label37;

		private Label label36;

		private Label label35;

		private Label label40;

		private Label label39;

		private Label label38;

		private TextBox txtJulianYear;

		private TextBox txtBesselianYear;

		private Label label32;

		private Label label31;

		private Label label14;

		private CheckBox chkApparent;

		private Label label33;

		private Label txtConstellation;

		private GroupBox groupBox5;

		private Label label43;

		private Label label42;

		private Label label41;

		private Label label34;

		private TextBox txtLat;

		private TextBox txtLong;

		private TextBox txtDec;

		private TextBox txtRA;

		private RadioButton opt2006;

		private RadioButton opt1976;

		private RadioButton opt2006Ecl;

		private RadioButton opt1976Ecl;

		private Label label44;

		private GroupBox grpHistoricalTimes;

		private Label label45;

		private TextBox txtDay4;

		private TextBox txtMonth4;

		private TextBox txtYear4;

		private Label label57;

		private Label label53;

		private Label label54;

		private Label label55;

		private TextBox txtUTs;

		private TextBox txtUTm;

		private TextBox txtUTh;

		private Label label52;

		private Label label46;

		private TextBox txtLong1S;

		private TextBox txtLong1M;

		private TextBox txtLong1D;

		private Label label50;

		private Label label51;

		private Label label47;

		private Label label48;

		private Label label49;

		private TextBox txtSTs;

		private TextBox txtSTm;

		private TextBox txtSTh;

		private Label label68;

		private Label label65;

		private Label label60;

		private Label label69;

		private Label label64;

		private Label label61;

		private Label label67;

		private Label label66;

		private Label label63;

		private Label label62;

		private Label label59;

		private Label label58;

		private TextBox txtUTday;

		private TextBox txtUTmonth;

		private TextBox txtUTyear;

		private ComboBox cmbSite;

		private ComboBox cmbSiteFiles;

		private Label label56;

		public Conversion()
		{
			InitializeComponent();
		}

		private void Conversion_Load(object sender, EventArgs e)
		{
			//IL_01b0: Unknown result type (might be due to invalid IL or missing references)
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			FormCreated = false;
			TextBox obj = txtDay2;
			string text;
			((Control)txtDay3).set_Text(text = DateTime.Now.Day.ToString());
			((Control)obj).set_Text(text);
			TextBox obj2 = txtMonth2;
			((Control)txtMonth3).set_Text(text = DateTime.Now.Month.ToString());
			((Control)obj2).set_Text(text);
			TextBox obj3 = txtTo;
			((Control)txtYear2).set_Text(text = DateTime.Now.Year.ToString());
			((Control)obj3).set_Text(text);
			((Control)txtYear3).set_Text(DateTime.Now.Year.ToString());
			((Control)txtJD1).set_Text(((Control)txtJD2).get_Text());
			((Control)txtRA).set_Text("0");
			((Control)txtDec).set_Text("10");
			ComputeUT();
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				cmbSiteFiles.get_Items().Add((object)Path.GetFileName(path));
			}
			if (cmbSiteFiles.get_Items().get_Count() <= 0)
			{
				MessageBox.Show("No site files available", "No site files", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			((ListControl)cmbSiteFiles).set_SelectedIndex(0);
			string siteFileSolarTime = Settings.Default.SiteFileSolarTime;
			for (int j = 0; j < cmbSiteFiles.get_Items().get_Count(); j++)
			{
				if (siteFileSolarTime == cmbSiteFiles.get_Items().get_Item(j).ToString())
				{
					((ListControl)cmbSiteFiles).set_SelectedIndex(j);
					break;
				}
			}
			CurrentSite = AllSites[((ListControl)cmbSite).get_SelectedIndex()];
			FormCreated = true;
		}

		private void txtJD1_TextChanged(object sender, EventArgs e)
		{
			int Year = 0;
			int Month = 0;
			double day = 0.0;
			if (double.TryParse(((Control)txtJD1).get_Text(), out var result))
			{
				Utilities.Date_from_JD(result, out Year, out Month, out day);
				((Control)txtYear1).set_Text(Year.ToString());
				((Control)txtMonth1).set_Text(Month.ToString());
				((Control)txtDay1).set_Text(day.ToString());
			}
		}

		private void txtYear2_TextChanged(object sender, EventArgs e)
		{
			DateToJD();
		}

		private void txtMonth2_TextChanged(object sender, EventArgs e)
		{
			DateToJD();
		}

		private void txtDay2_TextChanged(object sender, EventArgs e)
		{
			DateToJD();
		}

		private void DateToJD()
		{
			if (int.TryParse(((Control)txtYear2).get_Text(), out var result) && int.TryParse(((Control)txtMonth2).get_Text(), out var result2) && double.TryParse(((Control)txtDay2).get_Text(), out var result3))
			{
				double num = Utilities.JD_from_Date(result, result2, result3);
				((Control)txtJD2).set_Text(num.ToString());
				double num2 = 2000.0 + (num - 2451545.0) / 365.25;
				if (num2 > 1900.0 || num2 < 2100.0)
				{
					((Control)txtJulianYear).set_Text(string.Format("{0,4:f4}", num2));
				}
				else
				{
					((Control)txtJulianYear).set_Text("----");
				}
				double num3 = Utilities.BesselianYear(num);
				if (num2 > 1600.0 || num2 < 2100.0)
				{
					((Control)txtBesselianYear).set_Text(string.Format("{0,4:f4}", num3));
				}
				else
				{
					((Control)txtBesselianYear).set_Text("----");
				}
			}
		}

		private void txtRH1_TextChanged(object sender, EventArgs e)
		{
			Precess();
		}

		private void txtRM1_TextChanged(object sender, EventArgs e)
		{
			Precess();
		}

		private void txtRS1_TextChanged(object sender, EventArgs e)
		{
			Precess();
		}

		private void txtDD1_TextChanged(object sender, EventArgs e)
		{
			Precess();
		}

		private void textBox2_TextChanged(object sender, EventArgs e)
		{
			Precess();
		}

		private void txtDS1_TextChanged(object sender, EventArgs e)
		{
			Precess();
		}

		private void txtFrom_TextChanged(object sender, EventArgs e)
		{
			if ((((Control)txtFrom).get_Text() != "2000") & (((Control)txtFrom).get_Text() != "1950"))
			{
				((Control)chkApparent).set_Enabled(false);
				chkApparent.set_Checked(false);
			}
			else
			{
				((Control)chkApparent).set_Enabled(true);
			}
			Precess();
		}

		private void txtTo_TextChanged(object sender, EventArgs e)
		{
			Precess();
		}

		private void chkApparent_CheckedChanged(object sender, EventArgs e)
		{
			Precess();
		}

		private void opt1976_CheckedChanged(object sender, EventArgs e)
		{
			Precess();
		}

		private void opt2006_CheckedChanged(object sender, EventArgs e)
		{
			Precess();
		}

		private void Precess()
		{
			if (!int.TryParse(((Control)txtRH1).get_Text(), out var result) || !int.TryParse(((Control)txtRM1).get_Text(), out var result2) || !double.TryParse(((Control)txtRS1).get_Text(), out var result3) || !int.TryParse(((Control)txtDD1).get_Text(), out var result4) || !int.TryParse(((Control)txtDM1).get_Text(), out var result5) || !double.TryParse(((Control)txtDS1).get_Text(), out var result6) || !double.TryParse(((Control)txtFrom).get_Text(), out var result7))
			{
				return;
			}
			int num = (int)Math.Floor(result7);
			if (!double.TryParse(((Control)txtTo).get_Text(), out var result8))
			{
				return;
			}
			int num2 = (int)Math.Floor(result8);
			if (!((Math.Abs(result7) > 9999.0) | (Math.Abs(result8) > 9999.0)))
			{
				double RA = ((double)result + (double)result2 / 60.0 + result3 / 3600.0) * 15.0 / (180.0 / Math.PI);
				double Dec = ((double)Math.Abs(result4) + (double)result5 / 60.0 + result6 / 3600.0) / (180.0 / Math.PI);
				if (((Control)txtDD1).get_Text().Contains("-"))
				{
					Dec = 0.0 - Dec;
				}
				double rA = RA;
				double dec = Dec;
				double jDStart = Utilities.JD_from_Date(num, 1, 1.0) + (result7 - (double)num) * 365.25;
				double num3 = Utilities.JD_from_Date(num2, 1, 1.0) + (result8 - (double)num2) * 365.25;
				if (chkApparent.get_Checked())
				{
					Utilities.ApparentStarPosition(ref RA, ref Dec, 0.0, 0.0, (int)result7, num3, !opt1976.get_Checked());
				}
				else
				{
					Utilities.PrecessStartToEnd(jDStart, num3, !opt1976.get_Checked(), ref RA, ref Dec);
				}
				string text = Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 5, MinutesOnly: false);
				((Control)txtRH2).set_Text(text.Substring(0, 2));
				((Control)txtRM2).set_Text(text.Substring(3, 2));
				((Control)txtRS2).set_Text(text.Substring(6));
				string text2 = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 4, MinutesOnly: false);
				((Control)txtDD2).set_Text(text2.Substring(0, 3));
				((Control)txtDM2).set_Text(text2.Substring(4, 2));
				((Control)txtDS2).set_Text(text2.Substring(7));
				((Control)txtConstellation).set_Text(Utilities.ConstellationFromCoordinates(num, rA, dec));
			}
		}

		private void txtHH1_TextChanged(object sender, EventArgs e)
		{
			double Angle = 0.0;
			if (!Changing && GetHMS(out Angle))
			{
				SetAngles(Angle, 1);
			}
		}

		private void txtHM1_TextChanged(object sender, EventArgs e)
		{
			double Angle = 0.0;
			if (!Changing && GetHMS(out Angle))
			{
				SetAngles(Angle, 1);
			}
		}

		private void txtHS1_TextChanged(object sender, EventArgs e)
		{
			double Angle = 0.0;
			if (!Changing && GetHMS(out Angle))
			{
				SetAngles(Angle, 1);
			}
		}

		private void textDD3_TextChanged(object sender, EventArgs e)
		{
			double Angle = 0.0;
			if (!Changing && GetDMS(out Angle))
			{
				SetAngles(Angle, 2);
			}
		}

		private void txtDM3_TextChanged(object sender, EventArgs e)
		{
			double Angle = 0.0;
			if (!Changing && GetDMS(out Angle))
			{
				SetAngles(Angle, 2);
			}
		}

		private void txtDS3_TextChanged(object sender, EventArgs e)
		{
			double Angle = 0.0;
			if (!Changing && GetDMS(out Angle))
			{
				SetAngles(Angle, 2);
			}
		}

		private void txtDegrees_TextChanged(object sender, EventArgs e)
		{
			if (!Changing)
			{
				double result = 0.0;
				if (double.TryParse(((Control)txtDegrees).get_Text(), out result))
				{
					SetAngles(result, 3);
				}
			}
		}

		private void txtHours_TextChanged(object sender, EventArgs e)
		{
			if (!Changing)
			{
				double result = 0.0;
				if (double.TryParse(((Control)txtHours).get_Text(), out result))
				{
					SetAngles(result * 15.0, 4);
				}
			}
		}

		private void txtRadians_TextChanged(object sender, EventArgs e)
		{
			if (!Changing)
			{
				double result = 0.0;
				if (double.TryParse(((Control)txtRadians).get_Text(), out result))
				{
					SetAngles(result * (180.0 / Math.PI), 5);
				}
			}
		}

		private void txtRevolutions_TextChanged(object sender, EventArgs e)
		{
			if (!Changing)
			{
				double result = 0.0;
				if (double.TryParse(((Control)txtRevolutions).get_Text(), out result))
				{
					SetAngles(result * 360.0, 6);
				}
			}
		}

		private void SetAngles(double Angle, int LineNo)
		{
			Changing = true;
			if (LineNo != 1)
			{
				string text = Utilities.DEGtoDMS(Angle / 15.0, 4, 3, MinutesOnly: false);
				((Control)txtHH).set_Text(text.Substring(0, 4).Trim());
				((Control)txtHM).set_Text(text.Substring(5, 2));
				((Control)txtHS).set_Text(text.Substring(8));
			}
			if (LineNo != 2)
			{
				string text2 = Utilities.DEGtoDMS(Angle, 3, 2, MinutesOnly: false);
				((Control)txtDD3).set_Text(text2.Substring(0, 3).Trim());
				((Control)txtDM3).set_Text(text2.Substring(4, 2));
				((Control)txtDS3).set_Text(text2.Substring(7));
			}
			if (LineNo != 3)
			{
				double num = Angle;
				((Control)txtDegrees).set_Text(num.ToString("0.0000000"));
			}
			if (LineNo != 4)
			{
				double num = Angle / 15.0;
				((Control)txtHours).set_Text(num.ToString("0.0000000"));
			}
			if (LineNo != 5)
			{
				double num = Angle / (180.0 / Math.PI);
				((Control)txtRadians).set_Text(num.ToString("0.00000000"));
			}
			if (LineNo != 6)
			{
				double num = Angle / 360.0;
				((Control)txtRevolutions).set_Text(num.ToString("0.00000000"));
			}
			Changing = false;
		}

		private bool GetHMS(out double Angle)
		{
			Angle = 0.0;
			double result;
			bool num = double.TryParse(((Control)txtHH).get_Text().Replace('-', ' '), out result);
			double result2;
			bool flag = double.TryParse(((Control)txtHM).get_Text(), out result2);
			double result3;
			bool flag2 = double.TryParse(((Control)txtHS).get_Text(), out result3);
			bool num2 = num && flag && flag2;
			if (num2)
			{
				Angle = (Math.Abs(result) + result2 / 60.0 + result3 / 3600.0) * 15.0;
			}
			if (((Control)txtHH).get_Text().Contains("-"))
			{
				Angle = 0.0 - Angle;
			}
			return num2;
		}

		private bool GetDMS(out double Angle)
		{
			double num = 0.0;
			Angle = 0.0;
			double result;
			bool num2 = double.TryParse(((Control)txtDD3).get_Text().Replace('-', ' '), out result);
			double result2;
			bool flag = double.TryParse(((Control)txtDM3).get_Text(), out result2);
			double result3;
			bool flag2 = double.TryParse(((Control)txtDS3).get_Text(), out result3);
			bool num3 = num2 && flag && flag2;
			if (num3)
			{
				num = Math.Abs(result) + result2 / 60.0 + result3 / 3600.0;
			}
			if (((Control)txtDD3).get_Text().Contains("-"))
			{
				num = 0.0 - num;
			}
			Angle = num;
			return num3;
		}

		private void txtJD1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtJD1).SelectAll();
		}

		private void txtRH1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRH1).SelectAll();
		}

		private void txtRM1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRM1).SelectAll();
		}

		private void txtRS1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRS1).SelectAll();
		}

		private void txtDD1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDD1).SelectAll();
		}

		private void txtDM1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDM1).SelectAll();
		}

		private void txtDS1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDS1).SelectAll();
		}

		private void txtFrom_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtFrom).SelectAll();
		}

		private void txtTo_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtTo).SelectAll();
		}

		private void txtYear2_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtYear2).SelectAll();
		}

		private void txtMonth2_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtMonth2).SelectAll();
		}

		private void txtDay2_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDay2).SelectAll();
		}

		private void txtDD3_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDD3).SelectAll();
		}

		private void txtDM3_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDM3).SelectAll();
		}

		private void txtDS3_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDS3).SelectAll();
		}

		private void txtHH_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtHH).SelectAll();
		}

		private void txtHM_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtHM).SelectAll();
		}

		private void txtHS_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtHS).SelectAll();
		}

		private void txtDegrees_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDegrees).SelectAll();
		}

		private void txtHours_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtHours).SelectAll();
		}

		private void txtRadians_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRadians).SelectAll();
		}

		private void txtRevolutions_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRevolutions).SelectAll();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Conversions");
		}

		private void txtYear3_TextChanged(object sender, EventArgs e)
		{
			GetEOPparameters();
		}

		private void txtMonth3_TextChanged(object sender, EventArgs e)
		{
			GetEOPparameters();
		}

		private void txtDay3_TextChanged(object sender, EventArgs e)
		{
			GetEOPparameters();
		}

		private void GetEOPparameters()
		{
			if (int.TryParse(((Control)txtYear3).get_Text(), out var result) && int.TryParse(((Control)txtMonth3).get_Text(), out var result2) && double.TryParse(((Control)txtDay3).get_Text(), out var result3))
			{
				Utilities.EarthOrientationParameters(Utilities.JD_from_Date(result, result2, result3), out var x, out var y, out var dUT);
				((Control)txtX).set_Text(string.Format("{0,1:F2}", x));
				((Control)txtY).set_Text(string.Format("{0,1:F2}", y));
				((Control)txtUT1).set_Text(string.Format("{0,1:F2}", dUT));
			}
		}

		private void txtRA_TextChanged(object sender, EventArgs e)
		{
			if (!LatLonChanging)
			{
				LatLonChanging = true;
				Compute_RA_Dec_Lat_Long(ToEcliptic: true, !opt1976Ecl.get_Checked());
				LatLonChanging = false;
			}
		}

		private void txtDec_TextChanged(object sender, EventArgs e)
		{
			if (!LatLonChanging)
			{
				LatLonChanging = true;
				Compute_RA_Dec_Lat_Long(ToEcliptic: true, !opt1976Ecl.get_Checked());
				LatLonChanging = false;
			}
		}

		private void txtLong_TextChanged(object sender, EventArgs e)
		{
			if (!LatLonChanging)
			{
				LatLonChanging = true;
				Compute_RA_Dec_Lat_Long(ToEcliptic: false, !opt1976Ecl.get_Checked());
				LatLonChanging = false;
			}
		}

		private void txtLat_TextChanged(object sender, EventArgs e)
		{
			if (!LatLonChanging)
			{
				LatLonChanging = true;
				Compute_RA_Dec_Lat_Long(ToEcliptic: false, !opt1976Ecl.get_Checked());
				LatLonChanging = false;
			}
		}

		private void Compute_RA_Dec_Lat_Long(bool ToEcliptic, bool Use2006Not1976)
		{
			double RA = 0.0;
			double Dec = 0.0;
			double result = 0.0;
			double result2 = 0.0;
			double ecliptic = Utilities.Ecliptic2000_deg_1976 / (180.0 / Math.PI);
			if (Use2006Not1976)
			{
				ecliptic = Utilities.Ecliptic2000_deg_2006 / (180.0 / Math.PI);
			}
			if (ToEcliptic)
			{
				double.TryParse(((Control)txtRA).get_Text(), out RA);
				double.TryParse(((Control)txtDec).get_Text(), out Dec);
				Utilities.RA_DEC_to_Long_Lat(RA / (180.0 / Math.PI), Dec / (180.0 / Math.PI), ecliptic, ref result, ref result2);
				((Control)txtLong).set_Text(string.Format("{0,1:f6}", result * (180.0 / Math.PI)));
				((Control)txtLat).set_Text(string.Format("{0,1:f6}", result2 * (180.0 / Math.PI)));
			}
			else
			{
				double.TryParse(((Control)txtLong).get_Text(), out result);
				double.TryParse(((Control)txtLat).get_Text(), out result2);
				Utilities.Long_Lat_to_RA_DEC(result / (180.0 / Math.PI), result2 / (180.0 / Math.PI), ecliptic, ref RA, ref Dec);
				((Control)txtRA).set_Text(string.Format("{0,1:f6}", RA * (180.0 / Math.PI)));
				((Control)txtDec).set_Text(string.Format("{0,1:f6}", Dec * (180.0 / Math.PI)));
			}
		}

		private void opt2006Ecl_CheckedChanged(object sender, EventArgs e)
		{
			if (!LatLonChanging)
			{
				LatLonChanging = true;
				Compute_RA_Dec_Lat_Long(ToEcliptic: true, !opt1976Ecl.get_Checked());
				LatLonChanging = false;
			}
		}

		private void opt1976Ecl_CheckedChanged(object sender, EventArgs e)
		{
			if (!LatLonChanging)
			{
				LatLonChanging = true;
				Compute_RA_Dec_Lat_Long(ToEcliptic: true, !opt1976Ecl.get_Checked());
				LatLonChanging = false;
			}
		}

		private void txtYear4_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtYear4).SelectAll();
		}

		private void txtYear4_TextChanged(object sender, EventArgs e)
		{
			ComputeUT();
		}

		private void txtMonth4_TextChanged(object sender, EventArgs e)
		{
			((TextBoxBase)txtMonth4).SelectAll();
		}

		private void txtMonth4_Enter(object sender, EventArgs e)
		{
			ComputeUT();
		}

		private void txtDay4_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtDay4).SelectAll();
		}

		private void txtDay4_TextChanged(object sender, EventArgs e)
		{
			ComputeUT();
		}

		private void txtLong1D_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLong1D).SelectAll();
		}

		private void txtLong1D_TextChanged(object sender, EventArgs e)
		{
			ComputeUT();
		}

		private void txtLong1M_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLong1M).SelectAll();
		}

		private void txtLong1M_TextChanged(object sender, EventArgs e)
		{
			ComputeUT();
		}

		private void txtLong1S_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLong1S).SelectAll();
		}

		private void txtLong1S_TextChanged(object sender, EventArgs e)
		{
			ComputeUT();
		}

		private void txtSTh_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtSTh).SelectAll();
		}

		private void txtSTh_TextChanged(object sender, EventArgs e)
		{
			ComputeUT();
		}

		private void txtSTm_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtSTm).SelectAll();
		}

		private void txtSTm_TextChanged(object sender, EventArgs e)
		{
			ComputeUT();
		}

		private void txtSTs_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtSTs).SelectAll();
		}

		private void txtSTs_TextChanged(object sender, EventArgs e)
		{
			ComputeUT();
		}

		private void ComputeUT()
		{
			if (!int.TryParse(((Control)txtYear4).get_Text().Trim(), out var result) || !int.TryParse(((Control)txtMonth4).get_Text().Trim(), out var result2) || !double.TryParse(((Control)txtDay4).get_Text().Trim(), out var result3))
			{
				return;
			}
			double num = Utilities.JD_from_Date(result, result2, result3) + 1.0;
			double num2 = Utilities.SiderealTime_deg(num, Apparent: true) / 15.0;
			Utilities.PlanetGeocentric(num, 0, 1E-06, 0, out var RA, out var Dec, out var GeocentricDist);
			RA *= 12.0 / Math.PI;
			double num3 = num2 - RA;
			if (num3 < -14.0)
			{
				num3 += 24.0;
			}
			if (num3 > 14.0)
			{
				num3 -= 24.0;
			}
			double num4 = Utilities.SiderealTime_deg(num + 1.0, Apparent: true) / 15.0;
			Utilities.PlanetGeocentric(num + 1.0, 0, 1E-05, 0, out var RA2, out GeocentricDist, out Dec);
			RA2 *= 12.0 / Math.PI;
			double num5 = num4 - RA2;
			if (num5 < -14.0)
			{
				num5 += 24.0;
			}
			if (num5 > 14.0)
			{
				num5 -= 24.0;
			}
			double num6 = 1.0 - 0.9972696 * (num5 - num3) / 24.0;
			double num7 = 0.0;
			if (!int.TryParse(((Control)txtLong1D).get_Text().Trim().Replace("  ", "")
				.Replace(" ", ""), out var result4) || !int.TryParse(((Control)txtLong1M).get_Text().Trim(), out var result5) || !double.TryParse(((Control)txtLong1S).get_Text().Trim(), out var result6))
			{
				return;
			}
			num7 = (double)Math.Abs(result4) + (double)result5 / 60.0 + result6 / 3600.0;
			if (((Control)txtLong1D).get_Text().Contains("-"))
			{
				num7 = 0.0 - num7;
			}
			double num8 = num3 + num7 / 15.0;
			if (int.TryParse(((Control)txtSTh).get_Text().Trim(), out var result7) && int.TryParse(((Control)txtSTm).get_Text().Trim(), out var result8) && double.TryParse(((Control)txtSTs).get_Text().Trim(), out var result9))
			{
				double num9 = ((double)result7 + (double)result8 / 60.0 + result9 / 3600.0 - num8) * num6;
				if (num9 < 0.0)
				{
					num9 += 24.0;
					num -= 1.0;
				}
				if (num9 >= 24.0)
				{
					num9 -= 24.0;
					num += 1.0;
				}
				Utilities.Date_from_JD(num, out result, out result2, out result3);
				string text = Utilities.DEGtoDMS(num9, 2, 1, MinutesOnly: false);
				((Control)txtUTyear).set_Text(result.ToString());
				((Control)txtUTmonth).set_Text(result2.ToString());
				((Control)txtUTday).set_Text(string.Format("{0,1:f0}", result3));
				((Control)txtUTh).set_Text(text.Substring(0, 2));
				((Control)txtUTm).set_Text(text.Substring(3, 2));
				((Control)txtUTs).set_Text(text.Substring(6));
			}
		}

		private void txtYear4_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtMonth4_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtDay4_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtLong1D_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (char.IsNumber(e.get_KeyChar()) | (e.get_KeyChar() == '-') | (e.get_KeyChar() == '\b'))
			{
				e.set_Handled(false);
			}
			else
			{
				e.set_Handled(true);
			}
		}

		private void txtLong1M_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtLong1S_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtSTh_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtSTm_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void cmbSiteFiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			AllSites.Clear();
			StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\sites\\" + cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()));
			do
			{
				S = new Sites();
				S.Read_SiteFile(streamReader.ReadLine());
				AllSites.Add(S);
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			Sites.SortField = 0;
			AllSites.Sort();
			cmbSite.get_Items().Clear();
			for (int i = 0; i < AllSites.Count; i++)
			{
				cmbSite.get_Items().Add((object)AllSites[i].Name);
				if (AllSites[i].Name == Settings.Default.ObserverSolarTime)
				{
					((ListControl)cmbSite).set_SelectedIndex(i);
				}
			}
			if ((((ListControl)cmbSite).get_SelectedIndex() < 0) & (cmbSite.get_Items().get_Count() > 0))
			{
				((ListControl)cmbSite).set_SelectedIndex(0);
			}
			if (FormCreated)
			{
				Settings.Default.SiteFileSolarTime = cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString();
			}
		}

		private void cmbSite_SelectedIndexChanged(object sender, EventArgs e)
		{
			string text = Utilities.DEGtoDMS(AllSites[((ListControl)cmbSite).get_SelectedIndex()].Longitude, 4, 0, MinutesOnly: false);
			((Control)txtLong1D).set_Text(text.Substring(0, 4).Trim().Replace("  ", "")
				.Replace(" ", ""));
			((Control)txtLong1M).set_Text(text.Substring(5, 2).Trim());
			((Control)txtLong1S).set_Text(text.Substring(8, 2).Trim());
			if (FormCreated)
			{
				Settings.Default.ObserverSolarTime = cmbSite.get_Items().get_Item(((ListControl)cmbSite).get_SelectedIndex()).ToString();
			}
		}

		private void txtSTs_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
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
			//IL_0150: Unknown result type (might be due to invalid IL or missing references)
			//IL_015a: Expected O, but got Unknown
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Expected O, but got Unknown
			//IL_0166: Unknown result type (might be due to invalid IL or missing references)
			//IL_0170: Expected O, but got Unknown
			//IL_0171: Unknown result type (might be due to invalid IL or missing references)
			//IL_017b: Expected O, but got Unknown
			//IL_017c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0186: Expected O, but got Unknown
			//IL_0187: Unknown result type (might be due to invalid IL or missing references)
			//IL_0191: Expected O, but got Unknown
			//IL_0192: Unknown result type (might be due to invalid IL or missing references)
			//IL_019c: Expected O, but got Unknown
			//IL_019d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a7: Expected O, but got Unknown
			//IL_01a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b2: Expected O, but got Unknown
			//IL_01b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01bd: Expected O, but got Unknown
			//IL_01be: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c8: Expected O, but got Unknown
			//IL_01c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d3: Expected O, but got Unknown
			//IL_01d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01de: Expected O, but got Unknown
			//IL_01df: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e9: Expected O, but got Unknown
			//IL_01ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f4: Expected O, but got Unknown
			//IL_01f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ff: Expected O, but got Unknown
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_020a: Expected O, but got Unknown
			//IL_020b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0215: Expected O, but got Unknown
			//IL_0216: Unknown result type (might be due to invalid IL or missing references)
			//IL_0220: Expected O, but got Unknown
			//IL_0221: Unknown result type (might be due to invalid IL or missing references)
			//IL_022b: Expected O, but got Unknown
			//IL_022c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0236: Expected O, but got Unknown
			//IL_0237: Unknown result type (might be due to invalid IL or missing references)
			//IL_0241: Expected O, but got Unknown
			//IL_0242: Unknown result type (might be due to invalid IL or missing references)
			//IL_024c: Expected O, but got Unknown
			//IL_024d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0257: Expected O, but got Unknown
			//IL_0258: Unknown result type (might be due to invalid IL or missing references)
			//IL_0262: Expected O, but got Unknown
			//IL_0263: Unknown result type (might be due to invalid IL or missing references)
			//IL_026d: Expected O, but got Unknown
			//IL_026e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0278: Expected O, but got Unknown
			//IL_0279: Unknown result type (might be due to invalid IL or missing references)
			//IL_0283: Expected O, but got Unknown
			//IL_0284: Unknown result type (might be due to invalid IL or missing references)
			//IL_028e: Expected O, but got Unknown
			//IL_028f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0299: Expected O, but got Unknown
			//IL_029a: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a4: Expected O, but got Unknown
			//IL_02a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_02af: Expected O, but got Unknown
			//IL_02b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ba: Expected O, but got Unknown
			//IL_02bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c5: Expected O, but got Unknown
			//IL_02c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d0: Expected O, but got Unknown
			//IL_02d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02db: Expected O, but got Unknown
			//IL_02dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e6: Expected O, but got Unknown
			//IL_02e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f1: Expected O, but got Unknown
			//IL_02f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fc: Expected O, but got Unknown
			//IL_02fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0307: Expected O, but got Unknown
			//IL_0308: Unknown result type (might be due to invalid IL or missing references)
			//IL_0312: Expected O, but got Unknown
			//IL_0313: Unknown result type (might be due to invalid IL or missing references)
			//IL_031d: Expected O, but got Unknown
			//IL_031e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0328: Expected O, but got Unknown
			//IL_0329: Unknown result type (might be due to invalid IL or missing references)
			//IL_0333: Expected O, but got Unknown
			//IL_0334: Unknown result type (might be due to invalid IL or missing references)
			//IL_033e: Expected O, but got Unknown
			//IL_033f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0349: Expected O, but got Unknown
			//IL_034a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0354: Expected O, but got Unknown
			//IL_0355: Unknown result type (might be due to invalid IL or missing references)
			//IL_035f: Expected O, but got Unknown
			//IL_0360: Unknown result type (might be due to invalid IL or missing references)
			//IL_036a: Expected O, but got Unknown
			//IL_036b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0375: Expected O, but got Unknown
			//IL_0376: Unknown result type (might be due to invalid IL or missing references)
			//IL_0380: Expected O, but got Unknown
			//IL_0381: Unknown result type (might be due to invalid IL or missing references)
			//IL_038b: Expected O, but got Unknown
			//IL_038c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0396: Expected O, but got Unknown
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a1: Expected O, but got Unknown
			//IL_03a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ac: Expected O, but got Unknown
			//IL_03ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b7: Expected O, but got Unknown
			//IL_03b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c2: Expected O, but got Unknown
			//IL_03c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03cd: Expected O, but got Unknown
			//IL_03ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d8: Expected O, but got Unknown
			//IL_03d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e3: Expected O, but got Unknown
			//IL_03e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ee: Expected O, but got Unknown
			//IL_03ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f9: Expected O, but got Unknown
			//IL_03fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0404: Expected O, but got Unknown
			//IL_0405: Unknown result type (might be due to invalid IL or missing references)
			//IL_040f: Expected O, but got Unknown
			//IL_0410: Unknown result type (might be due to invalid IL or missing references)
			//IL_041a: Expected O, but got Unknown
			//IL_041b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0425: Expected O, but got Unknown
			//IL_0426: Unknown result type (might be due to invalid IL or missing references)
			//IL_0430: Expected O, but got Unknown
			//IL_0431: Unknown result type (might be due to invalid IL or missing references)
			//IL_043b: Expected O, but got Unknown
			//IL_043c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0446: Expected O, but got Unknown
			//IL_0447: Unknown result type (might be due to invalid IL or missing references)
			//IL_0451: Expected O, but got Unknown
			//IL_0452: Unknown result type (might be due to invalid IL or missing references)
			//IL_045c: Expected O, but got Unknown
			//IL_045d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0467: Expected O, but got Unknown
			//IL_0468: Unknown result type (might be due to invalid IL or missing references)
			//IL_0472: Expected O, but got Unknown
			//IL_0473: Unknown result type (might be due to invalid IL or missing references)
			//IL_047d: Expected O, but got Unknown
			//IL_047e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0488: Expected O, but got Unknown
			//IL_0489: Unknown result type (might be due to invalid IL or missing references)
			//IL_0493: Expected O, but got Unknown
			//IL_0494: Unknown result type (might be due to invalid IL or missing references)
			//IL_049e: Expected O, but got Unknown
			//IL_049f: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a9: Expected O, but got Unknown
			//IL_04aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_04b4: Expected O, but got Unknown
			//IL_04b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04bf: Expected O, but got Unknown
			//IL_04c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ca: Expected O, but got Unknown
			//IL_04cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d5: Expected O, but got Unknown
			//IL_04d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e0: Expected O, but got Unknown
			//IL_04e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04eb: Expected O, but got Unknown
			//IL_04ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f6: Expected O, but got Unknown
			//IL_04f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0501: Expected O, but got Unknown
			//IL_0502: Unknown result type (might be due to invalid IL or missing references)
			//IL_050c: Expected O, but got Unknown
			//IL_050d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0517: Expected O, but got Unknown
			//IL_0518: Unknown result type (might be due to invalid IL or missing references)
			//IL_0522: Expected O, but got Unknown
			//IL_0523: Unknown result type (might be due to invalid IL or missing references)
			//IL_052d: Expected O, but got Unknown
			//IL_052e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0538: Expected O, but got Unknown
			//IL_0539: Unknown result type (might be due to invalid IL or missing references)
			//IL_0543: Expected O, but got Unknown
			//IL_0544: Unknown result type (might be due to invalid IL or missing references)
			//IL_054e: Expected O, but got Unknown
			//IL_054f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0559: Expected O, but got Unknown
			//IL_055a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0564: Expected O, but got Unknown
			//IL_0565: Unknown result type (might be due to invalid IL or missing references)
			//IL_056f: Expected O, but got Unknown
			//IL_0570: Unknown result type (might be due to invalid IL or missing references)
			//IL_057a: Expected O, but got Unknown
			//IL_057b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0585: Expected O, but got Unknown
			//IL_0586: Unknown result type (might be due to invalid IL or missing references)
			//IL_0590: Expected O, but got Unknown
			//IL_0591: Unknown result type (might be due to invalid IL or missing references)
			//IL_059b: Expected O, but got Unknown
			//IL_059c: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a6: Expected O, but got Unknown
			//IL_05a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b1: Expected O, but got Unknown
			//IL_05b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05bc: Expected O, but got Unknown
			//IL_05bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c7: Expected O, but got Unknown
			//IL_05c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d2: Expected O, but got Unknown
			//IL_05d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05dd: Expected O, but got Unknown
			//IL_05de: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e8: Expected O, but got Unknown
			//IL_05e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f3: Expected O, but got Unknown
			//IL_05f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05fe: Expected O, but got Unknown
			//IL_05ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0609: Expected O, but got Unknown
			//IL_060a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0614: Expected O, but got Unknown
			//IL_0615: Unknown result type (might be due to invalid IL or missing references)
			//IL_061f: Expected O, but got Unknown
			//IL_0620: Unknown result type (might be due to invalid IL or missing references)
			//IL_062a: Expected O, but got Unknown
			//IL_062b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0635: Expected O, but got Unknown
			//IL_0636: Unknown result type (might be due to invalid IL or missing references)
			//IL_0640: Expected O, but got Unknown
			//IL_0641: Unknown result type (might be due to invalid IL or missing references)
			//IL_064b: Expected O, but got Unknown
			//IL_064c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0656: Expected O, but got Unknown
			//IL_5a3c: Unknown result type (might be due to invalid IL or missing references)
			//IL_5a46: Expected O, but got Unknown
			//IL_5afe: Unknown result type (might be due to invalid IL or missing references)
			//IL_5b08: Expected O, but got Unknown
			//IL_5bc0: Unknown result type (might be due to invalid IL or missing references)
			//IL_5bca: Expected O, but got Unknown
			//IL_5f07: Unknown result type (might be due to invalid IL or missing references)
			//IL_5f11: Expected O, but got Unknown
			//IL_5fcc: Unknown result type (might be due to invalid IL or missing references)
			//IL_5fd6: Expected O, but got Unknown
			//IL_6091: Unknown result type (might be due to invalid IL or missing references)
			//IL_609b: Expected O, but got Unknown
			//IL_61e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_61ee: Expected O, but got Unknown
			//IL_62a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_62b3: Expected O, but got Unknown
			//IL_636e: Unknown result type (might be due to invalid IL or missing references)
			//IL_6378: Expected O, but got Unknown
			//IL_64c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_64d0: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Conversion));
			groupBox1 = new GroupBox();
			label68 = new Label();
			label65 = new Label();
			label60 = new Label();
			txtDay1 = new TextBox();
			txtMonth1 = new TextBox();
			txtYear1 = new TextBox();
			txtJD1 = new TextBox();
			groupBox2 = new GroupBox();
			label69 = new Label();
			label64 = new Label();
			label61 = new Label();
			label32 = new Label();
			label31 = new Label();
			txtBesselianYear = new TextBox();
			txtJulianYear = new TextBox();
			txtDay2 = new TextBox();
			txtMonth2 = new TextBox();
			txtYear2 = new TextBox();
			txtJD2 = new TextBox();
			groupBox3 = new GroupBox();
			opt2006 = new RadioButton();
			opt1976 = new RadioButton();
			txtConstellation = new Label();
			label33 = new Label();
			chkApparent = new CheckBox();
			label18 = new Label();
			label17 = new Label();
			txtTo = new TextBox();
			txtFrom = new TextBox();
			label16 = new Label();
			label15 = new Label();
			label14 = new Label();
			label13 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			txtRS2 = new TextBox();
			txtRM2 = new TextBox();
			txtRH2 = new TextBox();
			txtDS2 = new TextBox();
			txtDM2 = new TextBox();
			txtDD2 = new TextBox();
			label11 = new Label();
			label12 = new Label();
			label4 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			txtRS1 = new TextBox();
			txtRM1 = new TextBox();
			txtRH1 = new TextBox();
			txtDS1 = new TextBox();
			txtDM1 = new TextBox();
			txtDD1 = new TextBox();
			label5 = new Label();
			label6 = new Label();
			groupBox4 = new GroupBox();
			label30 = new Label();
			label29 = new Label();
			label28 = new Label();
			label27 = new Label();
			label26 = new Label();
			label19 = new Label();
			txtRevolutions = new TextBox();
			txtRadians = new TextBox();
			txtDegrees = new TextBox();
			txtHours = new TextBox();
			label20 = new Label();
			label21 = new Label();
			label22 = new Label();
			txtHS = new TextBox();
			txtHM = new TextBox();
			txtHH = new TextBox();
			txtDS3 = new TextBox();
			txtDM3 = new TextBox();
			txtDD3 = new TextBox();
			label24 = new Label();
			label25 = new Label();
			label23 = new Label();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			groupBox6 = new GroupBox();
			label40 = new Label();
			label39 = new Label();
			label38 = new Label();
			label37 = new Label();
			label36 = new Label();
			label35 = new Label();
			txtUT1 = new TextBox();
			txtY = new TextBox();
			txtX = new TextBox();
			txtDay3 = new TextBox();
			txtMonth3 = new TextBox();
			txtYear3 = new TextBox();
			groupBox5 = new GroupBox();
			label44 = new Label();
			opt2006Ecl = new RadioButton();
			opt1976Ecl = new RadioButton();
			label43 = new Label();
			label42 = new Label();
			label41 = new Label();
			label34 = new Label();
			txtLat = new TextBox();
			txtLong = new TextBox();
			txtDec = new TextBox();
			txtRA = new TextBox();
			grpHistoricalTimes = new GroupBox();
			cmbSite = new ComboBox();
			cmbSiteFiles = new ComboBox();
			label67 = new Label();
			label66 = new Label();
			label63 = new Label();
			label62 = new Label();
			label59 = new Label();
			label58 = new Label();
			txtUTday = new TextBox();
			txtUTmonth = new TextBox();
			txtUTyear = new TextBox();
			label57 = new Label();
			label53 = new Label();
			label54 = new Label();
			label55 = new Label();
			txtUTs = new TextBox();
			txtUTm = new TextBox();
			txtUTh = new TextBox();
			label52 = new Label();
			label46 = new Label();
			txtLong1S = new TextBox();
			txtLong1M = new TextBox();
			txtLong1D = new TextBox();
			label50 = new Label();
			label51 = new Label();
			label47 = new Label();
			label48 = new Label();
			label49 = new Label();
			txtSTs = new TextBox();
			txtSTm = new TextBox();
			txtSTh = new TextBox();
			label45 = new Label();
			txtDay4 = new TextBox();
			txtMonth4 = new TextBox();
			txtYear4 = new TextBox();
			label56 = new Label();
			((Control)groupBox1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)groupBox4).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox6).SuspendLayout();
			((Control)groupBox5).SuspendLayout();
			((Control)grpHistoricalTimes).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)groupBox1).get_Controls().Add((Control)(object)label68);
			((Control)groupBox1).get_Controls().Add((Control)(object)label65);
			((Control)groupBox1).get_Controls().Add((Control)(object)label60);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtDay1);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtMonth1);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtYear1);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtJD1);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(12, 32));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(228, 53));
			((Control)groupBox1).set_TabIndex(0);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("JD to date");
			((Control)label68).set_AutoSize(true);
			((Control)label68).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label68).set_Location(new Point(180, 9));
			((Control)label68).set_Name("label68");
			((Control)label68).set_Size(new Size(24, 13));
			((Control)label68).set_TabIndex(49);
			((Control)label68).set_Text("day");
			((Control)label65).set_AutoSize(true);
			((Control)label65).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label65).set_Location(new Point(145, 9));
			((Control)label65).set_Name("label65");
			((Control)label65).set_Size(new Size(24, 13));
			((Control)label65).set_TabIndex(47);
			((Control)label65).set_Text("mth");
			((Control)label60).set_AutoSize(true);
			((Control)label60).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label60).set_Location(new Point(107, 9));
			((Control)label60).set_Name("label60");
			((Control)label60).set_Size(new Size(27, 13));
			((Control)label60).set_TabIndex(45);
			((Control)label60).set_Text("year");
			((Control)txtDay1).set_BackColor(Color.LightCyan);
			((Control)txtDay1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDay1).set_Location(new Point(177, 24));
			((Control)txtDay1).set_Name("txtDay1");
			((TextBoxBase)txtDay1).set_ReadOnly(true);
			((Control)txtDay1).set_Size(new Size(43, 20));
			((Control)txtDay1).set_TabIndex(3);
			txtDay1.set_TextAlign((HorizontalAlignment)2);
			((Control)txtMonth1).set_BackColor(Color.LightCyan);
			((Control)txtMonth1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMonth1).set_Location(new Point(145, 24));
			((Control)txtMonth1).set_Name("txtMonth1");
			((TextBoxBase)txtMonth1).set_ReadOnly(true);
			((Control)txtMonth1).set_Size(new Size(26, 20));
			((Control)txtMonth1).set_TabIndex(2);
			txtMonth1.set_TextAlign((HorizontalAlignment)2);
			((Control)txtYear1).set_BackColor(Color.LightCyan);
			((Control)txtYear1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtYear1).set_Location(new Point(101, 24));
			((Control)txtYear1).set_Name("txtYear1");
			((TextBoxBase)txtYear1).set_ReadOnly(true);
			((Control)txtYear1).set_Size(new Size(38, 20));
			((Control)txtYear1).set_TabIndex(1);
			txtYear1.set_TextAlign((HorizontalAlignment)2);
			((Control)txtJD1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtJD1).set_Location(new Point(8, 24));
			((Control)txtJD1).set_Name("txtJD1");
			((Control)txtJD1).set_Size(new Size(82, 20));
			((Control)txtJD1).set_TabIndex(0);
			((Control)txtJD1).set_Text("1");
			((Control)txtJD1).add_TextChanged((EventHandler)txtJD1_TextChanged);
			((Control)txtJD1).add_Enter((EventHandler)txtJD1_Enter);
			((Control)groupBox2).get_Controls().Add((Control)(object)label69);
			((Control)groupBox2).get_Controls().Add((Control)(object)label64);
			((Control)groupBox2).get_Controls().Add((Control)(object)label61);
			((Control)groupBox2).get_Controls().Add((Control)(object)label32);
			((Control)groupBox2).get_Controls().Add((Control)(object)label31);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtBesselianYear);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtJulianYear);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtDay2);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtMonth2);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtYear2);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtJD2);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(256, 32));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(232, 111));
			((Control)groupBox2).set_TabIndex(1);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Date to JD  (&& Epoch) ");
			((Control)label69).set_AutoSize(true);
			((Control)label69).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label69).set_Location(new Point(90, 16));
			((Control)label69).set_Name("label69");
			((Control)label69).set_Size(new Size(24, 13));
			((Control)label69).set_TabIndex(49);
			((Control)label69).set_Text("day");
			((Control)label64).set_AutoSize(true);
			((Control)label64).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label64).set_Location(new Point(56, 16));
			((Control)label64).set_Name("label64");
			((Control)label64).set_Size(new Size(24, 13));
			((Control)label64).set_TabIndex(47);
			((Control)label64).set_Text("mth");
			((Control)label61).set_AutoSize(true);
			((Control)label61).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label61).set_Location(new Point(15, 16));
			((Control)label61).set_Name("label61");
			((Control)label61).set_Size(new Size(27, 13));
			((Control)label61).set_TabIndex(45);
			((Control)label61).set_Text("year");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label32).set_Location(new Point(52, 84));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(75, 13));
			((Control)label32).set_TabIndex(6);
			((Control)label32).set_Text("Besselian year");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label31).set_Location(new Point(69, 60));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(57, 13));
			((Control)label31).set_TabIndex(4);
			((Control)label31).set_Text("Julian year");
			((Control)txtBesselianYear).set_BackColor(Color.LightCyan);
			((Control)txtBesselianYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtBesselianYear).set_Location(new Point(129, 80));
			((Control)txtBesselianYear).set_Name("txtBesselianYear");
			((TextBoxBase)txtBesselianYear).set_ReadOnly(true);
			((Control)txtBesselianYear).set_Size(new Size(97, 20));
			((Control)txtBesselianYear).set_TabIndex(7);
			txtBesselianYear.set_TextAlign((HorizontalAlignment)2);
			((Control)txtJulianYear).set_BackColor(Color.LightCyan);
			((Control)txtJulianYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtJulianYear).set_Location(new Point(129, 56));
			((Control)txtJulianYear).set_Name("txtJulianYear");
			((TextBoxBase)txtJulianYear).set_ReadOnly(true);
			((Control)txtJulianYear).set_Size(new Size(97, 20));
			((Control)txtJulianYear).set_TabIndex(5);
			txtJulianYear.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDay2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDay2).set_Location(new Point(87, 31));
			((Control)txtDay2).set_Name("txtDay2");
			((Control)txtDay2).set_Size(new Size(38, 20));
			((Control)txtDay2).set_TabIndex(2);
			((Control)txtDay2).set_Text("1");
			txtDay2.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDay2).add_TextChanged((EventHandler)txtDay2_TextChanged);
			((Control)txtDay2).add_Enter((EventHandler)txtDay2_Enter);
			((Control)txtMonth2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMonth2).set_Location(new Point(55, 31));
			((Control)txtMonth2).set_Name("txtMonth2");
			((Control)txtMonth2).set_Size(new Size(26, 20));
			((Control)txtMonth2).set_TabIndex(1);
			((Control)txtMonth2).set_Text("1");
			txtMonth2.set_TextAlign((HorizontalAlignment)2);
			((Control)txtMonth2).add_TextChanged((EventHandler)txtMonth2_TextChanged);
			((Control)txtMonth2).add_Enter((EventHandler)txtMonth2_Enter);
			((Control)txtYear2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtYear2).set_Location(new Point(8, 31));
			((Control)txtYear2).set_Name("txtYear2");
			((Control)txtYear2).set_Size(new Size(41, 20));
			((Control)txtYear2).set_TabIndex(0);
			((Control)txtYear2).set_Text("2007");
			txtYear2.set_TextAlign((HorizontalAlignment)2);
			((Control)txtYear2).add_TextChanged((EventHandler)txtYear2_TextChanged);
			((Control)txtYear2).add_Enter((EventHandler)txtYear2_Enter);
			((Control)txtJD2).set_BackColor(Color.LightCyan);
			((Control)txtJD2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtJD2).set_Location(new Point(129, 31));
			((Control)txtJD2).set_Name("txtJD2");
			((TextBoxBase)txtJD2).set_ReadOnly(true);
			((Control)txtJD2).set_Size(new Size(97, 20));
			((Control)txtJD2).set_TabIndex(3);
			txtJD2.set_TextAlign((HorizontalAlignment)2);
			((Control)groupBox3).get_Controls().Add((Control)(object)label56);
			((Control)groupBox3).get_Controls().Add((Control)(object)opt2006);
			((Control)groupBox3).get_Controls().Add((Control)(object)opt1976);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtConstellation);
			((Control)groupBox3).get_Controls().Add((Control)(object)label33);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkApparent);
			((Control)groupBox3).get_Controls().Add((Control)(object)label18);
			((Control)groupBox3).get_Controls().Add((Control)(object)label17);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtTo);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtFrom);
			((Control)groupBox3).get_Controls().Add((Control)(object)label16);
			((Control)groupBox3).get_Controls().Add((Control)(object)label15);
			((Control)groupBox3).get_Controls().Add((Control)(object)label14);
			((Control)groupBox3).get_Controls().Add((Control)(object)label13);
			((Control)groupBox3).get_Controls().Add((Control)(object)label7);
			((Control)groupBox3).get_Controls().Add((Control)(object)label8);
			((Control)groupBox3).get_Controls().Add((Control)(object)label9);
			((Control)groupBox3).get_Controls().Add((Control)(object)label10);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtRS2);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtRM2);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtRH2);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtDS2);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtDM2);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtDD2);
			((Control)groupBox3).get_Controls().Add((Control)(object)label11);
			((Control)groupBox3).get_Controls().Add((Control)(object)label12);
			((Control)groupBox3).get_Controls().Add((Control)(object)label4);
			((Control)groupBox3).get_Controls().Add((Control)(object)label3);
			((Control)groupBox3).get_Controls().Add((Control)(object)label2);
			((Control)groupBox3).get_Controls().Add((Control)(object)label1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtRS1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtRM1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtRH1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtDS1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtDM1);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtDD1);
			((Control)groupBox3).get_Controls().Add((Control)(object)label5);
			((Control)groupBox3).get_Controls().Add((Control)(object)label6);
			((Control)groupBox3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox3).set_Location(new Point(23, 102));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(206, 284));
			((Control)groupBox3).set_TabIndex(2);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Precession");
			((Control)opt2006).set_AutoSize(true);
			((Control)opt2006).set_Location(new Point(122, 27));
			((Control)opt2006).set_Name("opt2006");
			((Control)opt2006).set_Size(new Size(53, 17));
			((Control)opt2006).set_TabIndex(36);
			opt2006.set_TabStop(true);
			((Control)opt2006).set_Text("2006");
			((ButtonBase)opt2006).set_UseVisualStyleBackColor(true);
			opt2006.add_CheckedChanged((EventHandler)opt2006_CheckedChanged);
			((Control)opt1976).set_AutoSize(true);
			opt1976.set_Checked(true);
			((Control)opt1976).set_Location(new Point(122, 11));
			((Control)opt1976).set_Name("opt1976");
			((Control)opt1976).set_Size(new Size(53, 17));
			((Control)opt1976).set_TabIndex(35);
			opt1976.set_TabStop(true);
			((Control)opt1976).set_Text("1976");
			((ButtonBase)opt1976).set_UseVisualStyleBackColor(true);
			opt1976.add_CheckedChanged((EventHandler)opt1976_CheckedChanged);
			((Control)txtConstellation).set_AutoSize(true);
			((Control)txtConstellation).set_Location(new Point(96, 260));
			((Control)txtConstellation).set_Name("txtConstellation");
			((Control)txtConstellation).set_Size(new Size(43, 13));
			((Control)txtConstellation).set_TabIndex(34);
			((Control)txtConstellation).set_Text("Const.");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Location(new Point(6, 260));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(92, 13));
			((Control)label33).set_TabIndex(33);
			((Control)label33).set_Text("Constellation : ");
			((Control)chkApparent).set_AutoSize(true);
			chkApparent.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkApparent).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkApparent).set_Location(new Point(4, 144));
			((Control)chkApparent).set_Name("chkApparent");
			((Control)chkApparent).set_Size(new Size(84, 30));
			((Control)chkApparent).set_TabIndex(8);
			((Control)chkApparent).set_Text("=> Apparent\r\nposition");
			((ButtonBase)chkApparent).set_TextAlign(ContentAlignment.BottomRight);
			((ButtonBase)chkApparent).set_UseVisualStyleBackColor(true);
			chkApparent.add_CheckedChanged((EventHandler)chkApparent_CheckedChanged);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(99, 146));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(25, 20));
			((Control)label18).set_TabIndex(24);
			((Control)label18).set_Text("to");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(9, 124));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(114, 20));
			((Control)label17).set_TabIndex(23);
			((Control)label17).set_Text("Precess from");
			label17.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)txtTo).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtTo).set_Location(new Point(126, 148));
			((Control)txtTo).set_Name("txtTo");
			((Control)txtTo).set_Size(new Size(59, 20));
			((Control)txtTo).set_TabIndex(7);
			((Control)txtTo).set_Text("2007");
			((Control)txtTo).add_TextChanged((EventHandler)txtTo_TextChanged);
			((Control)txtTo).add_Enter((EventHandler)txtTo_Enter);
			((Control)txtFrom).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFrom).set_Location(new Point(126, 124));
			((Control)txtFrom).set_Name("txtFrom");
			((Control)txtFrom).set_Size(new Size(59, 20));
			((Control)txtFrom).set_TabIndex(6);
			((Control)txtFrom).set_Text("2000");
			((Control)txtFrom).add_TextChanged((EventHandler)txtFrom_TextChanged);
			((Control)txtFrom).add_Enter((EventHandler)txtFrom_Enter);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(15, 227));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(60, 13));
			((Control)label16).set_TabIndex(29);
			((Control)label16).set_Text("Declination");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(15, 98));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(60, 13));
			((Control)label15).set_TabIndex(19);
			((Control)label15).set_Text("Declination");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(2, 193));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(79, 13));
			((Control)label14).set_TabIndex(25);
			((Control)label14).set_Text("Right Acension");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(2, 65));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(79, 13));
			((Control)label13).set_TabIndex(15);
			((Control)label13).set_Text("Right Acension");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(93, 211));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(13, 13));
			((Control)label7).set_TabIndex(30);
			((Control)label7).set_Text("o");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(155, 175));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(12, 13));
			((Control)label8).set_TabIndex(28);
			((Control)label8).set_Text("s");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(121, 175));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(15, 13));
			((Control)label9).set_TabIndex(27);
			((Control)label9).set_Text("m");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(93, 175));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(13, 13));
			((Control)label10).set_TabIndex(26);
			((Control)label10).set_Text("h");
			((Control)txtRS2).set_BackColor(Color.LightCyan);
			((Control)txtRS2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRS2).set_Location(new Point(148, 190));
			((Control)txtRS2).set_Name("txtRS2");
			((TextBoxBase)txtRS2).set_ReadOnly(true);
			((Control)txtRS2).set_Size(new Size(52, 20));
			((Control)txtRS2).set_TabIndex(11);
			((Control)txtRS2).set_Text("0");
			txtRS2.set_TextAlign((HorizontalAlignment)2);
			((Control)txtRM2).set_BackColor(Color.LightCyan);
			((Control)txtRM2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRM2).set_Location(new Point(116, 190));
			((Control)txtRM2).set_Name("txtRM2");
			((TextBoxBase)txtRM2).set_ReadOnly(true);
			((Control)txtRM2).set_Size(new Size(23, 20));
			((Control)txtRM2).set_TabIndex(10);
			((Control)txtRM2).set_Text("0");
			txtRM2.set_TextAlign((HorizontalAlignment)2);
			((Control)txtRH2).set_BackColor(Color.LightCyan);
			((Control)txtRH2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRH2).set_Location(new Point(86, 190));
			((Control)txtRH2).set_Name("txtRH2");
			((TextBoxBase)txtRH2).set_ReadOnly(true);
			((Control)txtRH2).set_Size(new Size(23, 20));
			((Control)txtRH2).set_TabIndex(9);
			((Control)txtRH2).set_Text("0");
			txtRH2.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDS2).set_BackColor(Color.LightCyan);
			((Control)txtDS2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDS2).set_Location(new Point(148, 224));
			((Control)txtDS2).set_Name("txtDS2");
			((TextBoxBase)txtDS2).set_ReadOnly(true);
			((Control)txtDS2).set_Size(new Size(46, 20));
			((Control)txtDS2).set_TabIndex(14);
			((Control)txtDS2).set_Text("0");
			txtDS2.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDM2).set_BackColor(Color.LightCyan);
			((Control)txtDM2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDM2).set_Location(new Point(116, 224));
			((Control)txtDM2).set_Name("txtDM2");
			((TextBoxBase)txtDM2).set_ReadOnly(true);
			((Control)txtDM2).set_Size(new Size(23, 20));
			((Control)txtDM2).set_TabIndex(13);
			((Control)txtDM2).set_Text("0");
			txtDM2.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDD2).set_BackColor(Color.LightCyan);
			((Control)txtDD2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDD2).set_Location(new Point(81, 224));
			((Control)txtDD2).set_Name("txtDD2");
			((TextBoxBase)txtDD2).set_ReadOnly(true);
			((Control)txtDD2).set_Size(new Size(28, 20));
			((Control)txtDD2).set_TabIndex(12);
			((Control)txtDD2).set_Text("0");
			txtDD2.set_TextAlign((HorizontalAlignment)2);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(127, 216));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(9, 13));
			((Control)label11).set_TabIndex(31);
			((Control)label11).set_Text("'");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(157, 216));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(12, 13));
			((Control)label12).set_TabIndex(32);
			((Control)label12).set_Text("\"");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(93, 82));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(13, 13));
			((Control)label4).set_TabIndex(20);
			((Control)label4).set_Text("o");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(155, 46));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(12, 13));
			((Control)label3).set_TabIndex(18);
			((Control)label3).set_Text("s");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(121, 46));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(15, 13));
			((Control)label2).set_TabIndex(17);
			((Control)label2).set_Text("m");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(93, 46));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(13, 13));
			((Control)label1).set_TabIndex(16);
			((Control)label1).set_Text("h");
			((Control)txtRS1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRS1).set_Location(new Point(148, 61));
			((Control)txtRS1).set_Name("txtRS1");
			((Control)txtRS1).set_Size(new Size(52, 20));
			((Control)txtRS1).set_TabIndex(2);
			((Control)txtRS1).set_Text("0");
			txtRS1.set_TextAlign((HorizontalAlignment)2);
			((Control)txtRS1).add_TextChanged((EventHandler)txtRS1_TextChanged);
			((Control)txtRS1).add_Enter((EventHandler)txtRS1_Enter);
			((Control)txtRM1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRM1).set_Location(new Point(116, 61));
			((Control)txtRM1).set_Name("txtRM1");
			((Control)txtRM1).set_Size(new Size(23, 20));
			((Control)txtRM1).set_TabIndex(1);
			((Control)txtRM1).set_Text("0");
			txtRM1.set_TextAlign((HorizontalAlignment)2);
			((Control)txtRM1).add_TextChanged((EventHandler)txtRM1_TextChanged);
			((Control)txtRM1).add_Enter((EventHandler)txtRM1_Enter);
			((Control)txtRH1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRH1).set_Location(new Point(86, 61));
			((Control)txtRH1).set_Name("txtRH1");
			((Control)txtRH1).set_Size(new Size(23, 20));
			((Control)txtRH1).set_TabIndex(0);
			((Control)txtRH1).set_Text("0");
			txtRH1.set_TextAlign((HorizontalAlignment)2);
			((Control)txtRH1).add_TextChanged((EventHandler)txtRH1_TextChanged);
			((Control)txtRH1).add_Enter((EventHandler)txtRH1_Enter);
			((Control)txtDS1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDS1).set_Location(new Point(148, 95));
			((Control)txtDS1).set_Name("txtDS1");
			((Control)txtDS1).set_Size(new Size(46, 20));
			((Control)txtDS1).set_TabIndex(5);
			((Control)txtDS1).set_Text("0");
			txtDS1.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDS1).add_TextChanged((EventHandler)txtDS1_TextChanged);
			((Control)txtDS1).add_Enter((EventHandler)txtDS1_Enter);
			((Control)txtDM1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDM1).set_Location(new Point(116, 95));
			((Control)txtDM1).set_Name("txtDM1");
			((Control)txtDM1).set_Size(new Size(23, 20));
			((Control)txtDM1).set_TabIndex(4);
			((Control)txtDM1).set_Text("0");
			txtDM1.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDM1).add_TextChanged((EventHandler)textBox2_TextChanged);
			((Control)txtDM1).add_Enter((EventHandler)txtDM1_Enter);
			((Control)txtDD1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDD1).set_Location(new Point(81, 95));
			((Control)txtDD1).set_Name("txtDD1");
			((Control)txtDD1).set_Size(new Size(28, 20));
			((Control)txtDD1).set_TabIndex(3);
			((Control)txtDD1).set_Text("0");
			txtDD1.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDD1).add_TextChanged((EventHandler)txtDD1_TextChanged);
			((Control)txtDD1).add_Enter((EventHandler)txtDD1_Enter);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(127, 87));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(9, 13));
			((Control)label5).set_TabIndex(21);
			((Control)label5).set_Text("'");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(157, 87));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(12, 13));
			((Control)label6).set_TabIndex(22);
			((Control)label6).set_Text("\"");
			((Control)groupBox4).get_Controls().Add((Control)(object)label30);
			((Control)groupBox4).get_Controls().Add((Control)(object)label29);
			((Control)groupBox4).get_Controls().Add((Control)(object)label28);
			((Control)groupBox4).get_Controls().Add((Control)(object)label27);
			((Control)groupBox4).get_Controls().Add((Control)(object)label26);
			((Control)groupBox4).get_Controls().Add((Control)(object)label19);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtRevolutions);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtRadians);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtDegrees);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtHours);
			((Control)groupBox4).get_Controls().Add((Control)(object)label20);
			((Control)groupBox4).get_Controls().Add((Control)(object)label21);
			((Control)groupBox4).get_Controls().Add((Control)(object)label22);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtHS);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtHM);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtHH);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtDS3);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtDM3);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtDD3);
			((Control)groupBox4).get_Controls().Add((Control)(object)label24);
			((Control)groupBox4).get_Controls().Add((Control)(object)label25);
			((Control)groupBox4).get_Controls().Add((Control)(object)label23);
			((Control)groupBox4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox4).set_Location(new Point(271, 154));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(207, 232));
			((Control)groupBox4).set_TabIndex(3);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("Angle conversions");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(12, 201));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(63, 13));
			((Control)label30).set_TabIndex(15);
			((Control)label30).set_Text("Revolutions");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(12, 167));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(46, 13));
			((Control)label29).set_TabIndex(14);
			((Control)label29).set_Text("Radians");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(12, 134));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(35, 13));
			((Control)label28).set_TabIndex(13);
			((Control)label28).set_Text("Hours");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(12, 103));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(47, 13));
			((Control)label27).set_TabIndex(12);
			((Control)label27).set_Text("Degrees");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(12, 29));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(72, 13));
			((Control)label26).set_TabIndex(10);
			((Control)label26).set_Text("Deg, min, sec");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(12, 70));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(75, 13));
			((Control)label19).set_TabIndex(11);
			((Control)label19).set_Text("Hour, min, sec");
			((Control)txtRevolutions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRevolutions).set_Location(new Point(94, 197));
			((Control)txtRevolutions).set_Name("txtRevolutions");
			((Control)txtRevolutions).set_Size(new Size(86, 20));
			((Control)txtRevolutions).set_TabIndex(9);
			((Control)txtRevolutions).set_Text("0");
			((Control)txtRevolutions).add_TextChanged((EventHandler)txtRevolutions_TextChanged);
			((Control)txtRevolutions).add_Enter((EventHandler)txtRevolutions_Enter);
			((Control)txtRadians).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRadians).set_Location(new Point(94, 163));
			((Control)txtRadians).set_Name("txtRadians");
			((Control)txtRadians).set_Size(new Size(86, 20));
			((Control)txtRadians).set_TabIndex(8);
			((Control)txtRadians).set_Text("0");
			((Control)txtRadians).add_TextChanged((EventHandler)txtRadians_TextChanged);
			((Control)txtRadians).add_Enter((EventHandler)txtRadians_Enter);
			((Control)txtDegrees).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDegrees).set_Location(new Point(94, 99));
			((Control)txtDegrees).set_Name("txtDegrees");
			((Control)txtDegrees).set_Size(new Size(86, 20));
			((Control)txtDegrees).set_TabIndex(6);
			((Control)txtDegrees).set_Text("0");
			((Control)txtDegrees).add_TextChanged((EventHandler)txtDegrees_TextChanged);
			((Control)txtDegrees).add_Enter((EventHandler)txtDegrees_Enter);
			((Control)txtHours).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtHours).set_Location(new Point(94, 130));
			((Control)txtHours).set_Name("txtHours");
			((Control)txtHours).set_Size(new Size(86, 20));
			((Control)txtHours).set_TabIndex(7);
			((Control)txtHours).set_Text("0");
			((Control)txtHours).add_TextChanged((EventHandler)txtHours_TextChanged);
			((Control)txtHours).add_Enter((EventHandler)txtHours_Enter);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(105, 12));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(13, 13));
			((Control)label20).set_TabIndex(16);
			((Control)label20).set_Text("o");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(169, 52));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(12, 13));
			((Control)label21).set_TabIndex(21);
			((Control)label21).set_Text("s");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(133, 52));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(15, 13));
			((Control)label22).set_TabIndex(20);
			((Control)label22).set_Text("m");
			((Control)txtHS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtHS).set_Location(new Point(160, 66));
			((Control)txtHS).set_Name("txtHS");
			((Control)txtHS).set_Size(new Size(42, 20));
			((Control)txtHS).set_TabIndex(5);
			((Control)txtHS).set_Text("0");
			txtHS.set_TextAlign((HorizontalAlignment)2);
			((Control)txtHS).add_TextChanged((EventHandler)txtHS1_TextChanged);
			((Control)txtHS).add_Enter((EventHandler)txtHS_Enter);
			((Control)txtHM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtHM).set_Location(new Point(128, 66));
			((Control)txtHM).set_Name("txtHM");
			((Control)txtHM).set_Size(new Size(23, 20));
			((Control)txtHM).set_TabIndex(4);
			((Control)txtHM).set_Text("0");
			txtHM.set_TextAlign((HorizontalAlignment)2);
			((Control)txtHM).add_TextChanged((EventHandler)txtHM1_TextChanged);
			((Control)txtHM).add_Enter((EventHandler)txtHM_Enter);
			((Control)txtHH).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtHH).set_Location(new Point(94, 66));
			((Control)txtHH).set_Name("txtHH");
			((Control)txtHH).set_Size(new Size(27, 20));
			((Control)txtHH).set_TabIndex(3);
			((Control)txtHH).set_Text("0");
			txtHH.set_TextAlign((HorizontalAlignment)2);
			((Control)txtHH).add_TextChanged((EventHandler)txtHH1_TextChanged);
			((Control)txtHH).add_Enter((EventHandler)txtHH_Enter);
			((Control)txtDS3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDS3).set_Location(new Point(160, 25));
			((Control)txtDS3).set_Name("txtDS3");
			((Control)txtDS3).set_Size(new Size(41, 20));
			((Control)txtDS3).set_TabIndex(2);
			((Control)txtDS3).set_Text("0");
			txtDS3.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDS3).add_TextChanged((EventHandler)txtDS3_TextChanged);
			((Control)txtDS3).add_Enter((EventHandler)txtDS3_Enter);
			((Control)txtDM3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDM3).set_Location(new Point(128, 25));
			((Control)txtDM3).set_Name("txtDM3");
			((Control)txtDM3).set_Size(new Size(23, 20));
			((Control)txtDM3).set_TabIndex(1);
			((Control)txtDM3).set_Text("0");
			txtDM3.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDM3).add_TextChanged((EventHandler)txtDM3_TextChanged);
			((Control)txtDM3).add_Enter((EventHandler)txtDM3_Enter);
			((Control)txtDD3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDD3).set_Location(new Point(94, 25));
			((Control)txtDD3).set_Name("txtDD3");
			((Control)txtDD3).set_Size(new Size(28, 20));
			((Control)txtDD3).set_TabIndex(0);
			((Control)txtDD3).set_Text("0");
			txtDD3.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDD3).add_TextChanged((EventHandler)textDD3_TextChanged);
			((Control)txtDD3).add_Enter((EventHandler)txtDD3_Enter);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(139, 17));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(9, 13));
			((Control)label24).set_TabIndex(17);
			((Control)label24).set_Text("'");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(169, 17));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(12, 13));
			((Control)label25).set_TabIndex(18);
			((Control)label25).set_Text("\"");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(105, 52));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(13, 13));
			((Control)label23).set_TabIndex(19);
			((Control)label23).set_Text("h");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(499, 24));
			((Control)menuStrip1).set_TabIndex(5);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)groupBox6).get_Controls().Add((Control)(object)label40);
			((Control)groupBox6).get_Controls().Add((Control)(object)label39);
			((Control)groupBox6).get_Controls().Add((Control)(object)label38);
			((Control)groupBox6).get_Controls().Add((Control)(object)label37);
			((Control)groupBox6).get_Controls().Add((Control)(object)label36);
			((Control)groupBox6).get_Controls().Add((Control)(object)label35);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtUT1);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtY);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtX);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtDay3);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtMonth3);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtYear3);
			((Control)groupBox6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox6).set_Location(new Point(23, 468));
			((Control)groupBox6).set_Name("groupBox6");
			((Control)groupBox6).set_Size(new Size(455, 62));
			((Control)groupBox6).set_TabIndex(4);
			groupBox6.set_TabStop(false);
			((Control)groupBox6).set_Text("Earth Orientation Parameters");
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label40).set_Location(new Point(350, 38));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(60, 13));
			((Control)label40).set_TabIndex(7);
			((Control)label40).set_Text("dUT1 (sec)");
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(151, 38));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(17, 13));
			((Control)label39).set_TabIndex(3);
			((Control)label39).set_Text("x\"");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label38).set_Location(new Point(260, 38));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(17, 13));
			((Control)label38).set_TabIndex(5);
			((Control)label38).set_Text("y\"");
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(368, 20));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(53, 13));
			((Control)label37).set_TabIndex(11);
			((Control)label37).set_Text("UT1-UTC");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(252, 20));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(84, 13));
			((Control)label36).set_TabIndex(10);
			((Control)label36).set_Text("Towards L = -90");
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label35).set_Location(new Point(125, 20));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(108, 13));
			((Control)label35).set_TabIndex(9);
			((Control)label35).set_Text("Towards Greenwhich");
			((Control)txtUT1).set_BackColor(Color.LightCyan);
			((Control)txtUT1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtUT1).set_Location(new Point(412, 35));
			((Control)txtUT1).set_Name("txtUT1");
			((TextBoxBase)txtUT1).set_ReadOnly(true);
			((Control)txtUT1).set_Size(new Size(30, 20));
			((Control)txtUT1).set_TabIndex(8);
			((Control)txtUT1).set_Text("0");
			txtUT1.set_TextAlign((HorizontalAlignment)2);
			((Control)txtY).set_BackColor(Color.LightCyan);
			((Control)txtY).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtY).set_Location(new Point(278, 35));
			((Control)txtY).set_Name("txtY");
			((TextBoxBase)txtY).set_ReadOnly(true);
			((Control)txtY).set_Size(new Size(30, 20));
			((Control)txtY).set_TabIndex(6);
			((Control)txtY).set_Text("0");
			txtY.set_TextAlign((HorizontalAlignment)2);
			((Control)txtX).set_BackColor(Color.LightCyan);
			((Control)txtX).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtX).set_Location(new Point(169, 35));
			((Control)txtX).set_Name("txtX");
			((TextBoxBase)txtX).set_ReadOnly(true);
			((Control)txtX).set_Size(new Size(30, 20));
			((Control)txtX).set_TabIndex(4);
			((Control)txtX).set_Text("0");
			txtX.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDay3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDay3).set_Location(new Point(89, 35));
			((Control)txtDay3).set_Name("txtDay3");
			((Control)txtDay3).set_Size(new Size(24, 20));
			((Control)txtDay3).set_TabIndex(2);
			((Control)txtDay3).set_Text("1");
			txtDay3.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDay3).add_TextChanged((EventHandler)txtDay3_TextChanged);
			((Control)txtMonth3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMonth3).set_Location(new Point(57, 35));
			((Control)txtMonth3).set_Name("txtMonth3");
			((Control)txtMonth3).set_Size(new Size(26, 20));
			((Control)txtMonth3).set_TabIndex(1);
			((Control)txtMonth3).set_Text("1");
			txtMonth3.set_TextAlign((HorizontalAlignment)2);
			((Control)txtMonth3).add_TextChanged((EventHandler)txtMonth3_TextChanged);
			((Control)txtYear3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtYear3).set_Location(new Point(10, 35));
			((Control)txtYear3).set_Name("txtYear3");
			((Control)txtYear3).set_Size(new Size(41, 20));
			((Control)txtYear3).set_TabIndex(0);
			((Control)txtYear3).set_Text("2007");
			txtYear3.set_TextAlign((HorizontalAlignment)2);
			((Control)txtYear3).add_TextChanged((EventHandler)txtYear3_TextChanged);
			((Control)groupBox5).get_Controls().Add((Control)(object)label44);
			((Control)groupBox5).get_Controls().Add((Control)(object)opt2006Ecl);
			((Control)groupBox5).get_Controls().Add((Control)(object)opt1976Ecl);
			((Control)groupBox5).get_Controls().Add((Control)(object)label43);
			((Control)groupBox5).get_Controls().Add((Control)(object)label42);
			((Control)groupBox5).get_Controls().Add((Control)(object)label41);
			((Control)groupBox5).get_Controls().Add((Control)(object)label34);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtLat);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtLong);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtDec);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtRA);
			((Control)groupBox5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox5).set_Location(new Point(27, 392));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(446, 68));
			((Control)groupBox5).set_TabIndex(6);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("RA/Dec <> Ecliptic Long/Lat  (all in degrees)");
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label44).set_Location(new Point(85, 22));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(89, 13));
			((Control)label44).set_TabIndex(39);
			((Control)label44).set_Text("Ecliptic definition:");
			((Control)opt2006Ecl).set_AutoSize(true);
			((Control)opt2006Ecl).set_Location(new Point(242, 21));
			((Control)opt2006Ecl).set_Name("opt2006Ecl");
			((Control)opt2006Ecl).set_Size(new Size(53, 17));
			((Control)opt2006Ecl).set_TabIndex(38);
			opt2006Ecl.set_TabStop(true);
			((Control)opt2006Ecl).set_Text("2006");
			((ButtonBase)opt2006Ecl).set_UseVisualStyleBackColor(true);
			opt2006Ecl.add_CheckedChanged((EventHandler)opt2006Ecl_CheckedChanged);
			((Control)opt1976Ecl).set_AutoSize(true);
			opt1976Ecl.set_Checked(true);
			((Control)opt1976Ecl).set_Location(new Point(179, 21));
			((Control)opt1976Ecl).set_Name("opt1976Ecl");
			((Control)opt1976Ecl).set_Size(new Size(53, 17));
			((Control)opt1976Ecl).set_TabIndex(37);
			opt1976Ecl.set_TabStop(true);
			((Control)opt1976Ecl).set_Text("1976");
			((ButtonBase)opt1976Ecl).set_UseVisualStyleBackColor(true);
			opt1976Ecl.add_CheckedChanged((EventHandler)opt1976Ecl_CheckedChanged);
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label43).set_Location(new Point(343, 46));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(22, 13));
			((Control)label43).set_TabIndex(7);
			((Control)label43).set_Text("Lat");
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(230, 46));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(31, 13));
			((Control)label42).set_TabIndex(6);
			((Control)label42).set_Text("Long");
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(108, 46));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(27, 13));
			((Control)label41).set_TabIndex(5);
			((Control)label41).set_Text("Dec");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(7, 46));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(22, 13));
			((Control)label34).set_TabIndex(4);
			((Control)label34).set_Text("RA");
			((Control)txtLat).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLat).set_Location(new Point(367, 42));
			((Control)txtLat).set_Name("txtLat");
			((Control)txtLat).set_Size(new Size(69, 20));
			((Control)txtLat).set_TabIndex(3);
			((Control)txtLat).add_TextChanged((EventHandler)txtLat_TextChanged);
			((Control)txtLong).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLong).set_Location(new Point(264, 42));
			((Control)txtLong).set_Name("txtLong");
			((Control)txtLong).set_Size(new Size(69, 20));
			((Control)txtLong).set_TabIndex(2);
			((Control)txtLong).add_TextChanged((EventHandler)txtLong_TextChanged);
			((Control)txtDec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDec).set_Location(new Point(137, 42));
			((Control)txtDec).set_Name("txtDec");
			((Control)txtDec).set_Size(new Size(69, 20));
			((Control)txtDec).set_TabIndex(1);
			((Control)txtDec).add_TextChanged((EventHandler)txtDec_TextChanged);
			((Control)txtRA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRA).set_Location(new Point(29, 42));
			((Control)txtRA).set_Name("txtRA");
			((Control)txtRA).set_Size(new Size(69, 20));
			((Control)txtRA).set_TabIndex(0);
			((Control)txtRA).add_TextChanged((EventHandler)txtRA_TextChanged);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)cmbSite);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)cmbSiteFiles);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label67);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label66);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label63);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label62);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label59);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label58);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtUTday);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtUTmonth);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtUTyear);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label57);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label53);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label54);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label55);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtUTs);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtUTm);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtUTh);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label52);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label46);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtLong1S);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtLong1M);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtLong1D);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label50);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label51);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label47);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label48);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label49);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtSTs);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtSTm);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtSTh);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)label45);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtDay4);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtMonth4);
			((Control)grpHistoricalTimes).get_Controls().Add((Control)(object)txtYear4);
			((Control)grpHistoricalTimes).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpHistoricalTimes).set_Location(new Point(23, 543));
			((Control)grpHistoricalTimes).set_Name("grpHistoricalTimes");
			((Control)grpHistoricalTimes).set_Size(new Size(455, 117));
			((Control)grpHistoricalTimes).set_TabIndex(7);
			grpHistoricalTimes.set_TabStop(false);
			((Control)grpHistoricalTimes).set_Text("Convert reported apparent Solar Times of historical observations - to UT");
			cmbSite.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbSite).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbSite).set_FormattingEnabled(true);
			((Control)cmbSite).set_Location(new Point(17, 89));
			cmbSite.set_MaxDropDownItems(20);
			((Control)cmbSite).set_Name("cmbSite");
			((Control)cmbSite).set_Size(new Size(132, 21));
			((Control)cmbSite).set_TabIndex(49);
			cmbSite.add_SelectedIndexChanged((EventHandler)cmbSite_SelectedIndexChanged);
			cmbSiteFiles.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbSiteFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbSiteFiles).set_FormattingEnabled(true);
			((Control)cmbSiteFiles).set_Location(new Point(17, 64));
			cmbSiteFiles.set_MaxDropDownItems(20);
			((Control)cmbSiteFiles).set_Name("cmbSiteFiles");
			((Control)cmbSiteFiles).set_Size(new Size(132, 21));
			cmbSiteFiles.set_Sorted(true);
			((Control)cmbSiteFiles).set_TabIndex(48);
			cmbSiteFiles.add_SelectedIndexChanged((EventHandler)cmbSiteFiles_SelectedIndexChanged);
			((Control)label67).set_AutoSize(true);
			((Control)label67).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label67).set_Location(new Point(316, 68));
			((Control)label67).set_Name("label67");
			((Control)label67).set_Size(new Size(24, 13));
			((Control)label67).set_TabIndex(30);
			((Control)label67).set_Text("day");
			((Control)label66).set_AutoSize(true);
			((Control)label66).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label66).set_Location(new Point(317, 27));
			((Control)label66).set_Name("label66");
			((Control)label66).set_Size(new Size(24, 13));
			((Control)label66).set_TabIndex(18);
			((Control)label66).set_Text("day");
			((Control)label63).set_AutoSize(true);
			((Control)label63).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label63).set_Location(new Point(283, 68));
			((Control)label63).set_Name("label63");
			((Control)label63).set_Size(new Size(24, 13));
			((Control)label63).set_TabIndex(29);
			((Control)label63).set_Text("mth");
			((Control)label62).set_AutoSize(true);
			((Control)label62).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label62).set_Location(new Point(283, 27));
			((Control)label62).set_Name("label62");
			((Control)label62).set_Size(new Size(24, 13));
			((Control)label62).set_TabIndex(17);
			((Control)label62).set_Text("mth");
			((Control)label59).set_AutoSize(true);
			((Control)label59).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label59).set_Location(new Point(245, 27));
			((Control)label59).set_Name("label59");
			((Control)label59).set_Size(new Size(27, 13));
			((Control)label59).set_TabIndex(16);
			((Control)label59).set_Text("year");
			((Control)label58).set_AutoSize(true);
			((Control)label58).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label58).set_Location(new Point(246, 68));
			((Control)label58).set_Name("label58");
			((Control)label58).set_Size(new Size(27, 13));
			((Control)label58).set_TabIndex(28);
			((Control)label58).set_Text("year");
			((Control)txtUTday).set_BackColor(Color.LightCyan);
			((Control)txtUTday).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtUTday).set_Location(new Point(315, 83));
			((Control)txtUTday).set_Name("txtUTday");
			((TextBoxBase)txtUTday).set_ReadOnly(true);
			((Control)txtUTday).set_Size(new Size(28, 20));
			((Control)txtUTday).set_TabIndex(11);
			((Control)txtUTday).set_Text("1");
			txtUTday.set_TextAlign((HorizontalAlignment)2);
			((Control)txtUTmonth).set_BackColor(Color.LightCyan);
			((Control)txtUTmonth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtUTmonth).set_Location(new Point(283, 83));
			((Control)txtUTmonth).set_Name("txtUTmonth");
			((TextBoxBase)txtUTmonth).set_ReadOnly(true);
			((Control)txtUTmonth).set_Size(new Size(26, 20));
			((Control)txtUTmonth).set_TabIndex(10);
			((Control)txtUTmonth).set_Text("1");
			txtUTmonth.set_TextAlign((HorizontalAlignment)2);
			((Control)txtUTyear).set_BackColor(Color.LightCyan);
			((Control)txtUTyear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtUTyear).set_Location(new Point(241, 83));
			((Control)txtUTyear).set_Name("txtUTyear");
			((TextBoxBase)txtUTyear).set_ReadOnly(true);
			((Control)txtUTyear).set_Size(new Size(36, 20));
			((Control)txtUTyear).set_TabIndex(9);
			((Control)txtUTyear).set_Text("1800");
			txtUTyear.set_TextAlign((HorizontalAlignment)2);
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label57).set_Location(new Point(217, 87));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(22, 13));
			((Control)label57).set_TabIndex(27);
			((Control)label57).set_Text("UT");
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label53).set_Location(new Point(422, 68));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(12, 13));
			((Control)label53).set_TabIndex(33);
			((Control)label53).set_Text("s");
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label54).set_Location(new Point(389, 68));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(15, 13));
			((Control)label54).set_TabIndex(32);
			((Control)label54).set_Text("m");
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label55).set_Location(new Point(362, 68));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(13, 13));
			((Control)label55).set_TabIndex(31);
			((Control)label55).set_Text("h");
			((Control)txtUTs).set_BackColor(Color.LightCyan);
			((Control)txtUTs).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtUTs).set_Location(new Point(415, 83));
			((Control)txtUTs).set_Name("txtUTs");
			((TextBoxBase)txtUTs).set_ReadOnly(true);
			((Control)txtUTs).set_Size(new Size(32, 20));
			((Control)txtUTs).set_TabIndex(14);
			((Control)txtUTs).set_Text("0");
			txtUTs.set_TextAlign((HorizontalAlignment)2);
			((Control)txtUTm).set_BackColor(Color.LightCyan);
			((Control)txtUTm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtUTm).set_Location(new Point(385, 83));
			((Control)txtUTm).set_Name("txtUTm");
			((TextBoxBase)txtUTm).set_ReadOnly(true);
			((Control)txtUTm).set_Size(new Size(23, 20));
			((Control)txtUTm).set_TabIndex(13);
			((Control)txtUTm).set_Text("0");
			txtUTm.set_TextAlign((HorizontalAlignment)2);
			((Control)txtUTh).set_BackColor(Color.LightCyan);
			((Control)txtUTh).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtUTh).set_Location(new Point(355, 83));
			((Control)txtUTh).set_Name("txtUTh");
			((TextBoxBase)txtUTh).set_ReadOnly(true);
			((Control)txtUTh).set_Size(new Size(23, 20));
			((Control)txtUTh).set_TabIndex(12);
			((Control)txtUTh).set_Text("0");
			txtUTh.set_TextAlign((HorizontalAlignment)2);
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label52).set_Location(new Point(4, 31));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(50, 26));
			((Control)label52).set_TabIndex(19);
			((Control)label52).set_Text("Site \r\nlongitude");
			label52.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label46).set_Location(new Point(73, 21));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(13, 13));
			((Control)label46).set_TabIndex(20);
			((Control)label46).set_Text("o");
			((Control)txtLong1S).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLong1S).set_Location(new Point(123, 34));
			((Control)txtLong1S).set_Name("txtLong1S");
			((Control)txtLong1S).set_Size(new Size(28, 20));
			((Control)txtLong1S).set_TabIndex(2);
			((Control)txtLong1S).set_Text("0");
			txtLong1S.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLong1S).add_TextChanged((EventHandler)txtLong1S_TextChanged);
			((Control)txtLong1S).add_Enter((EventHandler)txtLong1S_Enter);
			((Control)txtLong1S).add_KeyPress(new KeyPressEventHandler(txtLong1S_KeyPress));
			((Control)txtLong1M).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLong1M).set_Location(new Point(91, 34));
			((Control)txtLong1M).set_Name("txtLong1M");
			((Control)txtLong1M).set_Size(new Size(23, 20));
			((Control)txtLong1M).set_TabIndex(1);
			((Control)txtLong1M).set_Text("0");
			txtLong1M.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLong1M).add_TextChanged((EventHandler)txtLong1M_TextChanged);
			((Control)txtLong1M).add_Enter((EventHandler)txtLong1M_Enter);
			((Control)txtLong1M).add_KeyPress(new KeyPressEventHandler(txtLong1M_KeyPress));
			((Control)txtLong1D).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLong1D).set_Location(new Point(57, 34));
			((Control)txtLong1D).set_Name("txtLong1D");
			((Control)txtLong1D).set_Size(new Size(28, 20));
			((Control)txtLong1D).set_TabIndex(0);
			((Control)txtLong1D).set_Text("0");
			txtLong1D.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLong1D).add_TextChanged((EventHandler)txtLong1D_TextChanged);
			((Control)txtLong1D).add_Enter((EventHandler)txtLong1D_Enter);
			((Control)txtLong1D).add_KeyPress(new KeyPressEventHandler(txtLong1D_KeyPress));
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label50).set_Location(new Point(102, 26));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(9, 13));
			((Control)label50).set_TabIndex(21);
			((Control)label50).set_Text("'");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label51).set_Location(new Point(132, 26));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(12, 13));
			((Control)label51).set_TabIndex(22);
			((Control)label51).set_Text("\"");
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(421, 27));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(12, 13));
			((Control)label47).set_TabIndex(26);
			((Control)label47).set_Text("s");
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label48).set_Location(new Point(389, 27));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(15, 13));
			((Control)label48).set_TabIndex(25);
			((Control)label48).set_Text("m");
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label49).set_Location(new Point(361, 27));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(13, 13));
			((Control)label49).set_TabIndex(24);
			((Control)label49).set_Text("h");
			((Control)txtSTs).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSTs).set_Location(new Point(415, 42));
			((Control)txtSTs).set_Name("txtSTs");
			((Control)txtSTs).set_Size(new Size(32, 20));
			((Control)txtSTs).set_TabIndex(8);
			((Control)txtSTs).set_Text("0");
			txtSTs.set_TextAlign((HorizontalAlignment)2);
			((Control)txtSTs).add_TextChanged((EventHandler)txtSTs_TextChanged);
			((Control)txtSTs).add_Enter((EventHandler)txtSTs_Enter);
			((Control)txtSTs).add_KeyPress(new KeyPressEventHandler(txtSTs_KeyPress));
			((Control)txtSTm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSTm).set_Location(new Point(385, 42));
			((Control)txtSTm).set_Name("txtSTm");
			((Control)txtSTm).set_Size(new Size(23, 20));
			((Control)txtSTm).set_TabIndex(7);
			((Control)txtSTm).set_Text("0");
			txtSTm.set_TextAlign((HorizontalAlignment)2);
			((Control)txtSTm).add_TextChanged((EventHandler)txtSTm_TextChanged);
			((Control)txtSTm).add_Enter((EventHandler)txtSTm_Enter);
			((Control)txtSTm).add_KeyPress(new KeyPressEventHandler(txtSTm_KeyPress));
			((Control)txtSTh).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSTh).set_Location(new Point(355, 42));
			((Control)txtSTh).set_Name("txtSTh");
			((Control)txtSTh).set_Size(new Size(23, 20));
			((Control)txtSTh).set_TabIndex(6);
			((Control)txtSTh).set_Text("0");
			txtSTh.set_TextAlign((HorizontalAlignment)2);
			((Control)txtSTh).add_TextChanged((EventHandler)txtSTh_TextChanged);
			((Control)txtSTh).add_Enter((EventHandler)txtSTh_Enter);
			((Control)txtSTh).add_KeyPress(new KeyPressEventHandler(txtSTh_KeyPress));
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(186, 46));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(53, 13));
			((Control)label45).set_TabIndex(15);
			((Control)label45).set_Text("Solar time");
			label45.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)txtDay4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDay4).set_Location(new Point(315, 42));
			((Control)txtDay4).set_Name("txtDay4");
			((Control)txtDay4).set_Size(new Size(28, 20));
			((Control)txtDay4).set_TabIndex(5);
			((Control)txtDay4).set_Text("1");
			txtDay4.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDay4).add_TextChanged((EventHandler)txtDay4_TextChanged);
			((Control)txtDay4).add_Enter((EventHandler)txtDay4_Enter);
			((Control)txtDay4).add_KeyPress(new KeyPressEventHandler(txtDay4_KeyPress));
			((Control)txtMonth4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMonth4).set_Location(new Point(283, 42));
			((Control)txtMonth4).set_Name("txtMonth4");
			((Control)txtMonth4).set_Size(new Size(26, 20));
			((Control)txtMonth4).set_TabIndex(4);
			((Control)txtMonth4).set_Text("1");
			txtMonth4.set_TextAlign((HorizontalAlignment)2);
			((Control)txtMonth4).add_TextChanged((EventHandler)txtMonth4_TextChanged);
			((Control)txtMonth4).add_Enter((EventHandler)txtMonth4_Enter);
			((Control)txtMonth4).add_KeyPress(new KeyPressEventHandler(txtMonth4_KeyPress));
			((Control)txtYear4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtYear4).set_Location(new Point(241, 42));
			((Control)txtYear4).set_Name("txtYear4");
			((Control)txtYear4).set_Size(new Size(36, 20));
			((Control)txtYear4).set_TabIndex(3);
			((Control)txtYear4).set_Text("1800");
			txtYear4.set_TextAlign((HorizontalAlignment)2);
			((Control)txtYear4).add_TextChanged((EventHandler)txtYear4_TextChanged);
			((Control)txtYear4).add_Enter((EventHandler)txtYear4_Enter);
			((Control)txtYear4).add_KeyPress(new KeyPressEventHandler(txtYear4_KeyPress));
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label56).set_Location(new Point(19, 20));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(97, 13));
			((Control)label56).set_TabIndex(37);
			((Control)label56).set_Text("using IAU standard");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(499, 664));
			((Control)this).get_Controls().Add((Control)(object)grpHistoricalTimes);
			((Control)this).get_Controls().Add((Control)(object)groupBox5);
			((Control)this).get_Controls().Add((Control)(object)groupBox6);
			((Control)this).get_Controls().Add((Control)(object)groupBox4);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemJD", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemJD);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("Conversion");
			((Control)this).set_Text("JD <=> Date          Precession          Angle conversions          Geoid height");
			((Form)this).add_Load((EventHandler)Conversion_Load);
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox6).ResumeLayout(false);
			((Control)groupBox6).PerformLayout();
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((Control)grpHistoricalTimes).ResumeLayout(false);
			((Control)grpHistoricalTimes).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
