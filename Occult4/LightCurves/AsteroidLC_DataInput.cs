using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace LightCurves
{
	public class AsteroidLC_DataInput : Form
	{
		private List<Sites> LCsites = new List<Sites>();

		private string LCSiteFile = Utilities.AppPath + "\\Sites\\LightCurveSites.dat";

		private IContainer components;

		private Label label1;

		private Button cmdExit;

		private Label label2;

		private Label label4;

		private Label label6;

		private Label label7;

		internal NumericUpDown updnYear;

		internal ComboBox cmbMth;

		internal ComboBox cmbDay;

		private TextBox txtLongD;

		private TextBox txtLatSec;

		private TextBox txtLatMin;

		private TextBox txtLatDeg;

		private TextBox txtLongSec;

		private TextBox txtLongMin;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private TextBox txtAsteroidNumber;

		private TextBox txtAsteroidName;

		private Label label13;

		private TextBox txtObserver;

		private Label label14;

		private TextBox txtAlt;

		private ComboBox cmbCatalogue;

		private Label label3;

		private Panel panelUCAC4;

		private Label label41;

		private TextBox txtU4Number;

		private TextBox txtU4Zone;

		private Panel panelNOMAD;

		private Label label40;

		private TextBox txtNOMADnumber;

		private TextBox txtNOMADzone;

		private Panel panelTycho2;

		private TextBox txtTycComp;

		private TextBox txtTycSeqNum;

		private TextBox txtTycRegion;

		private Label label15;

		private Label label16;

		private Panel panelHip;

		private TextBox txtHip;

		private Panel panelB1;

		private Label label17;

		private TextBox txtB1number;

		private TextBox txtB1zone;

		private Label label18;

		private Button cmdGetCoords;

		private TextBox txtDd;

		private TextBox txtDm;

		private TextBox txtDs;

		private TextBox txtS;

		private TextBox txtM;

		private TextBox txtH;

		private Label label21;

		private Label label20;

		private Label label19;

		private Label label5;

		private Label label23;

		private Label label22;

		private Button cmdGetEquivalents;

		private Label label24;

		private Label label33;

		internal TextBox txtK2num;

		internal TextBox txtTYC;

		private Label label32;

		internal TextBox txtSAO;

		private Label label31;

		internal TextBox txtHipparcos;

		private GroupBox groupBox5;

		private Button cmdSetDefaults;

		private Button cmdReset;

		private Button cmdCancel;

		private Label label25;

		private Label label26;

		private Panel panel3;

		private Button cmdAddSite;

		private TextBox txtID;

		private Panel panel2;

		private ComboBox cmbObserverSites;

		private Label label27;

		private Button cmdDeleteSite;

		private Panel panel9;

		private Panel panel8;

		private Panel panel7;

		private Label label35;

		private Label label34;

		private Panel panel1;

		private Panel panel6;

		private Label label29;

		private Panel panel5;

		private Label label30;

		private Panel panel4;

		private Label label28;

		private Panel panelGstar;

		private Label label36;

		private CheckBox chKCorrectDate;

		public AsteroidLC_DataInput()
		{
			InitializeComponent();
		}

		private void AsteroidLC_DataInput_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (!File.Exists(LCSiteFile))
			{
				WriteLCSites();
			}
			ReadLCSites();
			if (LightData.LightCurveForm.LCD == null)
			{
				LightData.LightCurveForm.LCD = new LightCurveData();
			}
			if (LightData.LightCurveForm.LCD.Year > 1900)
			{
				updnYear.set_Value((decimal)LightData.LightCurveForm.LCD.Year);
				if (LightData.LightCurveForm.LCD.Month > 0)
				{
					((ListControl)cmbMth).set_SelectedIndex(LightData.LightCurveForm.LCD.Month - 1);
				}
				else
				{
					((ListControl)cmbMth).set_SelectedIndex(0);
				}
				if (LightData.LightCurveForm.LCD.Day > 0)
				{
					((ListControl)cmbDay).set_SelectedIndex(LightData.LightCurveForm.LCD.Day - 1);
				}
				else
				{
					((ListControl)cmbDay).set_SelectedIndex(0);
				}
			}
			else
			{
				updnYear.set_Value((decimal)DateTime.UtcNow.Year);
				((ListControl)cmbMth).set_SelectedIndex(DateTime.UtcNow.Month - 1);
				((ListControl)cmbDay).set_SelectedIndex(DateTime.UtcNow.Day - 1);
			}
			NumericUpDown obj = updnYear;
			ComboBox obj2 = cmbMth;
			Color red;
			((Control)cmbDay).set_ForeColor(red = Color.Red);
			Color foreColor;
			((Control)obj2).set_ForeColor(foreColor = red);
			((Control)obj).set_ForeColor(foreColor);
			chKCorrectDate.set_Checked(false);
			if ((LightData.LightCurveForm.LCD.LatD == 0) & (LightData.LightCurveForm.LCD.LatM == 0) & (LightData.LightCurveForm.LCD.LatS == 0.0))
			{
				((Control)txtLongD).set_Text(Settings.Default.LCLongDeg);
				((Control)txtLongMin).set_Text(Settings.Default.LCLongMin);
				((Control)txtLongSec).set_Text(Settings.Default.LCLongSec);
				((Control)txtLatDeg).set_Text(Settings.Default.LCLatDeg);
				((Control)txtLatMin).set_Text(Settings.Default.LCLatMin);
				((Control)txtLatSec).set_Text(Settings.Default.LCLatSec);
				((Control)txtAlt).set_Text(Settings.Default.LCAlt);
			}
			else
			{
				((Control)txtLongD).set_Text(LightData.LightCurveForm.LCD.EW + LightData.LightCurveForm.LCD.LongD);
				((Control)txtLongMin).set_Text(LightData.LightCurveForm.LCD.LongM.ToString());
				((Control)txtLongSec).set_Text(string.Format("{0,1:f1}", LightData.LightCurveForm.LCD.LongS));
				((Control)txtLatDeg).set_Text(LightData.LightCurveForm.LCD.NS + LightData.LightCurveForm.LCD.LatD);
				((Control)txtLatMin).set_Text(LightData.LightCurveForm.LCD.LatM.ToString());
				((Control)txtLatSec).set_Text(string.Format("{0,1:f1}", LightData.LightCurveForm.LCD.LatS));
				((Control)txtAlt).set_Text(LightData.LightCurveForm.LCD.AltM.ToString());
			}
			if (LightData.LightCurveForm.LCD.Observer.Trim() == "")
			{
				((Control)txtObserver).set_Text(Settings.Default.LCObserver);
			}
			else
			{
				((Control)txtObserver).set_Text(LightData.LightCurveForm.LCD.Observer.Trim());
			}
			((Control)txtAsteroidNumber).set_Text(LightData.LightCurveForm.LCD.AsteroidNumber.ToString());
			((Control)txtAsteroidName).set_Text(LightData.LightCurveForm.LCD.AsteroidName.Trim());
			if (LightData.LightCurveForm.LCD.Hipparcos > 0)
			{
				((ListControl)cmbCatalogue).set_SelectedIndex(0);
				((Control)txtHip).set_Text(LightData.LightCurveForm.LCD.Hipparcos.ToString());
			}
			else if (LightData.LightCurveForm.LCD.Tyc2B > 0)
			{
				((ListControl)cmbCatalogue).set_SelectedIndex(1);
				((Control)txtTycRegion).set_Text(LightData.LightCurveForm.LCD.Tyc2A.ToString());
				((Control)txtTycSeqNum).set_Text(LightData.LightCurveForm.LCD.Tyc2B.ToString());
				((Control)txtTycComp).set_Text(LightData.LightCurveForm.LCD.Tyc2C.ToString());
			}
			else if (LightData.LightCurveForm.LCD.U4Number > 0)
			{
				((ListControl)cmbCatalogue).set_SelectedIndex(2);
				((Control)txtU4Zone).set_Text(LightData.LightCurveForm.LCD.U4Zone.ToString());
				((Control)txtU4Number).set_Text(LightData.LightCurveForm.LCD.U4Number.ToString());
			}
			else
			{
				((ListControl)cmbCatalogue).set_SelectedIndex(2);
				TextBox obj3 = txtU4Zone;
				string text;
				((Control)txtU4Number).set_Text(text = "");
				((Control)obj3).set_Text(text);
			}
			((Control)txtHipparcos).set_Text(LightData.LightCurveForm.LCD.Hipparcos.ToString());
			((Control)txtSAO).set_Text(LightData.LightCurveForm.LCD.SAO.ToString());
			((Control)txtTYC).set_Text(LightData.LightCurveForm.LCD.Tycho2);
			((Control)txtK2num).set_Text(LightData.LightCurveForm.LCD.Kepler2.ToString());
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			if (!chKCorrectDate.get_Checked())
			{
				MessageBox.Show("Before you can leave this form, you\r\nmust confirm you have set the correct date.\r\n\r\nIf you do not want to proceed, select the Cancel button", "Confirm date", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			TransferData();
			((Form)this).Close();
		}

		private void TransferData()
		{
			LightData.LightCurveForm.LCD.Year = (int)updnYear.get_Value();
			LightData.LightCurveForm.LCD.Month = ((ListControl)cmbMth).get_SelectedIndex() + 1;
			LightData.LightCurveForm.LCD.Day = ((ListControl)cmbDay).get_SelectedIndex() + 1;
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			bool num = ((Control)txtLongD).get_Text().Contains("-");
			if (!double.TryParse(((Control)txtLongD).get_Text().Replace('-', ' '), out result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtLongMin).get_Text(), out result2))
			{
				result2 = 0.0;
			}
			if (!double.TryParse(((Control)txtLongSec).get_Text(), out result3))
			{
				result3 = 0.0;
			}
			double num2 = result + result2 / 60.0 + result3 / 3600.0;
			if (num)
			{
				num2 = 0.0 - num2;
			}
			LightData.LightCurveForm.LCD.SetLongitude_fromDeg = num2;
			bool num3 = ((Control)txtLatDeg).get_Text().Contains("-");
			if (!double.TryParse(((Control)txtLatDeg).get_Text().Replace('-', ' '), out result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtLatMin).get_Text(), out result2))
			{
				result2 = 0.0;
			}
			if (!double.TryParse(((Control)txtLatSec).get_Text(), out result3))
			{
				result3 = 0.0;
			}
			double num4 = result + result2 / 60.0 + result3 / 3600.0;
			if (num3)
			{
				num4 = 0.0 - num4;
			}
			LightData.LightCurveForm.LCD.SetLatitude_fromDeg = num4;
			if (!double.TryParse(((Control)txtAlt).get_Text(), out var result4))
			{
				result4 = 0.0;
			}
			LightData.LightCurveForm.LCD.AltM = (int)result4;
			LightData.LightCurveForm.LCD.Observer = ((Control)txtObserver).get_Text().Trim();
			SetAsteroidName();
			if (!int.TryParse(((Control)txtAsteroidNumber).get_Text(), out var result5))
			{
				result5 = 0;
			}
			LightData.LightCurveForm.LCD.AsteroidNumber = result5;
			LightData.LightCurveForm.LCD.AsteroidName = ((Control)txtAsteroidName).get_Text().Trim();
			GetStarID();
			LightData.LightCurveForm.SetTextBoxes();
		}

		private void txtAsteroidNumber_Leave(object sender, EventArgs e)
		{
			SetAsteroidName();
		}

		private void SetAsteroidName()
		{
			if (!int.TryParse(((Control)txtAsteroidNumber).get_Text(), out var result))
			{
				result = 0;
			}
			if (result > 0)
			{
				if (Elements.MainAsteroids.AstElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
				int asteroidRecord_fromNumber = Elements.MainAsteroids.GetAsteroidRecord_fromNumber(result);
				if (asteroidRecord_fromNumber >= 0)
				{
					((Control)txtAsteroidName).set_Text(Elements.MainAsteroids.AstElements[asteroidRecord_fromNumber].IDName);
				}
			}
		}

		private void cmbCatalogue_SelectedIndexChanged(object sender, EventArgs e)
		{
			ShowNumberField(((ListControl)cmbCatalogue).get_SelectedIndex());
		}

		private void ShowNumberField(int x)
		{
			((Control)panelHip).set_Visible(x == 0);
			((Control)panelTycho2).set_Visible(x == 1);
			((Control)panelUCAC4).set_Visible(x == 2);
			((Control)panelGstar).set_Visible(x >= 3);
			((Control)panelB1).set_Visible(false);
			((Control)panelNOMAD).set_Visible(false);
		}

		private void GetStarID()
		{
			int result = 0;
			short result2 = 0;
			short result3 = 0;
			LightData.LightCurveForm.LCD.NoStarNumberExpected = false;
			if (((ListControl)cmbCatalogue).get_SelectedIndex() == 0)
			{
				if (!int.TryParse(((Control)txtHip).get_Text(), out var result4))
				{
					LightData.LightCurveForm.LCD.Hipparcos = 0;
				}
				else
				{
					LightData.LightCurveForm.LCD.Hipparcos = result4;
				}
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 1)
			{
				if (!short.TryParse(((Control)txtTycRegion).get_Text(), out result2) | !short.TryParse(((Control)txtTycSeqNum).get_Text(), out result3) | !short.TryParse(((Control)txtTycComp).get_Text(), out var result5))
				{
					LightCurveData lCD = LightData.LightCurveForm.LCD;
					LightCurveData lCD2 = LightData.LightCurveForm.LCD;
					short num2 = (LightData.LightCurveForm.LCD.Tyc2C = 0);
					short tyc2A = (lCD2.Tyc2B = num2);
					lCD.Tyc2A = tyc2A;
				}
				else
				{
					LightData.LightCurveForm.LCD.Tyc2A = result2;
					LightData.LightCurveForm.LCD.Tyc2B = result3;
					LightData.LightCurveForm.LCD.Tyc2C = result5;
				}
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 2)
			{
				if (!int.TryParse(((Control)txtU4Zone).get_Text(), out var result6) | !int.TryParse(((Control)txtU4Number).get_Text(), out result))
				{
					LightData.LightCurveForm.LCD.U4Zone = 0;
					LightData.LightCurveForm.LCD.U4Number = 0;
				}
				else
				{
					LightData.LightCurveForm.LCD.U4Zone = result6;
					LightData.LightCurveForm.LCD.U4Number = result;
				}
			}
			else
			{
				LightData.LightCurveForm.LCD.NoStarNumberExpected = true;
			}
		}

		private void cmdGetCoords_Click(object sender, EventArgs e)
		{
			double RA = 0.0;
			double pmRA = 0.0;
			double Dec = 0.0;
			double pmDec = 0.0;
			double Parallax = 0.0;
			double MagV = 0.0;
			double MagB = 0.0;
			double MagR = 0.0;
			double Epoch = 2000.0;
			bool UsedGaia = false;
			int result = 0;
			int result2 = 0;
			int result5;
			double StarReliability;
			double StarDiameter_mas;
			bool UsedGaia2;
			int GaiaPMfromUCAC;
			ulong GaiaSourceID;
			string SourceFile;
			double UncertPMDec;
			double UncertPMRA;
			double UncertDec;
			double UncertRA;
			double RadialVelocity;
			double Epoch2;
			double Parallax_asec;
			int NoGaiaPM;
			int DuplicateSource;
			int GaiaVersion;
			if (((ListControl)cmbCatalogue).get_SelectedIndex() == 0)
			{
				if (!int.TryParse(((Control)txtHip).get_Text(), out var result3))
				{
					result3 = 0;
				}
				if (GetStarPosition.GetHipparcosPosition(result3, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out Epoch, out UsedGaia, out var CorrectStarIdentifier))
				{
					LightData.LightCurveForm.LCD.StarForFileName = "Hip_" + ((Control)txtHip).get_Text().Trim();
				}
				else if (CorrectStarIdentifier.Length > 5)
				{
					LightData.LightCurveForm.LCD.StarForFileName = CorrectStarIdentifier;
				}
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 1)
			{
				if (!int.TryParse(((Control)txtTycRegion).get_Text(), out result))
				{
					result = 0;
				}
				if (!int.TryParse(((Control)txtTycSeqNum).get_Text(), out result2))
				{
					result2 = 0;
				}
				if (!int.TryParse(((Control)txtTycComp).get_Text(), out var result4))
				{
					result4 = 0;
				}
				if (GetStarPosition.GetTycho2Position(result, result2, result4, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out UsedGaia, out Epoch))
				{
					LightData.LightCurveForm.LCD.StarForFileName = "Tyc_" + ((Control)txtTycRegion).get_Text().Trim() + "-" + ((Control)txtTycSeqNum).get_Text().Trim() + "-" + ((Control)txtTycComp).get_Text().Trim();
				}
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 2)
			{
				if (!int.TryParse(((Control)txtU4Zone).get_Text(), out result5))
				{
					result5 = 0;
				}
				if (!int.TryParse(((Control)txtU4Number).get_Text(), out result2))
				{
					result2 = 0;
				}
				if (GetStarPosition.GetUCAC4Position(result5, result2, Parallax_IfNotInGaia_TryHipparcos: false, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out UsedGaia, out Epoch))
				{
					LightData.LightCurveForm.LCD.StarForFileName = "4U_" + ((Control)txtU4Zone).get_Text().Trim() + "-" + ((Control)txtU4Number).get_Text().Trim();
				}
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 3)
			{
				if (!int.TryParse(((Control)txtB1zone).get_Text(), out result5))
				{
					result5 = 0;
				}
				if (!int.TryParse(((Control)txtB1number).get_Text(), out result2))
				{
					result2 = 0;
				}
				if (GetStarPosition.GetB1Position(result5, result2, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out StarReliability, out StarDiameter_mas, out UsedGaia2, out GaiaPMfromUCAC, out GaiaSourceID, out SourceFile, out UncertPMDec, out UncertPMRA, out UncertDec, out UncertRA, out RadialVelocity, out Epoch2, out Parallax_asec, out NoGaiaPM, out DuplicateSource, out GaiaVersion))
				{
					LightData.LightCurveForm.LCD.StarForFileName = "B1_" + ((Control)txtB1zone).get_Text().Trim() + "-" + ((Control)txtB1number).get_Text().Trim();
				}
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 4)
			{
				if (!int.TryParse(((Control)txtNOMADzone).get_Text(), out result5))
				{
					result5 = 0;
				}
				if (!int.TryParse(((Control)txtNOMADnumber).get_Text(), out result2))
				{
					result2 = 0;
				}
				if (GetStarPosition.GetNOMAD_Full_Position(result5, result2, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax_asec, out Epoch2, out UsedGaia2, out GaiaVersion, out GaiaSourceID, out SourceFile, out RadialVelocity, out UncertRA, out UncertDec, out UncertPMRA, out UncertPMDec, out StarDiameter_mas, out StarReliability, out DuplicateSource, out NoGaiaPM, out GaiaPMfromUCAC))
				{
					LightData.LightCurveForm.LCD.StarForFileName = "N1_" + ((Control)txtNOMADzone).get_Text().Trim() + "-" + ((Control)txtNOMADnumber).get_Text().Trim();
				}
			}
		}

		private void GetEquivalents(double RA, double Dec)
		{
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("The functionality to identify equivalent stars requires internet access. You are not connected to the internet.", "No internet");
				return;
			}
			TextBox obj = txtHipparcos;
			TextBox obj2 = txtSAO;
			TextBox obj3 = txtTYC;
			string text;
			((Control)txtK2num).set_Text(text = "");
			string text2;
			((Control)obj3).set_Text(text2 = text);
			string text3;
			((Control)obj2).set_Text(text3 = text2);
			((Control)obj).set_Text(text3);
			Application.DoEvents();
			string[] array = Vizier_Sesame.GetEquivalents(RA, Dec).Split(new char[1] { ',' });
			TextBox obj4 = txtHipparcos;
			TextBox obj5 = txtSAO;
			((Control)txtTYC).set_Text(text2 = "");
			((Control)obj5).set_Text(text3 = text2);
			((Control)obj4).set_Text(text3);
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].Contains("HIP "))
				{
					((Control)txtHipparcos).set_Text(array[i].Substring(3).Trim());
					LightData.LightCurveForm.LCD.Hipparcos = int.Parse(array[i].Substring(3));
				}
				if (array[i].Contains("SAO "))
				{
					((Control)txtSAO).set_Text(array[i].Substring(3).Trim());
					LightData.LightCurveForm.LCD.SAO = int.Parse(array[i].Substring(3));
				}
				if (array[i].Contains("TYC "))
				{
					((Control)txtTYC).set_Text(array[i].Substring(3).Trim());
					LightData.LightCurveForm.LCD.SetTycho2 = ((Control)txtTYC).get_Text();
				}
			}
			Cursor.set_Current(Cursors.get_Default());
			GetKepler2(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI));
			((Control)txtK2num).set_Text(LightData.LightCurveForm.LCD.Kepler2.ToString());
		}

		private void GetKepler2(double RA_deg, double Dec_deg)
		{
			if ((Kepler2.NumKep2Stars > 1) & Kepler2.Kepler2DataExists)
			{
				Kepler2.Initialise_Kepler2_ForAsteroids();
				if (Kepler2.StarInKepler2(RA_deg, Dec_deg, out var RecNum))
				{
					LightData.LightCurveForm.LCD.Kepler2 = Kepler2.K2[RecNum].EPIC_ID;
				}
				else
				{
					LightData.LightCurveForm.LCD.Kepler2 = 0L;
				}
			}
			else
			{
				LightData.LightCurveForm.LCD.Kepler2 = 0L;
			}
		}

		private void cmdGetEquivalents_Click(object sender, EventArgs e)
		{
			//IL_00a1: Unknown result type (might be due to invalid IL or missing references)
			double num = 0.0;
			double num2 = 0.0;
			if (!int.TryParse(((Control)txtH).get_Text(), out var result))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtM).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txtS).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			num = ((double)result + (double)result2 / 60.0 + result3 / 3600.0) * 15.0;
			if (num == 0.0)
			{
				MessageBox.Show("The star's RA and Declination have not been specified", "No star coords", (MessageBoxButtons)0);
				return;
			}
			if (!int.TryParse(((Control)txtDd).get_Text().Replace("-", ""), out var result4))
			{
				result4 = 0;
			}
			if (!int.TryParse(((Control)txtDm).get_Text(), out var result5))
			{
				result5 = 0;
			}
			if (!double.TryParse(((Control)txtDs).get_Text(), out var result6))
			{
				result6 = 0.0;
			}
			num2 = (double)result4 + (double)result5 / 60.0 + result6 / 3600.0;
			if (((Control)txtDd).get_Text().Contains("-"))
			{
				num2 = 0.0 - num2;
			}
			GetEquivalents(num / (180.0 / Math.PI), num2 / (180.0 / Math.PI));
		}

		private void cmdSetDefaults_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("You are going to replace the stored Default values \r\nfor site coordinates and observer with the presently \r\ndisplayed values.\r\n\r\nDo you want to proceed?", "Confirm setting default values", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				Settings.Default.LCLongDeg = ((Control)txtLongD).get_Text();
				Settings.Default.LCLongMin = ((Control)txtLongMin).get_Text();
				Settings.Default.LCLongSec = ((Control)txtLongSec).get_Text();
				Settings.Default.LCLatDeg = ((Control)txtLatDeg).get_Text();
				Settings.Default.LCLatMin = ((Control)txtLatMin).get_Text();
				Settings.Default.LCLatSec = ((Control)txtLatSec).get_Text();
				Settings.Default.LCAlt = ((Control)txtAlt).get_Text();
				Settings.Default.LCObserver = ((Control)txtObserver).get_Text();
			}
		}

		private void cmdReset_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("You are going to replace the presently displayed \r\nvalues for site coordinates and observer with the \r\nstored Default values.\r\n\r\nDo you want to proceed?", "Confirm using default values", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				((Control)txtLongD).set_Text(Settings.Default.LCLongDeg);
				((Control)txtLongMin).set_Text(Settings.Default.LCLongMin);
				((Control)txtLongSec).set_Text(Settings.Default.LCLongSec);
				((Control)txtLatDeg).set_Text(Settings.Default.LCLatDeg);
				((Control)txtLatMin).set_Text(Settings.Default.LCLatMin);
				((Control)txtLatSec).set_Text(Settings.Default.LCLatSec);
				((Control)txtAlt).set_Text(Settings.Default.LCAlt);
				((Control)txtObserver).set_Text(Settings.Default.LCObserver);
			}
		}

		private void txtObserver_Leave(object sender, EventArgs e)
		{
			if (((Control)txtObserver).get_Text().Trim() == "")
			{
				((Control)txtObserver).set_Text("Unknown");
			}
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtObserver).get_Text(), "Observer name", CheckForPipes: false, out var RevisedText))
			{
				((Control)txtObserver).set_Text(RevisedText);
				((Control)txtObserver).Focus();
			}
		}

		private void cmbObserverSites_SelectedIndexChanged(object sender, EventArgs e)
		{
			PopulateSites(((ListControl)cmbObserverSites).get_SelectedIndex());
		}

		private void cmdAddSite_Click(object sender, EventArgs e)
		{
			//IL_0036: Unknown result type (might be due to invalid IL or missing references)
			//IL_0091: Unknown result type (might be due to invalid IL or missing references)
			//IL_0097: Invalid comparison between Unknown and I4
			//IL_00a9: Unknown result type (might be due to invalid IL or missing references)
			string text = ((Control)txtID).get_Text().Trim().Replace("|", "_");
			if (text.Length < 5)
			{
				MessageBox.Show("Identifier must include at least 5 characters", "Insufficient characters", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			int num = -1;
			for (int i = 0; i < LCsites.Count; i++)
			{
				if (text == LCsites[i].ID)
				{
					if ((int)MessageBox.Show("An entry for this tag already exists:\r\n\r\n" + LCsites[i].Entry + "\r\n\r\nDo you want to replace this entry?", "Duplicate tag", (MessageBoxButtons)4, (MessageBoxIcon)16, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 6)
					{
						MessageBox.Show("New entry not added", "Not added", (MessageBoxButtons)0);
						return;
					}
					num = i;
				}
			}
			int selectedIndex = 0;
			Sites sites = new Sites();
			string text3 = (sites.ID = ((Control)txtID).get_Text().Trim().Replace("|", "_"));
			string text4 = text3;
			sites.LonDeg = ((Control)txtLongD).get_Text().Trim().Replace("|", "_");
			sites.LongMin = ((Control)txtLongMin).get_Text().Trim().Replace("|", "_");
			sites.LongSec = ((Control)txtLongSec).get_Text().Trim().Replace("|", "_");
			sites.LatDeg = ((Control)txtLatDeg).get_Text().Trim().Replace("|", "_");
			sites.LatMin = ((Control)txtLatMin).get_Text().Trim().Replace("|", "_");
			sites.LatSec = ((Control)txtLatSec).get_Text().Trim().Replace("|", "_");
			sites.Alt = ((Control)txtAlt).get_Text().Trim().Replace("|", "_");
			sites.Observer = ((Control)txtObserver).get_Text().Trim().Replace("|", "_");
			if (num >= 0)
			{
				LCsites[num] = sites;
			}
			else
			{
				LCsites.Add(sites);
			}
			LCsites.Sort();
			WriteLCSites();
			cmbObserverSites.get_Items().Clear();
			for (int j = 0; j < LCsites.Count; j++)
			{
				cmbObserverSites.get_Items().Add((object)LCsites[j].ID);
				if (LCsites[j].ID == text4)
				{
					selectedIndex = j;
				}
			}
			((ListControl)cmbObserverSites).set_SelectedIndex(selectedIndex);
		}

		private void PopulateSites(int k)
		{
			((Control)txtID).set_Text(LCsites[k].ID);
			((Control)txtLongD).set_Text(LCsites[k].LonDeg);
			((Control)txtLongMin).set_Text(LCsites[k].LongMin);
			((Control)txtLongSec).set_Text(LCsites[k].LongSec);
			((Control)txtLatDeg).set_Text(LCsites[k].LatDeg);
			((Control)txtLatMin).set_Text(LCsites[k].LatMin);
			((Control)txtLatSec).set_Text(LCsites[k].LatSec);
			((Control)txtAlt).set_Text(LCsites[k].Alt);
			((Control)txtObserver).set_Text(LCsites[k].Observer);
		}

		private void ReadLCSites()
		{
			LCsites.Clear();
			using StreamReader streamReader = new StreamReader(LCSiteFile);
			streamReader.ReadLine();
			try
			{
				do
				{
					string[] array = streamReader.ReadLine()!.Split(new char[1] { '|' });
					Sites sites = new Sites();
					sites.ID = array[0];
					sites.LonDeg = array[1];
					sites.LongMin = array[2];
					sites.LongSec = array[3];
					sites.LatDeg = array[4];
					sites.LatMin = array[5];
					sites.LatSec = array[6];
					sites.Alt = array[7];
					sites.Observer = array[8];
					LCsites.Add(sites);
				}
				while (!streamReader.EndOfStream);
			}
			catch
			{
			}
			LCsites.Sort();
			cmbObserverSites.get_Items().Clear();
			for (int i = 0; i < LCsites.Count; i++)
			{
				cmbObserverSites.get_Items().Add((object)LCsites[i].ID);
			}
			if (LCsites.Count > 0)
			{
				((ListControl)cmbObserverSites).set_SelectedIndex(0);
				PopulateSites(0);
			}
		}

		private void WriteLCSites()
		{
			using StreamWriter streamWriter = new StreamWriter(LCSiteFile);
			streamWriter.WriteLine("ID|Log_deg|Long_min|Long_sec|Lat_deg|Lat_min|Lat_sec|Alt|Observer");
			for (int i = 0; i < LCsites.Count; i++)
			{
				streamWriter.WriteLine(LCsites[i].ID + "|" + LCsites[i].LonDeg + "|" + LCsites[i].LongMin + "|" + LCsites[i].LongSec + "|" + LCsites[i].LatDeg + "|" + LCsites[i].LatMin + "|" + LCsites[i].LatSec + "|" + LCsites[i].Alt + "|" + LCsites[i].Observer);
			}
		}

		private void cmdDeleteSite_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to delete the currently displayed entry from the Observer list?", "Confirm deletion", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				LCsites.RemoveAt(((ListControl)cmbObserverSites).get_SelectedIndex());
				LCsites.Sort();
				WriteLCSites();
				cmbObserverSites.get_Items().Clear();
				for (int i = 0; i < LCsites.Count; i++)
				{
					cmbObserverSites.get_Items().Add((object)LCsites[i].ID);
				}
				if (LCsites.Count > 0)
				{
					((ListControl)cmbObserverSites).set_SelectedIndex(0);
					PopulateSites(0);
				}
			}
		}

		private void txtLongD_Leave(object sender, EventArgs e)
		{
			if ((((Control)txtLongD).get_Text().Trim() == "") | (((Control)txtLongD).get_Text().Trim() == "-"))
			{
				((Control)txtLongD).set_Text("0");
			}
		}

		private void txtLongMin_Leave(object sender, EventArgs e)
		{
			if (((Control)txtLongMin).get_Text().Trim() == "")
			{
				((Control)txtLongMin).set_Text("0");
			}
		}

		private void txtLongSec_Leave(object sender, EventArgs e)
		{
			if (((Control)txtLongSec).get_Text().Trim() == "")
			{
				((Control)txtLongSec).set_Text("0.0");
			}
		}

		private void txtLatDeg_Leave(object sender, EventArgs e)
		{
			if ((((Control)txtLatDeg).get_Text().Trim() == "") | (((Control)txtLatDeg).get_Text().Trim() == "-"))
			{
				((Control)txtLatDeg).set_Text("0");
			}
		}

		private void txtLatMin_Leave(object sender, EventArgs e)
		{
			if (((Control)txtLatMin).get_Text().Trim() == "")
			{
				((Control)txtLatMin).set_Text("0");
			}
		}

		private void txtLatSec_Leave(object sender, EventArgs e)
		{
			if (((Control)txtLatSec).get_Text().Trim() == "")
			{
				((Control)txtLatSec).set_Text("0.0");
			}
		}

		private void txtAlt_Leave(object sender, EventArgs e)
		{
			if (((Control)txtAlt).get_Text().Trim() == "")
			{
				((Control)txtAlt).set_Text("0");
			}
		}

		private void chKCorrectDate_Click(object sender, EventArgs e)
		{
			chKCorrectDate.set_Checked(true);
			NumericUpDown obj = updnYear;
			ComboBox obj2 = cmbMth;
			Color darkGreen;
			((Control)cmbDay).set_ForeColor(darkGreen = Color.DarkGreen);
			Color foreColor;
			((Control)obj2).set_ForeColor(foreColor = darkGreen);
			((Control)obj).set_ForeColor(foreColor);
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AsteroidLC_DataInput));
			label1 = new Label();
			cmdExit = new Button();
			label2 = new Label();
			label4 = new Label();
			updnYear = new NumericUpDown();
			cmbMth = new ComboBox();
			cmbDay = new ComboBox();
			label6 = new Label();
			label7 = new Label();
			txtLongD = new TextBox();
			txtLatSec = new TextBox();
			txtLatMin = new TextBox();
			txtLatDeg = new TextBox();
			txtLongSec = new TextBox();
			txtLongMin = new TextBox();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			txtAsteroidNumber = new TextBox();
			txtAsteroidName = new TextBox();
			label13 = new Label();
			txtObserver = new TextBox();
			label14 = new Label();
			txtAlt = new TextBox();
			panel3 = new Panel();
			panel9 = new Panel();
			cmdDeleteSite = new Button();
			panel8 = new Panel();
			cmdAddSite = new Button();
			txtID = new TextBox();
			panel7 = new Panel();
			cmbObserverSites = new ComboBox();
			label27 = new Label();
			label35 = new Label();
			panel2 = new Panel();
			label34 = new Label();
			label25 = new Label();
			cmdReset = new Button();
			cmdSetDefaults = new Button();
			label26 = new Label();
			cmbCatalogue = new ComboBox();
			label3 = new Label();
			panelUCAC4 = new Panel();
			label41 = new Label();
			txtU4Number = new TextBox();
			txtU4Zone = new TextBox();
			panelNOMAD = new Panel();
			label40 = new Label();
			txtNOMADnumber = new TextBox();
			txtNOMADzone = new TextBox();
			panelTycho2 = new Panel();
			txtTycComp = new TextBox();
			txtTycSeqNum = new TextBox();
			txtTycRegion = new TextBox();
			label15 = new Label();
			label16 = new Label();
			panelHip = new Panel();
			txtHip = new TextBox();
			panelB1 = new Panel();
			label17 = new Label();
			txtB1number = new TextBox();
			txtB1zone = new TextBox();
			label18 = new Label();
			cmdGetEquivalents = new Button();
			label24 = new Label();
			label33 = new Label();
			txtK2num = new TextBox();
			txtTYC = new TextBox();
			label32 = new Label();
			txtSAO = new TextBox();
			label31 = new Label();
			txtHipparcos = new TextBox();
			label21 = new Label();
			label20 = new Label();
			label19 = new Label();
			label5 = new Label();
			cmdGetCoords = new Button();
			txtDd = new TextBox();
			txtDm = new TextBox();
			txtDs = new TextBox();
			txtS = new TextBox();
			txtM = new TextBox();
			txtH = new TextBox();
			label23 = new Label();
			label22 = new Label();
			groupBox5 = new GroupBox();
			cmdCancel = new Button();
			panel1 = new Panel();
			panel6 = new Panel();
			label29 = new Label();
			panel5 = new Panel();
			panelGstar = new Panel();
			label36 = new Label();
			label30 = new Label();
			panel4 = new Panel();
			chKCorrectDate = new CheckBox();
			label28 = new Label();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)panel3).SuspendLayout();
			((Control)panel9).SuspendLayout();
			((Control)panel8).SuspendLayout();
			((Control)panel7).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panelUCAC4).SuspendLayout();
			((Control)panelNOMAD).SuspendLayout();
			((Control)panelTycho2).SuspendLayout();
			((Control)panelHip).SuspendLayout();
			((Control)panelB1).SuspendLayout();
			((Control)groupBox5).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)panel6).SuspendLayout();
			((Control)panel5).SuspendLayout();
			((Control)panelGstar).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(84, 11));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(294, 20));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Details of the asteroidal occultation");
			((Control)cmdExit).set_BackColor(Color.LawnGreen);
			((Control)cmdExit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdExit).set_Location(new Point(217, 393));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(144, 42));
			((Control)cmdExit).set_TabIndex(2);
			((Control)cmdExit).set_Text("Transfer data and Exit");
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(false);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(42, 25));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(29, 13));
			((Control)label2).set_TabIndex(2);
			((Control)label2).set_Text("Year");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(32, 26));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(108, 13));
			((Control)label4).set_TabIndex(0);
			((Control)label4).set_Text("Number            Name");
			((Control)updnYear).set_ForeColor(Color.Red);
			((Control)updnYear).set_Location(new Point(39, 42));
			updnYear.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 1900, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(47, 20));
			((Control)updnYear).set_TabIndex(5);
			updnYear.set_Value(new decimal(new int[4] { 2015, 0, 0, 0 }));
			((Control)cmbMth).set_ForeColor(Color.Red);
			((ListControl)cmbMth).set_FormattingEnabled(true);
			cmbMth.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbMth).set_Location(new Point(93, 42));
			((Control)cmbMth).set_Name("cmbMth");
			((Control)cmbMth).set_Size(new Size(43, 21));
			((Control)cmbMth).set_TabIndex(6);
			((Control)cmbDay).set_ForeColor(Color.Red);
			((ListControl)cmbDay).set_FormattingEnabled(true);
			cmbDay.get_Items().AddRange(new object[31]
			{
				"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				"11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
				"21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
				"31"
			});
			((Control)cmbDay).set_Location(new Point(142, 42));
			((Control)cmbDay).set_Name("cmbDay");
			((Control)cmbDay).set_Size(new Size(42, 21));
			((Control)cmbDay).set_TabIndex(7);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(144, 25));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(26, 13));
			((Control)label6).set_TabIndex(4);
			((Control)label6).set_Text("Day");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(94, 25));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(37, 13));
			((Control)label7).set_TabIndex(3);
			((Control)label7).set_Text("Month");
			((Control)txtLongD).set_Location(new Point(78, 36));
			((TextBoxBase)txtLongD).set_MaxLength(4);
			((Control)txtLongD).set_Name("txtLongD");
			((Control)txtLongD).set_Size(new Size(32, 20));
			((Control)txtLongD).set_TabIndex(0);
			((Control)txtLongD).add_Leave((EventHandler)txtLongD_Leave);
			((Control)txtLatSec).set_Location(new Point(145, 60));
			((TextBoxBase)txtLatSec).set_MaxLength(4);
			((Control)txtLatSec).set_Name("txtLatSec");
			((Control)txtLatSec).set_Size(new Size(32, 20));
			((Control)txtLatSec).set_TabIndex(5);
			((Control)txtLatSec).add_Leave((EventHandler)txtLatSec_Leave);
			((Control)txtLatMin).set_Location(new Point(115, 60));
			((TextBoxBase)txtLatMin).set_MaxLength(2);
			((Control)txtLatMin).set_Name("txtLatMin");
			((Control)txtLatMin).set_Size(new Size(25, 20));
			((Control)txtLatMin).set_TabIndex(4);
			((Control)txtLatMin).add_Leave((EventHandler)txtLatMin_Leave);
			((Control)txtLatDeg).set_Location(new Point(79, 60));
			((TextBoxBase)txtLatDeg).set_MaxLength(3);
			((Control)txtLatDeg).set_Name("txtLatDeg");
			((Control)txtLatDeg).set_Size(new Size(32, 20));
			((Control)txtLatDeg).set_TabIndex(3);
			((Control)txtLatDeg).add_Leave((EventHandler)txtLatDeg_Leave);
			((Control)txtLongSec).set_Location(new Point(145, 36));
			((TextBoxBase)txtLongSec).set_MaxLength(4);
			((Control)txtLongSec).set_Name("txtLongSec");
			((Control)txtLongSec).set_Size(new Size(32, 20));
			((Control)txtLongSec).set_TabIndex(2);
			((Control)txtLongSec).add_Leave((EventHandler)txtLongSec_Leave);
			((Control)txtLongMin).set_Location(new Point(115, 36));
			((TextBoxBase)txtLongMin).set_MaxLength(2);
			((Control)txtLongMin).set_Name("txtLongMin");
			((Control)txtLongMin).set_Size(new Size(25, 20));
			((Control)txtLongMin).set_TabIndex(1);
			((Control)txtLongMin).add_Leave((EventHandler)txtLongMin_Leave);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(9, 41));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(67, 13));
			((Control)label8).set_TabIndex(8);
			((Control)label8).set_Text("E. Longitude");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(31, 62));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(45, 13));
			((Control)label9).set_TabIndex(12);
			((Control)label9).set_Text("Latitude");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(99, 21));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(13, 13));
			((Control)label10).set_TabIndex(9);
			((Control)label10).set_Text("o");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(128, 26));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(9, 13));
			((Control)label11).set_TabIndex(10);
			((Control)label11).set_Text("'");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(162, 25));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(12, 13));
			((Control)label12).set_TabIndex(11);
			((Control)label12).set_Text("\"");
			((Control)txtAsteroidNumber).set_Location(new Point(29, 42));
			((TextBoxBase)txtAsteroidNumber).set_MaxLength(7);
			((Control)txtAsteroidNumber).set_Name("txtAsteroidNumber");
			((Control)txtAsteroidNumber).set_Size(new Size(53, 20));
			((Control)txtAsteroidNumber).set_TabIndex(1);
			((Control)txtAsteroidNumber).add_Leave((EventHandler)txtAsteroidNumber_Leave);
			((Control)txtAsteroidName).set_Location(new Point(86, 42));
			((TextBoxBase)txtAsteroidName).set_MaxLength(16);
			((Control)txtAsteroidName).set_Name("txtAsteroidName");
			((TextBoxBase)txtAsteroidName).set_ReadOnly(true);
			((Control)txtAsteroidName).set_Size(new Size(107, 20));
			((Control)txtAsteroidName).set_TabIndex(2);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(27, 112));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(50, 13));
			((Control)label13).set_TabIndex(14);
			((Control)label13).set_Text("Observer");
			((Control)txtObserver).set_Location(new Point(79, 109));
			((TextBoxBase)txtObserver).set_MaxLength(25);
			((Control)txtObserver).set_Name("txtObserver");
			((Control)txtObserver).set_Size(new Size(107, 20));
			((Control)txtObserver).set_TabIndex(7);
			((Control)txtObserver).add_Leave((EventHandler)txtObserver_Leave);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(11, 88));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(66, 13));
			((Control)label14).set_TabIndex(13);
			((Control)label14).set_Text("Altitude (msl)");
			((Control)txtAlt).set_Location(new Point(79, 85));
			((TextBoxBase)txtAlt).set_MaxLength(5);
			((Control)txtAlt).set_Name("txtAlt");
			((Control)txtAlt).set_Size(new Size(39, 20));
			((Control)txtAlt).set_TabIndex(6);
			((Control)txtAlt).add_Leave((EventHandler)txtAlt_Leave);
			((Control)panel3).set_BackColor(Color.LightCyan);
			panel3.set_BorderStyle((BorderStyle)2);
			((Control)panel3).get_Controls().Add((Control)(object)panel9);
			((Control)panel3).get_Controls().Add((Control)(object)panel8);
			((Control)panel3).get_Controls().Add((Control)(object)panel7);
			((Control)panel3).get_Controls().Add((Control)(object)label35);
			((Control)panel3).set_Location(new Point(10, 240));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(196, 219));
			((Control)panel3).set_TabIndex(4);
			((Control)panel9).set_BackColor(Color.LightBlue);
			panel9.set_BorderStyle((BorderStyle)2);
			((Control)panel9).get_Controls().Add((Control)(object)cmdDeleteSite);
			((Control)panel9).set_Location(new Point(7, 166));
			((Control)panel9).set_Name("panel9");
			((Control)panel9).set_Size(new Size(179, 43));
			((Control)panel9).set_TabIndex(2);
			((Control)cmdDeleteSite).set_BackColor(Color.Pink);
			((Control)cmdDeleteSite).set_Location(new Point(31, 7));
			((Control)cmdDeleteSite).set_Name("cmdDeleteSite");
			((Control)cmdDeleteSite).set_Size(new Size(113, 25));
			((Control)cmdDeleteSite).set_TabIndex(25);
			((Control)cmdDeleteSite).set_Text("Delete selected site");
			((ButtonBase)cmdDeleteSite).set_UseVisualStyleBackColor(false);
			((Control)cmdDeleteSite).add_Click((EventHandler)cmdDeleteSite_Click);
			((Control)panel8).set_BackColor(Color.LightBlue);
			panel8.set_BorderStyle((BorderStyle)2);
			((Control)panel8).get_Controls().Add((Control)(object)cmdAddSite);
			((Control)panel8).get_Controls().Add((Control)(object)txtID);
			((Control)panel8).set_Location(new Point(7, 83));
			((Control)panel8).set_Name("panel8");
			((Control)panel8).set_Size(new Size(179, 75));
			((Control)panel8).set_TabIndex(1);
			((Control)cmdAddSite).set_BackColor(Color.NavajoWhite);
			((ButtonBase)cmdAddSite).get_FlatAppearance().set_BorderColor(Color.SaddleBrown);
			((ButtonBase)cmdAddSite).get_FlatAppearance().set_BorderSize(2);
			((ButtonBase)cmdAddSite).set_FlatStyle((FlatStyle)0);
			((Control)cmdAddSite).set_Location(new Point(6, 6));
			((Control)cmdAddSite).set_Name("cmdAddSite");
			((Control)cmdAddSite).set_Size(new Size(163, 39));
			((Control)cmdAddSite).set_TabIndex(1);
			((Control)cmdAddSite).set_Text("Add site to the observer list, using the following identifier");
			((ButtonBase)cmdAddSite).set_UseVisualStyleBackColor(false);
			((Control)cmdAddSite).add_Click((EventHandler)cmdAddSite_Click);
			((Control)txtID).set_Location(new Point(6, 47));
			((Control)txtID).set_Name("txtID");
			((Control)txtID).set_Size(new Size(162, 20));
			((Control)txtID).set_TabIndex(0);
			((Control)panel7).set_BackColor(Color.LightBlue);
			panel7.set_BorderStyle((BorderStyle)2);
			((Control)panel7).get_Controls().Add((Control)(object)cmbObserverSites);
			((Control)panel7).get_Controls().Add((Control)(object)label27);
			((Control)panel7).set_Location(new Point(7, 26));
			((Control)panel7).set_Name("panel7");
			((Control)panel7).set_Size(new Size(179, 49));
			((Control)panel7).set_TabIndex(0);
			((ListControl)cmbObserverSites).set_FormattingEnabled(true);
			((Control)cmbObserverSites).set_Location(new Point(3, 21));
			cmbObserverSites.set_MaxDropDownItems(10);
			((Control)cmbObserverSites).set_Name("cmbObserverSites");
			((Control)cmbObserverSites).set_Size(new Size(168, 21));
			((Control)cmbObserverSites).set_TabIndex(0);
			cmbObserverSites.add_SelectedIndexChanged((EventHandler)cmbObserverSites_SelectedIndexChanged);
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Location(new Point(15, 5));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(138, 13));
			((Control)label27).set_TabIndex(19);
			((Control)label27).set_Text("Select site from observer list");
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label35).set_Location(new Point(7, 3));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(179, 15));
			((Control)label35).set_TabIndex(26);
			((Control)label35).set_Text("Select / Manage Observers");
			((Control)panel2).set_BackColor(Color.Cornsilk);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)label34);
			((Control)panel2).get_Controls().Add((Control)(object)label25);
			((Control)panel2).get_Controls().Add((Control)(object)label14);
			((Control)panel2).get_Controls().Add((Control)(object)txtAlt);
			((Control)panel2).get_Controls().Add((Control)(object)cmdReset);
			((Control)panel2).get_Controls().Add((Control)(object)label13);
			((Control)panel2).get_Controls().Add((Control)(object)cmdSetDefaults);
			((Control)panel2).get_Controls().Add((Control)(object)txtObserver);
			((Control)panel2).get_Controls().Add((Control)(object)label10);
			((Control)panel2).get_Controls().Add((Control)(object)label9);
			((Control)panel2).get_Controls().Add((Control)(object)label8);
			((Control)panel2).get_Controls().Add((Control)(object)txtLongMin);
			((Control)panel2).get_Controls().Add((Control)(object)txtLongSec);
			((Control)panel2).get_Controls().Add((Control)(object)txtLatDeg);
			((Control)panel2).get_Controls().Add((Control)(object)txtLatMin);
			((Control)panel2).get_Controls().Add((Control)(object)txtLatSec);
			((Control)panel2).get_Controls().Add((Control)(object)txtLongD);
			((Control)panel2).get_Controls().Add((Control)(object)label11);
			((Control)panel2).get_Controls().Add((Control)(object)label12);
			((Control)panel2).get_Controls().Add((Control)(object)label26);
			((Control)panel2).set_Location(new Point(10, 43));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(196, 183));
			((Control)panel2).set_TabIndex(0);
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(26, 3));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(141, 15));
			((Control)label34).set_TabIndex(19);
			((Control)label34).set_Text("1.  Site and Observer");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Location(new Point(120, 88));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(38, 13));
			((Control)label25).set_TabIndex(17);
			((Control)label25).set_Text("meters");
			((Control)cmdReset).set_BackColor(Color.PowderBlue);
			((Control)cmdReset).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdReset).set_Location(new Point(101, 149));
			((Control)cmdReset).set_Name("cmdReset");
			((Control)cmdReset).set_Size(new Size(80, 25));
			((Control)cmdReset).set_TabIndex(16);
			((Control)cmdReset).set_Text("Use default");
			((ButtonBase)cmdReset).set_UseVisualStyleBackColor(false);
			((Control)cmdReset).add_Click((EventHandler)cmdReset_Click);
			((Control)cmdSetDefaults).set_BackColor(Color.PowderBlue);
			((Control)cmdSetDefaults).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetDefaults).set_Location(new Point(11, 149));
			((Control)cmdSetDefaults).set_Name("cmdSetDefaults");
			((Control)cmdSetDefaults).set_Size(new Size(82, 25));
			((Control)cmdSetDefaults).set_TabIndex(15);
			((Control)cmdSetDefaults).set_Text("Set as default");
			((ButtonBase)cmdSetDefaults).set_UseVisualStyleBackColor(false);
			((Control)cmdSetDefaults).add_Click((EventHandler)cmdSetDefaults_Click);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_ForeColor(Color.Maroon);
			((Control)label26).set_Location(new Point(8, 131));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(179, 13));
			((Control)label26).set_TabIndex(18);
			((Control)label26).set_Text("Separate multiple observers with    / ");
			cmbCatalogue.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbCatalogue).set_FormattingEnabled(true);
			cmbCatalogue.get_Items().AddRange(new object[6] { "Hipparcos", "Tycho-2", "UCAC4", "Jhhmmss.s_ddmmss", "USNO-B1", "NOMAD" });
			((Control)cmbCatalogue).set_Location(new Point(70, 30));
			((Control)cmbCatalogue).set_Name("cmbCatalogue");
			((Control)cmbCatalogue).set_Size(new Size(126, 21));
			((Control)cmbCatalogue).set_TabIndex(1);
			cmbCatalogue.add_SelectedIndexChanged((EventHandler)cmbCatalogue_SelectedIndexChanged);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(9, 33));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(55, 13));
			((Control)label3).set_TabIndex(0);
			((Control)label3).set_Text("Catalogue");
			((Control)panelUCAC4).get_Controls().Add((Control)(object)label41);
			((Control)panelUCAC4).get_Controls().Add((Control)(object)txtU4Number);
			((Control)panelUCAC4).get_Controls().Add((Control)(object)txtU4Zone);
			((Control)panelUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelUCAC4).set_Location(new Point(62, 57));
			((Control)panelUCAC4).set_Name("panelUCAC4");
			((Control)panelUCAC4).set_Size(new Size(141, 33));
			((Control)panelUCAC4).set_TabIndex(36);
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(48, 10));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(11, 13));
			((Control)label41).set_TabIndex(2);
			((Control)label41).set_Text("-");
			((Control)txtU4Number).set_Location(new Point(60, 7));
			((Control)txtU4Number).set_Name("txtU4Number");
			((Control)txtU4Number).set_Size(new Size(70, 20));
			((Control)txtU4Number).set_TabIndex(1);
			((Control)txtU4Zone).set_Location(new Point(16, 7));
			((Control)txtU4Zone).set_Name("txtU4Zone");
			((Control)txtU4Zone).set_Size(new Size(31, 20));
			((Control)txtU4Zone).set_TabIndex(0);
			((Control)panelNOMAD).get_Controls().Add((Control)(object)label40);
			((Control)panelNOMAD).get_Controls().Add((Control)(object)txtNOMADnumber);
			((Control)panelNOMAD).get_Controls().Add((Control)(object)txtNOMADzone);
			((Control)panelNOMAD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelNOMAD).set_Location(new Point(62, 57));
			((Control)panelNOMAD).set_Name("panelNOMAD");
			((Control)panelNOMAD).set_Size(new Size(141, 33));
			((Control)panelNOMAD).set_TabIndex(35);
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label40).set_Location(new Point(48, 10));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(11, 13));
			((Control)label40).set_TabIndex(2);
			((Control)label40).set_Text("-");
			((Control)txtNOMADnumber).set_Location(new Point(60, 7));
			((Control)txtNOMADnumber).set_Name("txtNOMADnumber");
			((Control)txtNOMADnumber).set_Size(new Size(70, 20));
			((Control)txtNOMADnumber).set_TabIndex(1);
			((Control)txtNOMADzone).set_Location(new Point(16, 7));
			((Control)txtNOMADzone).set_Name("txtNOMADzone");
			((Control)txtNOMADzone).set_Size(new Size(31, 20));
			((Control)txtNOMADzone).set_TabIndex(0);
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycComp);
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycSeqNum);
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycRegion);
			((Control)panelTycho2).get_Controls().Add((Control)(object)label15);
			((Control)panelTycho2).get_Controls().Add((Control)(object)label16);
			((Control)panelTycho2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelTycho2).set_Location(new Point(62, 57));
			((Control)panelTycho2).set_Name("panelTycho2");
			((Control)panelTycho2).set_Size(new Size(142, 33));
			((Control)panelTycho2).set_TabIndex(33);
			((Control)txtTycComp).set_Location(new Point(111, 7));
			((Control)txtTycComp).set_Name("txtTycComp");
			((Control)txtTycComp).set_Size(new Size(17, 20));
			((Control)txtTycComp).set_TabIndex(2);
			((Control)txtTycComp).set_Text("1");
			((Control)txtTycSeqNum).set_Location(new Point(56, 7));
			((Control)txtTycSeqNum).set_Name("txtTycSeqNum");
			((Control)txtTycSeqNum).set_Size(new Size(47, 20));
			((Control)txtTycSeqNum).set_TabIndex(1);
			((Control)txtTycRegion).set_Location(new Point(8, 7));
			((Control)txtTycRegion).set_Name("txtTycRegion");
			((Control)txtTycRegion).set_Size(new Size(39, 20));
			((Control)txtTycRegion).set_TabIndex(0);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(101, 10));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(11, 13));
			((Control)label15).set_TabIndex(0);
			((Control)label15).set_Text("-");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(46, 10));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(11, 13));
			((Control)label16).set_TabIndex(4);
			((Control)label16).set_Text("-");
			((Control)panelHip).get_Controls().Add((Control)(object)txtHip);
			((Control)panelHip).set_Location(new Point(62, 57));
			((Control)panelHip).set_Name("panelHip");
			((Control)panelHip).set_Size(new Size(141, 33));
			((Control)panelHip).set_TabIndex(30);
			((Control)txtHip).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtHip).set_Location(new Point(25, 7));
			((Control)txtHip).set_Name("txtHip");
			((Control)txtHip).set_Size(new Size(70, 20));
			((Control)txtHip).set_TabIndex(2);
			((Control)panelB1).get_Controls().Add((Control)(object)label17);
			((Control)panelB1).get_Controls().Add((Control)(object)txtB1number);
			((Control)panelB1).get_Controls().Add((Control)(object)txtB1zone);
			((Control)panelB1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelB1).set_Location(new Point(62, 57));
			((Control)panelB1).set_Name("panelB1");
			((Control)panelB1).set_Size(new Size(141, 33));
			((Control)panelB1).set_TabIndex(32);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(48, 10));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(11, 13));
			((Control)label17).set_TabIndex(2);
			((Control)label17).set_Text("-");
			((Control)txtB1number).set_Location(new Point(60, 7));
			((Control)txtB1number).set_Name("txtB1number");
			((Control)txtB1number).set_Size(new Size(70, 20));
			((Control)txtB1number).set_TabIndex(1);
			((Control)txtB1zone).set_Location(new Point(8, 7));
			((Control)txtB1zone).set_Name("txtB1zone");
			((Control)txtB1zone).set_Size(new Size(39, 20));
			((Control)txtB1zone).set_TabIndex(0);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(18, 67));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(44, 13));
			((Control)label18).set_TabIndex(29);
			((Control)label18).set_Text("Number");
			((Control)cmdGetEquivalents).set_Location(new Point(261, 13));
			((Control)cmdGetEquivalents).set_Name("cmdGetEquivalents");
			((Control)cmdGetEquivalents).set_Size(new Size(109, 22));
			((Control)cmdGetEquivalents).set_TabIndex(7);
			((Control)cmdGetEquivalents).set_Text("Get IDs from coords");
			((ButtonBase)cmdGetEquivalents).set_UseVisualStyleBackColor(true);
			((Control)cmdGetEquivalents).add_Click((EventHandler)cmdGetEquivalents_Click);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Location(new Point(325, 65));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(20, 13));
			((Control)label24).set_TabIndex(22);
			((Control)label24).set_Text("K2");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Location(new Point(320, 40));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(25, 13));
			((Control)label33).set_TabIndex(32);
			((Control)label33).set_Text("Tyc");
			((Control)txtK2num).set_Location(new Point(346, 61));
			((Control)txtK2num).set_Name("txtK2num");
			((TextBoxBase)txtK2num).set_ReadOnly(true);
			((Control)txtK2num).set_Size(new Size(80, 20));
			((Control)txtK2num).set_TabIndex(11);
			((Control)txtTYC).set_Location(new Point(346, 36));
			((Control)txtTYC).set_Name("txtTYC");
			((TextBoxBase)txtTYC).set_ReadOnly(true);
			((Control)txtTYC).set_Size(new Size(76, 20));
			((Control)txtTYC).set_TabIndex(9);
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Location(new Point(219, 65));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(29, 13));
			((Control)label32).set_TabIndex(30);
			((Control)label32).set_Text("SAO");
			((Control)txtSAO).set_Location(new Point(250, 61));
			((Control)txtSAO).set_Name("txtSAO");
			((TextBoxBase)txtSAO).set_ReadOnly(true);
			((Control)txtSAO).set_Size(new Size(49, 20));
			((Control)txtSAO).set_TabIndex(10);
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Location(new Point(225, 40));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(23, 13));
			((Control)label31).set_TabIndex(28);
			((Control)label31).set_Text("Hip");
			((Control)txtHipparcos).set_Location(new Point(250, 36));
			((Control)txtHipparcos).set_Name("txtHipparcos");
			((TextBoxBase)txtHipparcos).set_ReadOnly(true);
			((Control)txtHipparcos).set_Size(new Size(44, 20));
			((Control)txtHipparcos).set_TabIndex(8);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Location(new Point(151, 13));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(13, 13));
			((Control)label21).set_TabIndex(48);
			((Control)label21).set_Text("o");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(111, 13));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(12, 13));
			((Control)label20).set_TabIndex(47);
			((Control)label20).set_Text("s");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Location(new Point(92, 13));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(15, 13));
			((Control)label19).set_TabIndex(46);
			((Control)label19).set_Text("m");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(77, 13));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(13, 13));
			((Control)label5).set_TabIndex(45);
			((Control)label5).set_Text("h");
			((Control)cmdGetCoords).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetCoords).set_Location(new Point(4, 24));
			((Control)cmdGetCoords).set_Name("cmdGetCoords");
			((Control)cmdGetCoords).set_Size(new Size(60, 32));
			((Control)cmdGetCoords).set_TabIndex(0);
			((Control)cmdGetCoords).set_Text("Get coords \r\nfrom Star");
			((ButtonBase)cmdGetCoords).set_UseVisualStyleBackColor(true);
			((Control)cmdGetCoords).add_Click((EventHandler)cmdGetCoords_Click);
			((Control)txtDd).set_Location(new Point(148, 30));
			((Control)txtDd).set_Name("txtDd");
			((Control)txtDd).set_Size(new Size(21, 20));
			((Control)txtDd).set_TabIndex(4);
			((Control)txtDm).set_Location(new Point(169, 30));
			((Control)txtDm).set_Name("txtDm");
			((Control)txtDm).set_Size(new Size(18, 20));
			((Control)txtDm).set_TabIndex(5);
			((Control)txtDs).set_Location(new Point(187, 30));
			((Control)txtDs).set_Name("txtDs");
			((Control)txtDs).set_Size(new Size(31, 20));
			((Control)txtDs).set_TabIndex(6);
			((Control)txtS).set_Location(new Point(105, 30));
			((Control)txtS).set_Name("txtS");
			((Control)txtS).set_Size(new Size(35, 20));
			((Control)txtS).set_TabIndex(3);
			((Control)txtM).set_Location(new Point(88, 30));
			((Control)txtM).set_Name("txtM");
			((Control)txtM).set_Size(new Size(17, 20));
			((Control)txtM).set_TabIndex(2);
			((Control)txtH).set_Location(new Point(68, 30));
			((Control)txtH).set_Name("txtH");
			((Control)txtH).set_Size(new Size(20, 20));
			((Control)txtH).set_TabIndex(1);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(193, 18));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(12, 13));
			((Control)label23).set_TabIndex(50);
			((Control)label23).set_Text("\"");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(175, 18));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(9, 13));
			((Control)label22).set_TabIndex(49);
			((Control)label22).set_Text("'");
			((Control)groupBox5).get_Controls().Add((Control)(object)txtH);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdGetEquivalents);
			((Control)groupBox5).get_Controls().Add((Control)(object)label24);
			((Control)groupBox5).get_Controls().Add((Control)(object)label33);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdGetCoords);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtK2num);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtTYC);
			((Control)groupBox5).get_Controls().Add((Control)(object)label32);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtSAO);
			((Control)groupBox5).get_Controls().Add((Control)(object)label31);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtHipparcos);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtM);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtDd);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtS);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtDm);
			((Control)groupBox5).get_Controls().Add((Control)(object)txtDs);
			((Control)groupBox5).get_Controls().Add((Control)(object)label21);
			((Control)groupBox5).get_Controls().Add((Control)(object)label20);
			((Control)groupBox5).get_Controls().Add((Control)(object)label19);
			((Control)groupBox5).get_Controls().Add((Control)(object)label5);
			((Control)groupBox5).get_Controls().Add((Control)(object)label22);
			((Control)groupBox5).get_Controls().Add((Control)(object)label23);
			((Control)groupBox5).set_Enabled(false);
			((Control)groupBox5).set_Location(new Point(15, 440));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(453, 86));
			((Control)groupBox5).set_TabIndex(4);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("Star ID's   (from coordinates)");
			((Control)groupBox5).set_Visible(false);
			((Control)cmdCancel).set_BackColor(Color.LightCoral);
			((Control)cmdCancel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCancel).set_Location(new Point(381, 393));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(78, 42));
			((Control)cmdCancel).set_TabIndex(3);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(false);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)panel1).set_BackColor(Color.SeaShell);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)panel6);
			((Control)panel1).get_Controls().Add((Control)(object)panel5);
			((Control)panel1).get_Controls().Add((Control)(object)panel4);
			((Control)panel1).set_Location(new Point(217, 43));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(246, 330));
			((Control)panel1).set_TabIndex(1);
			((Control)panel6).set_BackColor(Color.Cornsilk);
			panel6.set_BorderStyle((BorderStyle)2);
			((Control)panel6).get_Controls().Add((Control)(object)txtAsteroidName);
			((Control)panel6).get_Controls().Add((Control)(object)txtAsteroidNumber);
			((Control)panel6).get_Controls().Add((Control)(object)label29);
			((Control)panel6).get_Controls().Add((Control)(object)label4);
			((Control)panel6).set_Location(new Point(11, 118));
			((Control)panel6).set_Name("panel6");
			((Control)panel6).set_Size(new Size(226, 75));
			((Control)panel6).set_TabIndex(3);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(72, 3));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(79, 15));
			((Control)label29).set_TabIndex(9);
			((Control)label29).set_Text("3.  Asteroid");
			((Control)panel5).set_BackColor(Color.Cornsilk);
			panel5.set_BorderStyle((BorderStyle)2);
			((Control)panel5).get_Controls().Add((Control)(object)panelGstar);
			((Control)panel5).get_Controls().Add((Control)(object)label18);
			((Control)panel5).get_Controls().Add((Control)(object)label30);
			((Control)panel5).get_Controls().Add((Control)(object)panelUCAC4);
			((Control)panel5).get_Controls().Add((Control)(object)cmbCatalogue);
			((Control)panel5).get_Controls().Add((Control)(object)panelNOMAD);
			((Control)panel5).get_Controls().Add((Control)(object)label3);
			((Control)panel5).get_Controls().Add((Control)(object)panelTycho2);
			((Control)panel5).get_Controls().Add((Control)(object)panelB1);
			((Control)panel5).get_Controls().Add((Control)(object)panelHip);
			((Control)panel5).set_Location(new Point(11, 201));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(226, 118));
			((Control)panel5).set_TabIndex(4);
			((Control)panelGstar).get_Controls().Add((Control)(object)label36);
			((Control)panelGstar).set_Location(new Point(62, 58));
			((Control)panelGstar).set_Name("panelGstar");
			((Control)panelGstar).set_Size(new Size(141, 33));
			((Control)panelGstar).set_TabIndex(37);
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Location(new Point(7, 10));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(113, 13));
			((Control)label36).set_TabIndex(0);
			((Control)label36).set_Text("No details are required");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(85, 3));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(53, 15));
			((Control)label30).set_TabIndex(9);
			((Control)label30).set_Text("4.  Star");
			((Control)panel4).set_BackColor(Color.Cornsilk);
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)chKCorrectDate);
			((Control)panel4).get_Controls().Add((Control)(object)label28);
			((Control)panel4).get_Controls().Add((Control)(object)label7);
			((Control)panel4).get_Controls().Add((Control)(object)cmbMth);
			((Control)panel4).get_Controls().Add((Control)(object)label6);
			((Control)panel4).get_Controls().Add((Control)(object)label2);
			((Control)panel4).get_Controls().Add((Control)(object)cmbDay);
			((Control)panel4).get_Controls().Add((Control)(object)updnYear);
			((Control)panel4).set_Location(new Point(11, 11));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(226, 98));
			((Control)panel4).set_TabIndex(2);
			chKCorrectDate.set_AutoCheck(false);
			((Control)chKCorrectDate).set_AutoSize(true);
			((Control)chKCorrectDate).set_Location(new Point(43, 72));
			((Control)chKCorrectDate).set_Name("chKCorrectDate");
			((Control)chKCorrectDate).set_Size(new Size(137, 17));
			((Control)chKCorrectDate).set_TabIndex(5);
			((Control)chKCorrectDate).set_Text("Confirm - date is correct");
			((ButtonBase)chKCorrectDate).set_UseVisualStyleBackColor(true);
			((Control)chKCorrectDate).add_Click((EventHandler)chKCorrectDate_Click);
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(28, 3));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(166, 15));
			((Control)label28).set_TabIndex(8);
			((Control)label28).set_Text("2.  Date of the event - UT");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(469, 463));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)groupBox5);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("AsteroidLC_DataInput");
			((Form)this).set_StartPosition((FormStartPosition)4);
			((Control)this).set_Text("Enter event details");
			((Form)this).add_Load((EventHandler)AsteroidLC_DataInput_Load);
			((ISupportInitialize)updnYear).EndInit();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)panel9).ResumeLayout(false);
			((Control)panel8).ResumeLayout(false);
			((Control)panel8).PerformLayout();
			((Control)panel7).ResumeLayout(false);
			((Control)panel7).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panelUCAC4).ResumeLayout(false);
			((Control)panelUCAC4).PerformLayout();
			((Control)panelNOMAD).ResumeLayout(false);
			((Control)panelNOMAD).PerformLayout();
			((Control)panelTycho2).ResumeLayout(false);
			((Control)panelTycho2).PerformLayout();
			((Control)panelHip).ResumeLayout(false);
			((Control)panelHip).PerformLayout();
			((Control)panelB1).ResumeLayout(false);
			((Control)panelB1).PerformLayout();
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel6).ResumeLayout(false);
			((Control)panel6).PerformLayout();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((Control)panelGstar).ResumeLayout(false);
			((Control)panelGstar).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
