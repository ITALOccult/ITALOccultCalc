using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using AOTA;
using LightCurves;
using Occult.Mapping;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult.Lunar_Observations
{
	public class ObservationsEditor : Form
	{
		private string Current_OCC_File = "";

		private bool UpdatingData;

		private bool ChangingStar;

		private const double Radian = 180.0 / Math.PI;

		private IContainer components;

		private ListBox lstSites;

		private ListBox lstNames;

		private GroupBox grpSites;

		private GroupBox grpNames;

		private GroupBox grpEvents;

		private Panel panelDDD;

		private TextBox txtLongDDD;

		private TextBox txtLatDDD;

		private Panel panelDMM;

		private TextBox txtLongDmm;

		private TextBox txtLatMM;

		private TextBox txtLatDmm;

		private TextBox txtLongMM;

		private TextBox txtAlt_ft;

		private Panel panelDMS;

		private TextBox txtLongDeg;

		private TextBox txtLatSec;

		private TextBox txtLatMin;

		private TextBox txtLatDeg;

		private TextBox txtLongSec;

		private TextBox txtLongMin;

		private Label label20;

		private Label label19;

		private Label label18;

		private Label label17;

		private Label label16;

		private Label label15;

		private Panel panel2;

		private RadioButton optFeet;

		private RadioButton optMeters;

		private Panel panel1;

		private RadioButton optDMM;

		private RadioButton optDDD;

		private RadioButton optDMS;

		private ComboBox cmbDatum;

		private ComboBox cmbTelescope;

		private TextBox txtAperture;

		private TextBox txtAlt_m;

		private Label label3;

		private Label label1;

		private Label label2;

		private TextBox txtFocalLength;

		private Label label5;

		private ComboBox cmbMount;

		private Label label4;

		private ComboBox cmbDrive;

		private Panel panelHeightDatum;

		private RadioButton optWGS;

		private RadioButton optMSL;

		private Button cmdDeleteSites;

		private Button cmdReplaceSites;

		private Button cmdAddSites;

		private GroupBox grpILOCstations;

		private Label label7;

		private Label label6;

		private TextBox txtILOCTelescope;

		private TextBox txtILOCStation;

		private Button cmdRenumberSites;

		private Button cmdDefaultSite;

		private MenuStrip menuStrip1;

		private Panel panel3;

		private RadioButton optSites;

		private Label label8;

		private GroupBox groupBox3;

		private GroupBox groupBox2;

		private ToolStripMenuItem fileToolStripMenuItem;

		private RadioButton optHeader;

		private ToolStripMenuItem openToolStripMenuItem;

		private ToolStripMenuItem importToolStripMenuItem;

		private ToolStripSeparator toolStripMenuItem1;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem saveAsToolStripMenuItem;

		private GroupBox grpHeader;

		private TextBox txtAddress;

		private TextBox txtReported;

		private TextBox txtEmail;

		private TextBox txtRepresentative;

		private TextBox txtPlace;

		private Label label14;

		private Label label13;

		private Label label12;

		private Label label11;

		private Label label9;

		private Button cmdSetPlace;

		private Button cmdGetAddress;

		private Button cmdGetEMail;

		private Button cmdGetRep;

		private Button cmdGetReported;

		private Button cmdSetAddress;

		private Button cmdSetEmail;

		private Button cmdSetRep;

		private Button cmdSetReported;

		private Button cmdGetPlace;

		private Label label10;

		private RadioButton optView;

		private Label label22;

		private Label label21;

		private ListBox lstReport;

		private ToolStripMenuItem newToolStripMenuItem;

		private ToolStripSeparator toolStripMenuItem2;

		private Button cmdAddDefaultSite;

		private GroupBox grpSortSites;

		private Button cmdSortByLatitude;

		private Button cmdSortByLongitude;

		private Button cmdSortByNumber;

		private GroupBox grpILOCnames;

		private Label label23;

		private Label label24;

		private TextBox txtILOCName;

		private TextBox txtILOCStation_Name;

		private GroupBox groupBox5;

		private Button cmdSortByFamilyName;

		private Button cmdSortByName;

		private Button cmdSortByName_Number;

		private Button cmdRenumberNames;

		private Button cmdInsertDefaultName;

		private Button cmdSetDefaultName;

		private Button cmdDeleteName;

		private Button cmdReplaceName;

		private Button cmdAddName;

		private Label label25;

		private TextBox txtName;

		private Label label26;

		private GroupBox groupBox8;

		private GroupBox grpMethods;

		private GroupBox grpObserver;

		private GroupBox groupBox6;

		private NumericUpDown updnMonth;

		private NumericUpDown updnDay;

		private NumericUpDown updnHour;

		private NumericUpDown updnMin;

		private NumericUpDown updnYear;

		private TextBox txtSecond;

		private Label label28;

		private Label label27;

		private CheckBox chkGraze;

		private RadioButton optUmbra;

		private RadioButton optBrightLimb;

		private RadioButton optDarkLimb;

		private RadioButton optReappear;

		private RadioButton optFailed;

		private RadioButton optStarted;

		private RadioButton optMiss;

		private RadioButton optStopped;

		private RadioButton optFlash;

		private RadioButton optBlink;

		private RadioButton optDisappear;

		private Button cmdToday;

		private Label label34;

		private Label label33;

		private Label label32;

		private Label label31;

		private Label label30;

		private Label label29;

		private Label label35;

		private ComboBox cmbCertainty;

		private ComboBox cmbPE;

		private ComboBox cmbTimeKeeping;

		private ComboBox cmbMethod2;

		private ComboBox cmbMethod1;

		private ComboBox cmbRemarkable;

		private ComboBox cmbStability;

		private ComboBox cmbTransparency;

		private Label label47;

		private Label label46;

		private Label label45;

		private Label label44;

		private Label label43;

		private Label label42;

		private Label label41;

		private Label label40;

		private Label label39;

		private Label label38;

		private Label label37;

		private Label label36;

		private TextBox txtPE;

		private TextBox txtSN;

		private TextBox txtAccuracy;

		private TextBox txtComments;

		private Label label48;

		private Label label51;

		private Label label50;

		private Label label49;

		private TextBox txtZC;

		private TextBox txtSAO;

		private TextBox txtMag;

		private TextBox txtPA;

		private TextBox txtResidual;

		private Button cmdRenumberEvents;

		private Button cmdDeleteEvent;

		private Button cmdReplaceEvent;

		private Button cmdSortEvents;

		private Button cmdAddEvent;

		private RadioButton optByOBS;

		private RadioButton optByTEL;

		private RadioButton optByNumber;

		private Label label52;

		private TextBox txtGSC;

		private Label lblSAOValid;

		private GroupBox grpILOCevents;

		private TextBox txtILOCEventObserver;

		private TextBox txtILOCEventRecorder;

		private TextBox txtILOCEventTelescope;

		private TextBox txtILOCEventStation;

		private ComboBox cmbObservers;

		private ComboBox cmbRecorders;

		private ComboBox cmbSites;

		private Button cmdIdentify;

		private Label label55;

		private Label label54;

		private Label label53;

		private Label label56;

		internal TextBox txtXZ;

		private RadioButton optSortEventsByDate;

		private Label label60;

		private Label label63;

		private Label txtRecorder;

		private Label label61;

		private Button cmdMoveSiteDown;

		private Label label64;

		private Button cmdMoveSiteUp;

		private Button cmdMoveNameDown;

		private Label label65;

		private Button cmdMoveNameUp;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem addGrazeToRecentGrazesFileToolStripMenuItem;

		private ToolStripMenuItem pasteToolStripMenuItem;

		private ToolStripMenuItem pasteMergeToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripMenuItem addGrazeToMainFileOfGrazesToolStripMenuItem;

		private ToolStripMenuItem displayOnGoogleEarthToolStripMenuItem;

		private Panel panel4;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem saveoldFormatToolStripMenuItem;

		private ToolStripMenuItem saveAsoldFormatToolStripMenuItem;

		private CheckBox chkFormat;

		private Label label66;

		private TextBox txtDuration;

		private Label label67;

		private ComboBox cmbLightLevel;

		private Panel panelReportedTo;

		private Panel panelAddress;

		private Panel panelRepresentative;

		private Label label68;

		private Label lblCharsLeft;

		private Label label69;

		private Label lblCharsLeftName;

		private Label label70;

		private Label label57;

		internal TextBox txtAsteroid;

		private Label label58;

		private Label label59;

		private ComboBox cmbMoons;

		private ComboBox cmbPlanets;

		private Label label71;

		private GroupBox grpPlanets;

		private Label lblWDS;

		private GroupBox grpEditControl;

		private CheckBox chkHighPrecision;

		internal ToolTip toolTip;

		private Label label62;

		private Label label74;

		private Label label75;

		private ComboBox cmbWDS;

		private Label lblSep;

		private Label lblPA;

		private CheckBox chkUnidentified;

		private ComboBox cmbTemp;

		private ToolStripMenuItem withReportToolStripMenuItem;

		private ToolStripMenuItem submitToolStripMenuItem;

		private ToolStripMenuItem downloadCurrentEmailAddressesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator4;

		private Label label73;

		private TextBox txtMessage;

		private Label label76;

		private TextBox txtObserverEmail;

		private Label label77;

		private ToolStripMenuItem verifyDataEntryToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator5;

		internal ComboBox cmbDoubles;

		private ToolStripMenuItem identifyUnidentifiedStarsInReportToolStripMenuItem;

		internal ListBox lstEvents;

		internal RadioButton optEvents;

		private ToolStripMenuItem listPossibleDoubleStarsToolStripMenuItem;

		private ToolStripMenuItem verifyAccuracyValuesToolStripMenuItem;

		private ToolStripMenuItem doAllToolStripMenuItem;

		private ToolStripMenuItem checkTemperatureValuesToolStripMenuItem;

		private ToolStripMenuItem displayCurrentSitesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator6;

		private ToolStripMenuItem plotMoonFirst10EventsToolStripMenuItem;

		private ToolStripMenuItem plotMoonAllEventsToolStripMenuItem;

		private ToolStripMenuItem plotMoonFirst20EventsToolStripMenuItem;

		private ToolStripMenuItem plotMoonFirst50EventsToolStripMenuItem;

		private ToolStripTextBox mnuGoogleHeight;

		private ToolStripSeparator toolStripSeparator7;

		private ToolStripSeparator toolStripSeparator8;

		private ToolStripMenuItem altitudemToUseInLimbPlotsToolStripMenuItem;

		private ToolStripMenuItem includeLimbProfile;

		private ToolStripMenuItem doubleStarReportToolStripMenuItem;

		private ToolStripMenuItem createReportForSelectedStarToolStripMenuItem;

		private Button cmdReduce;

		private ToolStripMenuItem liMovieToolStripMenuItem;

		private ToolStripMenuItem saveGraphicFromClipboardToolStripMenuItem;

		private PictureBox picLiMovie;

		private ToolStripMenuItem copyReportToolStripMenuItem;

		private Button cmdGetDoubles;

		private Button cmdGetWDS;

		private ToolStripMenuItem selectTheBrighterComponentToolStripMenuItem;

		private Label lblLimbBasis;

		private Label lblIsDouble;

		private ToolStripMenuItem generalHelpToolStripMenuItem;

		private ToolStripMenuItem fresnelDiffractionToolStripMenuItem;

		private Label lblUnsavedEdits;

		private ToolStripMenuItem cameraCorrectionsToolStripMenuItem;

		private Button cmdCamera;

		private ToolStripMenuItem lightCurveReportToolStripMenuItem;

		private ToolStripMenuItem createLightCurveReportToolStripMenuItem;

		private Label lblK2Star;

		private ToolStripMenuItem emailLightCurveReportsToolStripMenuItem;

		private Label label72;

		private Label label80;

		private Label label79;

		private Label label78;

		private Label label81;

		public ObservationsEditor()
		{
			InitializeComponent();
			UpdatingData = true;
			((Control)txtAlt_ft).set_Top(((Control)txtAlt_m).get_Top());
			((Control)panelDDD).set_Top(((Control)panelDMS).get_Top());
			((Control)panelDMM).set_Top(((Control)panelDMS).get_Top());
			((Control)panelDDD).set_Left(((Control)panelDMS).get_Left());
			((Control)panelDMM).set_Left(((Control)panelDMS).get_Left());
			((ListControl)cmbDatum).set_SelectedIndex(1);
			((ListControl)cmbTelescope).set_SelectedIndex(2);
			((ListControl)cmbMount).set_SelectedIndex(0);
			((ListControl)cmbDrive).set_SelectedIndex(0);
			cmbMoons.get_Items().Clear();
			cmbMoons.get_Items().Add((object)"");
			GroupBox obj = grpSites;
			int left;
			((Control)grpEvents).set_Left(left = 10);
			((Control)obj).set_Left(left);
			((Control)grpNames).set_Left(630);
			GroupBox obj2 = grpSites;
			GroupBox obj3 = grpNames;
			GroupBox obj4 = grpEvents;
			int num;
			((Control)grpHeader).set_Top(num = 70);
			int num2;
			((Control)obj4).set_Top(num2 = num);
			((Control)obj3).set_Top(left = num2);
			((Control)obj2).set_Top(left);
			((Control)lstReport).set_Top(70);
			((Control)this).set_Height((int)(0.95 * (double)Screen.GetBounds((Control)(object)this).Height));
			if (((Control)this).get_Height() > 1100)
			{
				((Control)this).set_Height(1100);
			}
			((ToolStripItem)displayOnGoogleEarthToolStripMenuItem).set_Enabled(Settings.Default.GoogleEarthInstalled);
			((ToolStripItem)addGrazeToRecentGrazesFileToolStripMenuItem).set_Enabled(Settings.Default.AdministratorGlobal);
			((ToolStripItem)addGrazeToMainFileOfGrazesToolStripMenuItem).set_Enabled(Settings.Default.AdministratorGlobal);
			toolTip.SetToolTip((Control)(object)cmdSetPlace, "Current default = " + Settings.Default.ILOC_Place);
			toolTip.SetToolTip((Control)(object)cmdGetPlace, "Current default = " + Settings.Default.ILOC_Place);
			toolTip.SetToolTip((Control)(object)cmdSetEmail, "Current default = " + Settings.Default.ILOC_Email);
			toolTip.SetToolTip((Control)(object)cmdGetEMail, "Current default = " + Settings.Default.ILOC_Email);
			toolTip.SetToolTip((Control)(object)cmdSetRep, "Current default = " + Settings.Default.ILOC_Representative);
			toolTip.SetToolTip((Control)(object)cmdGetRep, "Current default = " + Settings.Default.ILOC_Representative);
			toolTip.SetToolTip((Control)(object)cmdSetAddress, "Current default = " + Settings.Default.ILOC_Address);
			toolTip.SetToolTip((Control)(object)cmdGetAddress, "Current default = " + Settings.Default.ILOC_Address);
			toolTip.SetToolTip((Control)(object)cmdSetReported, "Current default = " + Settings.Default.ILOC_ReportedTo);
			toolTip.SetToolTip((Control)(object)cmdGetReported, "Current default = " + Settings.Default.ILOC_ReportedTo);
			toolTip.SetToolTip((Control)(object)cmdDefaultSite, "Current default = " + Settings.Default.ILOC_DefaultSite);
			toolTip.SetToolTip((Control)(object)cmdAddDefaultSite, "Current default = " + Settings.Default.ILOC_DefaultSite);
			toolTip.SetToolTip((Control)(object)cmdSetDefaultName, "Current default = " + Settings.Default.ILOC_DefaultName);
			toolTip.SetToolTip((Control)(object)cmdInsertDefaultName, "Current default = " + Settings.Default.ILOC_DefaultName);
			ReSetForm();
			UpdatingData = false;
		}

		private void ObservationsEditor_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			SetEditFrame();
			AltitudeVisibility();
			DMSformatVisibility();
			if (Utilities.LOLAFileExists)
			{
				((Control)lblLimbBasis).set_Text("LRO-LOLA");
			}
			else
			{
				((Control)lblLimbBasis).set_Text("None");
			}
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
		}

		private void optMeters_CheckedChanged(object sender, EventArgs e)
		{
			AltitudeVisibility();
		}

		private void optFeet_CheckedChanged(object sender, EventArgs e)
		{
			AltitudeVisibility();
		}

		private void AltitudeVisibility()
		{
			((Control)txtAlt_m).set_Visible(optMeters.get_Checked());
			((Control)txtAlt_ft).set_Visible(!((Control)txtAlt_m).get_Visible());
		}

		private void txtAlt_m_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				if (!double.TryParse(((Control)txtAlt_m).get_Text(), out var result))
				{
					result = 0.0;
				}
				if (chkHighPrecision.get_Checked())
				{
					((Control)txtAlt_m).set_Text(string.Format("{0,1:F1}", result));
					((Control)txtAlt_ft).set_Text(string.Format("{0,1:F1}", result / 0.3048));
				}
				else
				{
					((Control)txtAlt_m).set_Text(string.Format("{0,1:F0}", result));
					((Control)txtAlt_ft).set_Text(string.Format("{0,1:F0}", result / 0.3048));
				}
				UpdatingData = false;
			}
		}

		private void txtAlt_ft_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				if (!double.TryParse(((Control)txtAlt_ft).get_Text(), out var result))
				{
					result = 0.0;
				}
				if (chkHighPrecision.get_Checked())
				{
					((Control)txtAlt_ft).set_Text(string.Format("{0,1:F1}", result));
					((Control)txtAlt_m).set_Text(string.Format("{0,1:F1}", result * 0.3048));
				}
				else
				{
					((Control)txtAlt_ft).set_Text(string.Format("{0,1:F0}", result));
					((Control)txtAlt_m).set_Text(string.Format("{0,1:F0}", result * 0.3048));
				}
				UpdatingData = false;
			}
		}

		private void optDMS_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void optDMM_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void optDDD_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void DMSformatVisibility()
		{
			((Control)panelDMS).set_Visible(optDMS.get_Checked());
			((Control)panelDMM).set_Visible(optDMM.get_Checked());
			((Control)panelDDD).set_Visible(optDDD.get_Checked());
		}

		private void txtLongDeg_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongMin_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongSec_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatDeg_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatMin_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatSec_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongDDD_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(2);
				UpdatingData = false;
			}
		}

		private void txtLatDDD_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(2);
				UpdatingData = false;
			}
		}

		private void txtLongDmm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLongMM_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLatDmm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLatMM_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(1);
				UpdatingData = false;
			}
		}

		private void UpdateLongitudes(int style)
		{
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			bool flag = false;
			int num;
			switch (style)
			{
			case 0:
				flag = ((Control)txtLongDeg).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDeg).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLongMin).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLongSec).get_Text().Replace('-', ' '), out result3))
				{
					result3 = 0.0;
				}
				num = Utilities.DecimalPlaces(((Control)txtLongSec).get_Text());
				if ((num > 1) & !chkHighPrecision.get_Checked())
				{
					num = 1;
				}
				break;
			case 1:
				flag = ((Control)txtLongDmm).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDmm).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLongMM).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				num = Utilities.DecimalPlaces(((Control)txtLongMM).get_Text());
				num = ((num > 2) ? ((!((num > 3) & chkHighPrecision.get_Checked())) ? 1 : 2) : 0);
				break;
			default:
				flag = ((Control)txtLongDDD).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDDD).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				num = Utilities.DecimalPlaces(((Control)txtLongDDD).get_Text());
				num = ((num > 4) ? ((!((num > 5) & chkHighPrecision.get_Checked())) ? 1 : 2) : 0);
				break;
			}
			double num2 = result + result2 / 60.0 + result3 / 3600.0;
			if (flag)
			{
				num2 = 0.0 - num2;
			}
			string text = Utilities.DEGtoDMS(num2, 4, num, MinutesOnly: false);
			((Control)txtLongDeg).set_Text(text.Substring(0, 4).Replace(" ", ""));
			((Control)txtLongMin).set_Text(text.Substring(5, 2).Replace(" ", ""));
			((Control)txtLongSec).set_Text(text.Substring(8).Replace(" ", ""));
			text = Utilities.DEGtoDMS(num2, 4, num + 2, MinutesOnly: true);
			((Control)txtLongDmm).set_Text(text.Substring(0, 4).Replace(" ", ""));
			((Control)txtLongMM).set_Text(text.Substring(5).Replace(" ", ""));
			((Control)txtLongDDD).set_Text(string.Format("{0,2:F" + (num + 4) + "}", num2));
		}

		private void UpdateLatitudes(int style)
		{
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			bool flag = false;
			int num;
			switch (style)
			{
			case 0:
				flag = ((Control)txtLatDeg).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDeg).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatMin).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLatSec).get_Text().Replace('-', ' '), out result3))
				{
					result3 = 0.0;
				}
				num = Utilities.DecimalPlaces(((Control)txtLatSec).get_Text());
				if ((num > 1) & !chkHighPrecision.get_Checked())
				{
					num = 1;
				}
				break;
			case 1:
				flag = ((Control)txtLatDmm).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDmm).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatMM).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				num = Utilities.DecimalPlaces(((Control)txtLatMM).get_Text());
				num = ((num > 2) ? ((!((num > 3) & chkHighPrecision.get_Checked())) ? 1 : 2) : 0);
				break;
			default:
				flag = ((Control)txtLatDDD).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDDD).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				num = Utilities.DecimalPlaces(((Control)txtLatDDD).get_Text());
				num = ((num > 4) ? ((!((num > 5) & chkHighPrecision.get_Checked())) ? 1 : 2) : 0);
				break;
			}
			double num2 = result + result2 / 60.0 + result3 / 3600.0;
			if (flag)
			{
				num2 = 0.0 - num2;
			}
			string text = Utilities.DEGtoDMS(num2, 3, num, MinutesOnly: false);
			((Control)txtLatDeg).set_Text(text.Substring(0, 3).Replace(" ", ""));
			((Control)txtLatMin).set_Text(text.Substring(4, 2).Replace(" ", ""));
			((Control)txtLatSec).set_Text(text.Substring(7).Replace(" ", ""));
			text = Utilities.DEGtoDMS(num2, 3, num + 2, MinutesOnly: true);
			((Control)txtLatDmm).set_Text(text.Substring(0, 3).Replace(" ", ""));
			((Control)txtLatMM).set_Text(text.Substring(4).Replace(" ", ""));
			((Control)txtLatDDD).set_Text(string.Format("{0,2:F" + (num + 4) + "}", num2));
		}

		private void optSites_CheckedChanged(object sender, EventArgs e)
		{
			SetEditFrame();
		}

		private void optNames_CheckedChanged(object sender, EventArgs e)
		{
			SetEditFrame();
		}

		private void optEvents_CheckedChanged(object sender, EventArgs e)
		{
			SetEditFrame();
		}

		private void optHeader_CheckedChanged(object sender, EventArgs e)
		{
			SetEditFrame();
		}

		internal void SetEditFrame()
		{
			GroupBox obj = grpSites;
			bool @checked;
			((Control)grpNames).set_Visible(@checked = optSites.get_Checked());
			((Control)obj).set_Visible(@checked);
			((Control)grpEvents).set_Visible(optEvents.get_Checked());
			((Control)grpHeader).set_Visible(optHeader.get_Checked());
			((Control)lstReport).set_Visible(optView.get_Checked());
			ToolStripMenuItem obj2 = doubleStarReportToolStripMenuItem;
			((ToolStripItem)lightCurveReportToolStripMenuItem).set_Enabled(@checked = optEvents.get_Checked());
			((ToolStripItem)obj2).set_Enabled(@checked);
			((ToolStripItem)createLightCurveReportToolStripMenuItem).set_Enabled(optEvents.get_Checked() & (((ListControl)lstEvents).get_SelectedIndex() >= 0));
		}

		private void openToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			//IL_0060: Unknown result type (might be due to invalid IL or missing references)
			//IL_0066: Invalid comparison between Unknown and I4
			if (!UnsavedDataNeedsToBeSaved())
			{
				OpenFileDialog val = new OpenFileDialog();
				((FileDialog)val).set_Title("Specify file to READ lunar observations from.");
				((FileDialog)val).set_Filter("DAT file (*.dat)|*.dat|Graze files (*.grz)|*.grz|Text files (*.txt)|*.txt|All files (*.*)|*.*");
				((FileDialog)val).set_FilterIndex(Settings.Default.OCC_File_LastIndex);
				((FileDialog)val).set_FileName(Settings.Default.OCC_File_LastName);
				try
				{
					((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.OCC_File_LastName));
				}
				catch
				{
				}
				if ((int)((CommonDialog)val).ShowDialog() == 1)
				{
					ReSetForm();
					Current_OCC_File = ((FileDialog)val).get_FileName();
					Settings.Default.OCC_File_LastName = Current_OCC_File;
					Settings.Default.OCC_File_LastIndex = ((FileDialog)val).get_FilterIndex();
					((Control)this).set_Text("Observations editor : " + Path.GetFileName(Current_OCC_File) + " : Occult v." + Utilities.OccultVersion_Short);
					LunarObservations.OccMain.ReadReport(Current_OCC_File);
					DisplayReport(AllForms: true, HighlightLast: true);
					ToolStripMenuItem obj2 = importToolStripMenuItem;
					bool enabled;
					((ToolStripItem)pasteMergeToolStripMenuItem).set_Enabled(enabled = true);
					((ToolStripItem)obj2).set_Enabled(enabled);
					LunarObservations.CurrentSourceFile = Path.GetFileName(Current_OCC_File);
					LunarObservations.VerifyReportData(ShowIfNoErrors: false);
					((Control)lblUnsavedEdits).set_Visible(enabled = false);
					LunarObservations.EditsNotSaved = enabled;
					optEvents.set_Checked(true);
					SetEditFrame();
				}
			}
		}

		private void importToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0014: Expected O, but got Unknown
			//IL_0065: Unknown result type (might be due to invalid IL or missing references)
			//IL_006b: Invalid comparison between Unknown and I4
			string Line = "";
			string Line2 = "";
			int num = 0;
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Specify file to IMPORT lunar observations from.");
			((FileDialog)val).set_Filter("DAT file (*.dat)|*.dat|Graze files (*.grz)|*.grz|Text files (*.txt)|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(Settings.Default.OCC_File_LastIndex);
			((FileDialog)val).set_FileName(Settings.Default.OCC_File_LastName);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.OCC_File_LastName));
			}
			catch
			{
			}
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			EventLine.UseOldFormat = chkFormat.get_Checked();
			TelescopeLine.UseOldFormat = chkFormat.get_Checked();
			ObserverLine.UseOldFormat = chkFormat.get_Checked();
			LunarObservations.OccMerge.Events.Clear();
			LunarObservations.OccMerge.Observers.Clear();
			LunarObservations.OccMerge.Telescopes.Clear();
			((Control)this).set_Text(((Control)this).get_Text() + " + " + Path.GetFileName(((FileDialog)val).get_FileName()));
			LunarObservations.OccMerge.ReadReport(((FileDialog)val).get_FileName());
			LunarObservations.OccMain.ReNumberSites("A");
			LunarObservations.OccMain.ReNumberNames("A");
			LunarObservations.OccMerge.ReNumberSites(LunarObservations.OccMain.GetNextSiteID());
			LunarObservations.OccMerge.ReNumberNames(LunarObservations.OccMain.GetNextNameID());
			num = LunarObservations.OccMain.Telescopes.Count;
			for (int i = 0; i < LunarObservations.OccMerge.Telescopes.Count; i++)
			{
				LunarObservations.OccMain.AddNewTelescopeLine(LunarObservations.OccMerge.Telescopes[i].ToString());
				LunarObservations.OccMain.Telescopes[num + i].TelescopePlace = LunarObservations.OccMerge.Place;
			}
			for (int j = 0; j < LunarObservations.OccMerge.Observers.Count; j++)
			{
				LunarObservations.OccMain.AddNewObserverLine(LunarObservations.OccMerge.Observers[j].ToString());
			}
			for (int k = 0; k < LunarObservations.OccMerge.Events.Count; k++)
			{
				LunarObservations.OccMerge.Events[k].GetEventLines(out Line, out Line2);
				if (Line2.Trim().Length < 1)
				{
					Line2 = "";
				}
				LunarObservations.OccMain.AddNewEventLine(Line, Line2);
			}
			EventLine.SortField = 3;
			LunarObservations.OccMain.Events.Sort();
			LunarObservations.OccMain.ReNumberEvents();
			DisplayReport(AllForms: true, HighlightLast: false);
			DisplaySitesInEditor(0);
			LunarObservations.VerifyReportData(ShowIfNoErrors: false);
			LunarObservations.EditsNotSaved = true;
			verifyDataEntryToolStripMenuItem.set_Checked(false);
			identifyUnidentifiedStarsInReportToolStripMenuItem.set_Checked(false);
			listPossibleDoubleStarsToolStripMenuItem.set_Checked(false);
			verifyAccuracyValuesToolStripMenuItem.set_Checked(false);
		}

		private void saveoldFormatToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0047: Unknown result type (might be due to invalid IL or missing references)
			//IL_0065: Unknown result type (might be due to invalid IL or missing references)
			//IL_006b: Invalid comparison between Unknown and I4
			if ((LunarObservations.OccMain.Events.Count < 1) & (LunarObservations.OccMain.Telescopes.Count < 1) & (LunarObservations.OccMain.Observers.Count < 1))
			{
				MessageBox.Show("There are no observations, sites or observers to be saved", "No observations", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else if ((int)MessageBox.Show("Are you sure you want to save in the old Format?\r\n\r\nSome data may be lost", "Confirm old format", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				if (Current_OCC_File.Length < 1)
				{
					saveAsToolStripMenuItem_Click(sender, e);
				}
				else
				{
					Save_OCC_file(UseOldFormat: true);
				}
			}
		}

		private void saveAsoldFormatToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0047: Unknown result type (might be due to invalid IL or missing references)
			//IL_0065: Unknown result type (might be due to invalid IL or missing references)
			//IL_006b: Invalid comparison between Unknown and I4
			//IL_006e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0074: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c2: Invalid comparison between Unknown and I4
			if ((LunarObservations.OccMain.Events.Count < 1) & (LunarObservations.OccMain.Telescopes.Count < 1) & (LunarObservations.OccMain.Observers.Count < 1))
			{
				MessageBox.Show("There are no observations to be saved", "No observations", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else if ((int)MessageBox.Show("Are you sure you want to save in the old Format?\r\n\r\nSome data may be lost", "Confirm old format", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				SaveFileDialog val = new SaveFileDialog();
				((FileDialog)val).set_Title("Specify file to Write lunar observations to.");
				((FileDialog)val).set_Filter("DAT file (*.dat)|*.dat|Graze files (*.grz)|*.grz|Text files (*.txt)|*.txt|All files (*.*)|*.*");
				((FileDialog)val).set_FilterIndex(Settings.Default.OCC_File_LastIndex);
				val.set_OverwritePrompt(false);
				try
				{
					((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.OCC_File_LastName));
				}
				catch
				{
				}
				if ((int)((CommonDialog)val).ShowDialog() == 1)
				{
					Current_OCC_File = ((FileDialog)val).get_FileName();
					((Control)this).set_Text("Observations editor : " + Path.GetFileName(Current_OCC_File));
					Settings.Default.OCC_File_LastName = Current_OCC_File;
					Settings.Default.OCC_File_LastIndex = ((FileDialog)val).get_FilterIndex();
					Settings.Default.OCC_FileDirectory = ((FileDialog)val).get_FileName();
					Save_OCC_file(UseOldFormat: true);
				}
			}
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveReport(sender, e);
		}

		private void SaveReport(object sender, EventArgs e)
		{
			//IL_0047: Unknown result type (might be due to invalid IL or missing references)
			if ((LunarObservations.OccMain.Events.Count < 1) & (LunarObservations.OccMain.Telescopes.Count < 1) & (LunarObservations.OccMain.Observers.Count < 1))
			{
				MessageBox.Show("There are no observations, sites or observers to be saved", "No observations", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else if (Current_OCC_File.Length < 1)
			{
				saveAsToolStripMenuItem_Click(sender, e);
			}
			else
			{
				Save_OCC_file(UseOldFormat: false);
			}
		}

		private void saveAsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0047: Unknown result type (might be due to invalid IL or missing references)
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0054: Expected O, but got Unknown
			//IL_009c: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a2: Invalid comparison between Unknown and I4
			if ((LunarObservations.OccMain.Events.Count < 1) & (LunarObservations.OccMain.Telescopes.Count < 1) & (LunarObservations.OccMain.Observers.Count < 1))
			{
				MessageBox.Show("There are no observations to be saved", "No observations", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_Title("Specify file to Write lunar observations to.");
			((FileDialog)val).set_Filter("DAT file (*.dat)|*.dat|Graze files (*.grz)|*.grz|Text files (*.txt)|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(Settings.Default.OCC_File_LastIndex);
			val.set_OverwritePrompt(false);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.OCC_File_LastName));
			}
			catch
			{
			}
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Current_OCC_File = ((FileDialog)val).get_FileName();
				((Control)this).set_Text("Observations editor : " + Path.GetFileName(Current_OCC_File));
				Settings.Default.OCC_File_LastName = Current_OCC_File;
				Settings.Default.OCC_File_LastIndex = ((FileDialog)val).get_FilterIndex();
				Settings.Default.OCC_FileDirectory = ((FileDialog)val).get_FileName();
				Save_OCC_file(UseOldFormat: false);
			}
		}

		private void copyReportToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayReport(AllForms: false, HighlightLast: false);
			string text = "";
			for (int i = 0; i < lstReport.get_Items().get_Count(); i++)
			{
				text = text + lstReport.get_Items().get_Item(i).ToString() + "\r\n";
			}
			Clipboard.SetText(text);
		}

		private void Save_OCC_file(bool UseOldFormat)
		{
			LunarObservations.OccMain.WriteReport(Current_OCC_File, UseOldFormat);
			bool editsNotSaved;
			((Control)lblUnsavedEdits).set_Visible(editsNotSaved = false);
			LunarObservations.EditsNotSaved = editsNotSaved;
		}

		private bool UnsavedDataNeedsToBeSaved()
		{
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Invalid comparison between Unknown and I4
			if (!LunarObservations.EditsNotSaved)
			{
				return false;
			}
			return (int)MessageBox.Show("Edits to the current data have not been saved,\r\nand will be lost if the current action is continued.\r\n\r\nDo you want to continue?", "Unsaved edits", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7;
		}

		private void newToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!UnsavedDataNeedsToBeSaved())
			{
				ReSetForm();
			}
		}

		private void ReSetForm()
		{
			UpdatingData = true;
			((Control)this).set_Text("Observations editor : ");
			ToolStripMenuItem obj = importToolStripMenuItem;
			bool enabled;
			((ToolStripItem)pasteMergeToolStripMenuItem).set_Enabled(enabled = false);
			((ToolStripItem)obj).set_Enabled(enabled);
			Current_OCC_File = "";
			LunarObservations.OccMain.Events.Clear();
			LunarObservations.OccMain.Observers.Clear();
			LunarObservations.OccMain.Telescopes.Clear();
			lstReport.get_Items().Clear();
			lstSites.get_Items().Clear();
			lstNames.get_Items().Clear();
			lstEvents.get_Items().Clear();
			((ToolStripItem)createLightCurveReportToolStripMenuItem).set_Enabled(false);
			ComboBox obj2 = cmbSites;
			ComboBox obj3 = cmbObservers;
			string text;
			((Control)cmbRecorders).set_Text(text = "");
			string text2;
			((Control)obj3).set_Text(text2 = text);
			((Control)obj2).set_Text(text2);
			try
			{
				((Component)(object)ReductionProfile.ObservedProfile).Dispose();
			}
			catch
			{
			}
			try
			{
				((Component)(object)LunarObservations.List_Residuals).Dispose();
			}
			catch
			{
			}
			LunarObservations.OccMain.Place = Settings.Default.ILOC_Place;
			LunarObservations.OccMain.Address = Settings.Default.ILOC_Address;
			LunarObservations.OccMain.EMail = Settings.Default.ILOC_Email;
			LunarObservations.OccMain.Representative = Settings.Default.ILOC_Representative;
			LunarObservations.OccMain.Reported = Settings.Default.ILOC_ReportedTo;
			LunarObservations.OccMain.Message = "";
			((ListControl)cmbDatum).set_SelectedIndex(0);
			ComboBox obj6 = cmbDrive;
			ComboBox obj7 = cmbMount;
			int num;
			((ListControl)cmbWDS).set_SelectedIndex(num = 0);
			int selectedIndex;
			((ListControl)obj7).set_SelectedIndex(selectedIndex = num);
			((ListControl)obj6).set_SelectedIndex(selectedIndex);
			TextBox obj8 = txtAlt_ft;
			TextBox obj9 = txtAlt_m;
			TextBox obj10 = txtAperture;
			string text3;
			((Control)txtFocalLength).set_Text(text3 = "");
			((Control)obj10).set_Text(text = text3);
			((Control)obj9).set_Text(text2 = text);
			((Control)obj8).set_Text(text2);
			TextBox obj11 = txtILOCName;
			TextBox obj12 = txtILOCStation;
			TextBox obj13 = txtILOCStation_Name;
			((Control)txtILOCTelescope).set_Text(text3 = "");
			((Control)obj13).set_Text(text = text3);
			((Control)obj12).set_Text(text2 = text);
			((Control)obj11).set_Text(text2);
			TextBox obj14 = txtLatDDD;
			TextBox obj15 = txtLatDeg;
			TextBox obj16 = txtLatDmm;
			TextBox obj17 = txtLatMin;
			TextBox obj18 = txtLatMM;
			string text4;
			((Control)txtLatSec).set_Text(text4 = "");
			string text5;
			((Control)obj18).set_Text(text5 = text4);
			((Control)obj17).set_Text(text3 = text5);
			((Control)obj16).set_Text(text = text3);
			((Control)obj15).set_Text(text2 = text);
			((Control)obj14).set_Text(text2);
			TextBox obj19 = txtLongDDD;
			TextBox obj20 = txtLongDeg;
			TextBox obj21 = txtLongDmm;
			TextBox obj22 = txtLongMin;
			TextBox obj23 = txtLongMM;
			((Control)txtLongSec).set_Text(text4 = "");
			((Control)obj23).set_Text(text5 = text4);
			((Control)obj22).set_Text(text3 = text5);
			((Control)obj21).set_Text(text = text3);
			((Control)obj20).set_Text(text2 = text);
			((Control)obj19).set_Text(text2);
			TextBox obj24 = txtPE;
			TextBox obj25 = txtAccuracy;
			TextBox obj26 = txtSN;
			TextBox obj27 = txtDuration;
			((Control)txtComments).set_Text(text5 = "");
			((Control)obj27).set_Text(text3 = text5);
			((Control)obj26).set_Text(text = text3);
			((Control)obj25).set_Text(text2 = text);
			((Control)obj24).set_Text(text2);
			((ListControl)cmbTemp).set_SelectedIndex(0);
			TextBox obj28 = txtName;
			((Control)txtObserverEmail).set_Text(text2 = "");
			((Control)obj28).set_Text(text2);
			updnYear.set_Value((decimal)DateTime.Now.ToUniversalTime().Year);
			updnMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().Month);
			updnDay.set_Value((decimal)DateTime.Now.ToUniversalTime().Day);
			NumericUpDown obj29 = updnHour;
			NumericUpDown obj30 = updnMin;
			decimal value = default(decimal);
			obj30.set_Value(value);
			obj29.set_Value(value);
			((Control)txtSecond).set_Text("0.0");
			RadioButton obj31 = optDisappear;
			optDarkLimb.set_Checked(enabled = true);
			obj31.set_Checked(enabled);
			RadioButton obj32 = optReappear;
			RadioButton obj33 = optBrightLimb;
			bool flag;
			optUmbra.set_Checked(flag = false);
			obj33.set_Checked(enabled = flag);
			obj32.set_Checked(enabled);
			CheckBox obj34 = chkGraze;
			RadioButton obj35 = optFlash;
			RadioButton obj36 = optBlink;
			RadioButton obj37 = optMiss;
			RadioButton obj38 = optStarted;
			RadioButton obj39 = optStopped;
			bool flag2;
			optFailed.set_Checked(flag2 = false);
			bool flag3;
			obj39.set_Checked(flag3 = flag2);
			bool flag4;
			obj38.set_Checked(flag4 = flag3);
			bool flag5;
			obj37.set_Checked(flag5 = flag4);
			obj36.set_Checked(flag = flag5);
			obj35.set_Checked(enabled = flag);
			obj34.set_Checked(enabled);
			((ListControl)cmbCertainty).set_SelectedIndex(0);
			((ListControl)cmbDoubles).set_SelectedIndex(0);
			((ListControl)cmbMethod1).set_SelectedIndex(0);
			((ListControl)cmbMethod2).set_SelectedIndex(0);
			((ListControl)cmbPE).set_SelectedIndex(0);
			((ListControl)cmbRemarkable).set_SelectedIndex(0);
			((ListControl)cmbStability).set_SelectedIndex(0);
			((ListControl)cmbTimeKeeping).set_SelectedIndex(0);
			((ListControl)cmbTransparency).set_SelectedIndex(0);
			((ListControl)cmbLightLevel).set_SelectedIndex(0);
			identifyUnidentifiedStarsInReportToolStripMenuItem.set_Checked(false);
			listPossibleDoubleStarsToolStripMenuItem.set_Checked(false);
			verifyAccuracyValuesToolStripMenuItem.set_Checked(false);
			checkTemperatureValuesToolStripMenuItem.set_Checked(false);
			verifyDataEntryToolStripMenuItem.set_Checked(false);
			LunarObservations.EditsNotSaved = false;
			DisplayReport(AllForms: true, HighlightLast: false);
		}

		internal void DisplayReport(bool AllForms, bool HighlightLast)
		{
			lstReport.get_Items().Clear();
			EventLine.UseOldFormat = chkFormat.get_Checked();
			TelescopeLine.UseOldFormat = chkFormat.get_Checked();
			ObserverLine.UseOldFormat = chkFormat.get_Checked();
			GroupBox obj = grpILOCnames;
			GroupBox obj2 = grpILOCstations;
			bool @checked;
			((Control)grpILOCevents).set_Visible(@checked = chkFormat.get_Checked());
			bool visible;
			((Control)obj2).set_Visible(visible = @checked);
			((Control)obj).set_Visible(visible);
			Label obj3 = txtRecorder;
			((Control)cmbRecorders).set_Visible(visible = chkFormat.get_Checked());
			((Control)obj3).set_Visible(visible);
			lstReport.get_Items().Add((object)("Place name     " + LunarObservations.OccMain.Place));
			((Control)txtPlace).set_Text(LunarObservations.OccMain.Place);
			if (chkFormat.get_Checked())
			{
				lstReport.get_Items().Add((object)("Address        " + LunarObservations.OccMain.Address));
				((Control)txtAddress).set_Text(LunarObservations.OccMain.Address);
				((Control)panelAddress).set_Visible(true);
			}
			else
			{
				((Control)panelAddress).set_Visible(false);
			}
			lstReport.get_Items().Add((object)("Email address  " + LunarObservations.OccMain.EMail));
			((Control)txtEmail).set_Text(LunarObservations.OccMain.EMail);
			lstReport.get_Items().Add((object)("Representative " + LunarObservations.OccMain.Representative));
			((Control)txtRepresentative).set_Text(LunarObservations.OccMain.Representative);
			if (chkFormat.get_Checked())
			{
				lstReport.get_Items().Add((object)("Reported to    " + LunarObservations.OccMain.Reported));
				((Control)txtReported).set_Text(LunarObservations.OccMain.Reported);
				((Control)panelReportedTo).set_Visible(true);
				((Control)panelRepresentative).set_Top(166);
			}
			else
			{
				((Control)panelReportedTo).set_Visible(false);
				((Control)panelRepresentative).set_Top(202);
			}
			lstReport.get_Items().Add((object)"");
			((Control)txtMessage).set_Text(LunarObservations.OccMain.Message);
			if (LunarObservations.OccMain.Message.Length > 0)
			{
				int num = 0;
				int num2 = LunarObservations.OccMain.Message.Length - 1;
				int num3 = 0;
				do
				{
					num3 = ((num2 - num >= 60) ? LunarObservations.OccMain.Message.LastIndexOf(" ", num + 60) : num2);
					int num4 = LunarObservations.OccMain.Message.Substring(num, num3 - num + 1).IndexOf("\r\n");
					if (num4 > 0 && num4 < num3 - 2)
					{
						num3 = num + num4;
					}
					lstReport.get_Items().Add((object)("Message        " + LunarObservations.OccMain.Message.Substring(num, num3 - num + 1)));
					num = num3 + 1;
				}
				while (num3 < num2);
				lstReport.get_Items().Add((object)"");
			}
			for (int i = 0; i < LunarObservations.OccMain.Telescopes.Count; i++)
			{
				lstReport.get_Items().Add((object)LunarObservations.OccMain.Telescopes[i].ToString());
			}
			lstReport.get_Items().Add((object)"");
			for (int j = 0; j < LunarObservations.OccMain.Observers.Count; j++)
			{
				lstReport.get_Items().Add((object)LunarObservations.OccMain.Observers[j].ToString());
			}
			lstReport.get_Items().Add((object)"");
			for (int k = 0; k < LunarObservations.OccMain.Events.Count; k++)
			{
				LunarObservations.OccMain.Events[k].GetEventLines(out var Line, out var Line2);
				lstReport.get_Items().Add((object)Line);
				if (Line2.Trim().Length > 0)
				{
					lstReport.get_Items().Add((object)Line2);
				}
			}
			if (AllForms)
			{
				UpdatingData = true;
				DisplaySitesInEditor(0);
				DisplayNamesInEditor(0);
				if (HighlightLast & (lstReport.get_Items().get_Count() > 0))
				{
					DisplayEventsInEditor(lstReport.get_Items().get_Count() - 1);
				}
				else
				{
					DisplayEventsInEditor(0);
				}
				UpdatingData = false;
			}
		}

		internal void DisplayEventsInEditor(int Line)
		{
			lstEvents.get_Items().Clear();
			for (int i = 0; i < LunarObservations.OccMain.Events.Count; i++)
			{
				if (chkFormat.get_Checked())
				{
					lstEvents.get_Items().Add((object)LunarObservations.OccMain.Events[i].ToString());
				}
				else
				{
					lstEvents.get_Items().Add((object)(string.Format("{0,3:F0} ", LunarObservations.OccMain.Events[i].SeqNumber) + LunarObservations.OccMain.Events[i].ToString()));
				}
			}
			if ((Line >= 0) & (Line < lstEvents.get_Items().get_Count()))
			{
				((ListControl)lstEvents).set_SelectedIndex(Line);
			}
			else if ((Line >= lstEvents.get_Items().get_Count()) & (lstEvents.get_Items().get_Count() > 0))
			{
				((ListControl)lstEvents).set_SelectedIndex(lstEvents.get_Items().get_Count() - 1);
			}
			else if ((Line < 0) & (lstEvents.get_Items().get_Count() > 0))
			{
				((ListControl)lstEvents).set_SelectedIndex(0);
			}
			((ToolStripItem)createLightCurveReportToolStripMenuItem).set_Enabled(((ListControl)lstEvents).get_SelectedIndex() >= 0);
		}

		private void DisplaySitesInEditor(int Line)
		{
			lstSites.get_Items().Clear();
			cmbSites.get_Items().Clear();
			for (int i = 0; i < LunarObservations.OccMain.Telescopes.Count; i++)
			{
				lstSites.get_Items().Add((object)LunarObservations.OccMain.Telescopes[i].ToString());
				cmbSites.get_Items().Add((object)(LunarObservations.OccMain.Telescopes[i].TelescopeCodeForEvent + " : " + $"{LunarObservations.OccMain.Telescopes[i].Aperture:F1}cm at " + Utilities.DEGtoDMS(LunarObservations.OccMain.Telescopes[i].LongitudeDeg, 4, 1, MinutesOnly: false) + " " + Utilities.DEGtoDMS(LunarObservations.OccMain.Telescopes[i].LatitudeDeg, 3, 1, MinutesOnly: false)));
			}
			if ((Line >= 0) & (Line < lstSites.get_Items().get_Count()))
			{
				((ListControl)lstSites).set_SelectedIndex(Line);
			}
			else if ((Line >= lstSites.get_Items().get_Count()) & (lstSites.get_Items().get_Count() > 0))
			{
				((ListControl)lstSites).set_SelectedIndex(Line - 1);
			}
			else if ((Line < 0) & (lstSites.get_Items().get_Count() > 0))
			{
				((ListControl)lstSites).set_SelectedIndex(0);
			}
			if (cmbSites.get_Items().get_Count() == 1)
			{
				((ListControl)cmbSites).set_SelectedIndex(0);
			}
			if (LunarObservations.OccMain.Events.Count <= 0)
			{
				return;
			}
			string eventTelescope = LunarObservations.OccMain.Events[Line].EventTelescope;
			for (int j = 0; j < cmbSites.get_Items().get_Count(); j++)
			{
				if (eventTelescope == LunarObservations.OccMain.Telescopes[j].TelescopeCodeForEvent)
				{
					((ListControl)cmbSites).set_SelectedIndex(j);
					break;
				}
			}
		}

		private void DisplayNamesInEditor(int Line)
		{
			lstNames.get_Items().Clear();
			cmbObservers.get_Items().Clear();
			cmbRecorders.get_Items().Clear();
			for (int i = 0; i < LunarObservations.OccMain.Observers.Count; i++)
			{
				lstNames.get_Items().Add((object)LunarObservations.OccMain.Observers[i].ToString());
				cmbRecorders.get_Items().Add((object)(LunarObservations.OccMain.Observers[i].ObserverCodeForEvent + " : " + LunarObservations.OccMain.Observers[i].ObserverName.Trim()));
				cmbObservers.get_Items().Add((object)(LunarObservations.OccMain.Observers[i].ObserverCodeForEvent + " : " + LunarObservations.OccMain.Observers[i].ObserverName.Trim()));
			}
			if ((Line >= 0) & (Line < lstNames.get_Items().get_Count()))
			{
				((ListControl)lstNames).set_SelectedIndex(Line);
			}
			else if ((Line >= lstNames.get_Items().get_Count()) & (lstNames.get_Items().get_Count() > 0))
			{
				((ListControl)lstNames).set_SelectedIndex(Line - 1);
			}
			else if ((Line < 0) & (lstNames.get_Items().get_Count() > 0))
			{
				((ListControl)lstNames).set_SelectedIndex(0);
			}
			if (cmbObservers.get_Items().get_Count() == 1)
			{
				((ListControl)cmbObservers).set_SelectedIndex(0);
			}
			if (cmbRecorders.get_Items().get_Count() == 1)
			{
				((ListControl)cmbRecorders).set_SelectedIndex(0);
			}
			if (LunarObservations.OccMain.Events.Count <= 0)
			{
				return;
			}
			string eventObserver = LunarObservations.OccMain.Events[Line].EventObserver;
			if (eventObserver.Trim().Length > 0)
			{
				for (int j = 0; j < cmbObservers.get_Items().get_Count(); j++)
				{
					if (eventObserver == LunarObservations.OccMain.Observers[j].ObserverCodeForEvent)
					{
						((ListControl)cmbObservers).set_SelectedIndex(j);
						break;
					}
				}
			}
			eventObserver = LunarObservations.OccMain.Events[Line].EventRecorder;
			if (eventObserver.Trim().Length <= 0)
			{
				return;
			}
			for (int k = 0; k < cmbObservers.get_Items().get_Count(); k++)
			{
				if (eventObserver == LunarObservations.OccMain.Observers[k].ObserverCodeForEvent)
				{
					((ListControl)cmbRecorders).set_SelectedIndex(k);
					break;
				}
			}
		}

		private void cmdGetPlace_Click(object sender, EventArgs e)
		{
			((Control)txtPlace).set_Text(Settings.Default.ILOC_Place);
		}

		private void cmdSetPlace_Click(object sender, EventArgs e)
		{
			Settings.Default.ILOC_Place = ((Control)txtPlace).get_Text();
			toolTip.SetToolTip((Control)(object)cmdSetPlace, "Current default = " + Settings.Default.ILOC_Place);
			toolTip.SetToolTip((Control)(object)cmdGetPlace, "Current default = " + Settings.Default.ILOC_Place);
		}

		private void cmdGetAddress_Click(object sender, EventArgs e)
		{
			((Control)txtAddress).set_Text(Settings.Default.ILOC_Address);
		}

		private void cmdSetAddress_Click(object sender, EventArgs e)
		{
			Settings.Default.ILOC_Address = ((Control)txtAddress).get_Text();
			toolTip.SetToolTip((Control)(object)cmdSetAddress, "Current default = " + Settings.Default.ILOC_Address);
			toolTip.SetToolTip((Control)(object)cmdGetAddress, "Current default = " + Settings.Default.ILOC_Address);
		}

		private void cmdGetEMail_Click(object sender, EventArgs e)
		{
			((Control)txtEmail).set_Text(Settings.Default.ILOC_Email);
		}

		private void cmdSetEmail_Click(object sender, EventArgs e)
		{
			Settings.Default.ILOC_Email = ((Control)txtEmail).get_Text();
			toolTip.SetToolTip((Control)(object)cmdSetEmail, "Current default = " + Settings.Default.ILOC_Email);
			toolTip.SetToolTip((Control)(object)cmdGetEMail, "Current default = " + Settings.Default.ILOC_Email);
		}

		private void cmdGetRep_Click(object sender, EventArgs e)
		{
			((Control)txtRepresentative).set_Text(Settings.Default.ILOC_Representative);
		}

		private void cmdSetRep_Click(object sender, EventArgs e)
		{
			Settings.Default.ILOC_Representative = ((Control)txtRepresentative).get_Text();
			toolTip.SetToolTip((Control)(object)cmdSetRep, "Current default = " + Settings.Default.ILOC_Representative);
			toolTip.SetToolTip((Control)(object)cmdGetRep, "Current default = " + Settings.Default.ILOC_Representative);
		}

		private void cmdGetReported_Click(object sender, EventArgs e)
		{
			((Control)txtReported).set_Text(Settings.Default.ILOC_ReportedTo);
		}

		private void cmdSetReported_Click(object sender, EventArgs e)
		{
			Settings.Default.ILOC_ReportedTo = ((Control)txtReported).get_Text();
			toolTip.SetToolTip((Control)(object)cmdSetReported, "Current default = " + Settings.Default.ILOC_ReportedTo);
			toolTip.SetToolTip((Control)(object)cmdGetReported, "Current default = " + Settings.Default.ILOC_ReportedTo);
		}

		private void txtPlace_KeyUp(object sender, KeyEventArgs e)
		{
			((Control)lblCharsLeft).set_Text((50 - ((Control)txtPlace).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
		}

		private void txtPlace_Enter(object sender, EventArgs e)
		{
			((Control)lblCharsLeft).set_Text((50 - ((Control)txtPlace).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
		}

		private void txtPlace_TextChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				((Control)lblCharsLeft).set_Text((50 - ((Control)txtPlace).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
				LunarObservations.OccMain.Place = ((Control)txtPlace).get_Text().Trim();
			}
		}

		private void txtPlace_Leave(object sender, EventArgs e)
		{
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtPlace).get_Text(), "Place name", CheckForPipes: false, out var RevisedText))
			{
				((Control)txtPlace).set_Text(RevisedText);
				((TextBoxBase)txtPlace).set_SelectionStart(0);
				((Control)txtPlace).Focus();
			}
		}

		private void txtAddress_TextChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				LunarObservations.OccMain.Address = ((Control)txtAddress).get_Text();
			}
		}

		private void txtEmail_TextChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				LunarObservations.OccMain.EMail = ((Control)txtEmail).get_Text();
			}
		}

		private void txtMessage_TextChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				LunarObservations.OccMain.Message = ((Control)txtMessage).get_Text().Replace("\r\n\r\n", "\r\n");
			}
		}

		private void txtRepresentative_TextChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				LunarObservations.OccMain.Representative = ((Control)txtRepresentative).get_Text();
			}
		}

		private void txtReported_TextChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				LunarObservations.OccMain.Reported = ((Control)txtReported).get_Text();
			}
		}

		private void lstSites_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstSites).get_SelectedIndex() >= 0)
			{
				DecodeSite(((ListControl)lstSites).get_SelectedIndex());
			}
		}

		private void DecodeSite(int Line)
		{
			UpdatingData = true;
			chkHighPrecision.set_Checked(LunarObservations.OccMain.Telescopes[Line].LongSec_DecPlaces > 1);
			((Control)txtLongDDD).set_Text(string.Format("{0,1:F" + (LunarObservations.OccMain.Telescopes[Line].LongSec_DecPlaces + 4) + "}", LunarObservations.OccMain.Telescopes[Line].LongitudeDeg));
			UpdateLongitudes(2);
			((Control)txtLatDDD).set_Text(string.Format("{0,1:F" + (LunarObservations.OccMain.Telescopes[Line].LatSec_DecPlaces + 4) + "}", LunarObservations.OccMain.Telescopes[Line].LatitudeDeg));
			UpdateLatitudes(2);
			((Control)txtAlt_m).set_Text(string.Format("{0:F" + LunarObservations.OccMain.Telescopes[Line].Alt_DecPlaces + "}", LunarObservations.OccMain.Telescopes[Line].Altitude));
			((Control)txtAlt_ft).set_Text(string.Format("{0:F" + LunarObservations.OccMain.Telescopes[Line].Alt_DecPlaces + "}", LunarObservations.OccMain.Telescopes[Line].Altitude / 0.3048));
			optWGS.set_Checked(LunarObservations.OccMain.Telescopes[Line].VerticalDatum == "E");
			optMSL.set_Checked(!optWGS.get_Checked());
			((Control)txtAperture).set_Text($"{LunarObservations.OccMain.Telescopes[Line].Aperture:F1}");
			((Control)txtFocalLength).set_Text($"{LunarObservations.OccMain.Telescopes[Line].FocalLength:F1}");
			int num = " RNCO".IndexOf(LunarObservations.OccMain.Telescopes[Line].TelescopeType);
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbTelescope).set_SelectedIndex(num);
			num = " EA".IndexOf(LunarObservations.OccMain.Telescopes[Line].MountType);
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbMount).set_SelectedIndex(num);
			num = " DM".IndexOf(LunarObservations.OccMain.Telescopes[Line].DriveType);
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbDrive).set_SelectedIndex(num);
			((Control)txtILOCStation).set_Text(LunarObservations.OccMain.Telescopes[Line].TelescopeStation);
			((Control)txtILOCTelescope).set_Text(LunarObservations.OccMain.Telescopes[Line].TelescopeTelescopeCode);
			string value = LunarObservations.OccMain.Telescopes[Line].DatumNumber.ToString().PadLeft(2);
			for (int i = 0; i < cmbDatum.get_Items().get_Count(); i++)
			{
				if (cmbDatum.get_Items().get_Item(i).ToString()!.Substring(0, 4).Contains(value))
				{
					((ListControl)cmbDatum).set_SelectedIndex(i);
					break;
				}
				((ListControl)cmbDatum).set_SelectedIndex(0);
			}
			UpdatingData = false;
		}

		private void cmdDefaultSite_Click(object sender, EventArgs e)
		{
			//IL_004b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0051: Invalid comparison between Unknown and I4
			if (((ListControl)lstSites).get_SelectedIndex() >= 0 && (int)MessageBox.Show("Are you sure you want to make the following the Default site?:\r\n\r\n" + lstSites.get_Items().get_Item(((ListControl)lstSites).get_SelectedIndex()).ToString(), "Confirm Default site", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				Settings.Default.ILOC_DefaultSite = lstSites.get_Items().get_Item(((ListControl)lstSites).get_SelectedIndex()).ToString();
				toolTip.SetToolTip((Control)(object)cmdDefaultSite, "Current default = " + Settings.Default.ILOC_DefaultSite);
				toolTip.SetToolTip((Control)(object)cmdAddDefaultSite, "Current default = " + Settings.Default.ILOC_DefaultSite);
			}
		}

		private void cmdAddDefaultSite_Click(object sender, EventArgs e)
		{
			int line = LunarObservations.OccMain.AddNewSite(Settings.Default.ILOC_DefaultSite);
			DisplayReport(AllForms: true, HighlightLast: true);
			DisplaySitesInEditor(line);
		}

		private void cmdAddSites_Click(object sender, EventArgs e)
		{
			int line = LunarObservations.OccMain.AddNewSite("");
			EncodeSite(line);
			DisplayReport(AllForms: true, HighlightLast: true);
			DisplaySitesInEditor(line);
		}

		private void cmdReplaceSites_Click(object sender, EventArgs e)
		{
			//IL_0042: Unknown result type (might be due to invalid IL or missing references)
			//IL_0048: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstSites).get_SelectedIndex();
			if (selectedIndex >= 0 && (int)MessageBox.Show("Do you want to replace the site:\r\n\r\n" + LunarObservations.OccMain.Telescopes[selectedIndex].ToString(), "Confirm replace", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				EncodeSite(selectedIndex);
				DisplayReport(AllForms: true, HighlightLast: true);
				DisplaySitesInEditor(selectedIndex);
			}
		}

		private void cmdDeleteSites_Click(object sender, EventArgs e)
		{
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c3: Invalid comparison between Unknown and I4
			int num = ((ListControl)lstSites).get_SelectedIndex();
			if (num < 0)
			{
				return;
			}
			string telescopeCodeForEvent = LunarObservations.OccMain.Telescopes[num].TelescopeCodeForEvent;
			for (int i = 0; i < LunarObservations.OccMain.Events.Count; i++)
			{
				if (LunarObservations.OccMain.Events[i].EventTelescope == telescopeCodeForEvent)
				{
					MessageBox.Show("This Site cannot be deleted. It is used in the following observation:\r\n\r\n" + LunarObservations.OccMain.Events[i].ToString(), "Cannot Delete Site", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
			}
			if ((int)MessageBox.Show("Do you want to delete the site:\r\n\r\n" + LunarObservations.OccMain.Telescopes[num].ToString(), "Confirm deletion", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				LunarObservations.OccMain.Telescopes.RemoveAt(num);
				DisplayReport(AllForms: true, HighlightLast: true);
				if (num >= LunarObservations.OccMain.Telescopes.Count)
				{
					num--;
				}
				DisplaySitesInEditor(num);
			}
		}

		private void EncodeSite(int Line)
		{
			if (!int.TryParse(((Control)txtLongDeg).get_Text().Replace('-', ' '), out var result))
			{
				result = 0;
			}
			LunarObservations.OccMain.Telescopes[Line].LongDeg = result;
			if (((Control)txtLongDeg).get_Text().Contains("-"))
			{
				LunarObservations.OccMain.Telescopes[Line].LongSign = "-";
				LunarObservations.OccMain.Telescopes[Line].EW = "W";
			}
			else
			{
				LunarObservations.OccMain.Telescopes[Line].LongSign = "+";
				LunarObservations.OccMain.Telescopes[Line].EW = "E";
			}
			if (!int.TryParse(((Control)txtLongMin).get_Text(), out result))
			{
				result = 0;
			}
			LunarObservations.OccMain.Telescopes[Line].LongMin = result;
			if (!double.TryParse(((Control)txtLongSec).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			LunarObservations.OccMain.Telescopes[Line].LongSec = result2;
			LunarObservations.OccMain.Telescopes[Line].LongSec_DecPlaces = Utilities.DecimalPlaces(((Control)txtLongSec).get_Text());
			if (!int.TryParse(((Control)txtLatDeg).get_Text().Replace('-', ' '), out result))
			{
				result = 0;
			}
			LunarObservations.OccMain.Telescopes[Line].LatDeg = result;
			if (((Control)txtLatDeg).get_Text().Contains("-"))
			{
				LunarObservations.OccMain.Telescopes[Line].LatSign = "-";
				LunarObservations.OccMain.Telescopes[Line].NS = "S";
			}
			else
			{
				LunarObservations.OccMain.Telescopes[Line].LatSign = "+";
				LunarObservations.OccMain.Telescopes[Line].NS = "N";
			}
			if (!int.TryParse(((Control)txtLatMin).get_Text(), out result))
			{
				result = 0;
			}
			LunarObservations.OccMain.Telescopes[Line].LatMin = result;
			if (!double.TryParse(((Control)txtLatSec).get_Text(), out result2))
			{
				result2 = 0.0;
			}
			LunarObservations.OccMain.Telescopes[Line].LatSec = result2;
			LunarObservations.OccMain.Telescopes[Line].LatSec_DecPlaces = Utilities.DecimalPlaces(((Control)txtLatSec).get_Text());
			if (!double.TryParse(((Control)txtAlt_m).get_Text(), out result2))
			{
				result2 = 0.0;
			}
			LunarObservations.OccMain.Telescopes[Line].Altitude = result2;
			LunarObservations.OccMain.Telescopes[Line].Alt_DecPlaces = Utilities.DecimalPlaces(((Control)txtAlt_m).get_Text());
			if (!double.TryParse(((Control)txtAperture).get_Text(), out result2))
			{
				result2 = 0.0;
			}
			LunarObservations.OccMain.Telescopes[Line].Aperture = result2;
			if (!double.TryParse(((Control)txtFocalLength).get_Text(), out result2))
			{
				result2 = 0.0;
			}
			LunarObservations.OccMain.Telescopes[Line].FocalLength = result2;
			if (((ListControl)cmbTelescope).get_SelectedIndex() < 0)
			{
				((ListControl)cmbTelescope).set_SelectedIndex(0);
			}
			LunarObservations.OccMain.Telescopes[Line].TelescopeType = " RNCO".Substring(((ListControl)cmbTelescope).get_SelectedIndex(), 1);
			if (((ListControl)cmbMount).get_SelectedIndex() < 0)
			{
				((ListControl)cmbMount).set_SelectedIndex(0);
			}
			LunarObservations.OccMain.Telescopes[Line].MountType = " EA".Substring(((ListControl)cmbMount).get_SelectedIndex(), 1);
			if (((ListControl)cmbDrive).get_SelectedIndex() < 0)
			{
				((ListControl)cmbDrive).set_SelectedIndex(0);
			}
			LunarObservations.OccMain.Telescopes[Line].DriveType = " DM".Substring(((ListControl)cmbDrive).get_SelectedIndex(), 1);
			LunarObservations.OccMain.Telescopes[Line].TelescopeStation = ((Control)txtILOCStation).get_Text().Trim().PadRight(5)
				.Substring(0, 5);
			LunarObservations.OccMain.Telescopes[Line].TelescopeTelescopeCode = ((Control)txtILOCTelescope).get_Text().Trim().PadRight(2)
				.Substring(0, 2);
			if (((ListControl)cmbDatum).get_SelectedIndex() < 0)
			{
				((ListControl)cmbDatum).set_SelectedIndex(0);
			}
			LunarObservations.OccMain.Telescopes[Line].DatumNumber = int.Parse(cmbDatum.get_Items().get_Item(((ListControl)cmbDatum).get_SelectedIndex()).ToString()!.Substring(0, 2));
			if (optWGS.get_Checked())
			{
				LunarObservations.OccMain.Telescopes[Line].VerticalDatum = "E";
			}
			else
			{
				LunarObservations.OccMain.Telescopes[Line].VerticalDatum = "M";
			}
		}

		private void cmdSortByNumber_Click(object sender, EventArgs e)
		{
			TelescopeLine.SortField = 0;
			LunarObservations.OccMain.Telescopes.Sort();
			DisplayReport(AllForms: true, HighlightLast: false);
			DisplaySitesInEditor(0);
		}

		private void cmdSortByLongitude_Click(object sender, EventArgs e)
		{
			TelescopeLine.SortField = 1;
			LunarObservations.OccMain.Telescopes.Sort();
			DisplayReport(AllForms: true, HighlightLast: false);
			DisplaySitesInEditor(0);
		}

		private void cmdSortByLatitude_Click(object sender, EventArgs e)
		{
			TelescopeLine.SortField = 2;
			LunarObservations.OccMain.Telescopes.Sort();
			DisplayReport(AllForms: true, HighlightLast: false);
			DisplaySitesInEditor(0);
		}

		private void cmdRenumberSites_Click(object sender, EventArgs e)
		{
			LunarObservations.OccMain.ReNumberSites("A");
			DisplayReport(AllForms: true, HighlightLast: false);
			DisplaySitesInEditor(0);
		}

		private void cmdMoveSiteUp_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstSites).get_SelectedIndex();
			if (selectedIndex >= 1)
			{
				LunarObservations.OccMain.MoveSiteUp(selectedIndex);
				DisplaySitesInEditor(selectedIndex - 1);
			}
		}

		private void cmdMoveSiteDown_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstSites).get_SelectedIndex();
			if (selectedIndex >= 0)
			{
				LunarObservations.OccMain.MoveSiteDown(selectedIndex);
				DisplaySitesInEditor(selectedIndex + 1);
			}
		}

		private void lstNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstNames).get_SelectedIndex();
			if (selectedIndex >= 0)
			{
				((Control)txtName).set_Text(LunarObservations.OccMain.Observers[selectedIndex].ObserverName.TrimEnd(Array.Empty<char>()));
				((Control)txtObserverEmail).set_Text(LunarObservations.OccMain.Observers[selectedIndex].ObserverEmail.Trim());
				((Control)txtILOCStation_Name).set_Text(LunarObservations.OccMain.Observers[selectedIndex].ObserverStation);
				((Control)txtILOCName).set_Text(LunarObservations.OccMain.Observers[selectedIndex].ObserverCode);
			}
		}

		private void txtName_TextChanged(object sender, EventArgs e)
		{
			((Control)lblCharsLeftName).set_Text((25 - ((Control)txtName).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
		}

		private void txtName_KeyUp(object sender, KeyEventArgs e)
		{
			((Control)lblCharsLeftName).set_Text((25 - ((Control)txtName).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
		}

		private void txtName_Enter(object sender, EventArgs e)
		{
			((Control)lblCharsLeftName).set_Text((25 - ((Control)txtName).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
		}

		private void txtName_Leave(object sender, EventArgs e)
		{
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtName).get_Text(), "Name for Observers", CheckForPipes: false, out var RevisedText))
			{
				((Control)txtName).set_Text(RevisedText);
				((Control)txtName).Focus();
				((TextBoxBase)txtName).set_SelectionStart(0);
			}
		}

		private void cmdAddName_Click(object sender, EventArgs e)
		{
			int line = LunarObservations.OccMain.AddNewName("");
			ChangeName(line);
		}

		private void cmdReplaceName_Click(object sender, EventArgs e)
		{
			//IL_0042: Unknown result type (might be due to invalid IL or missing references)
			//IL_0048: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstNames).get_SelectedIndex();
			if (selectedIndex >= 0 && (int)MessageBox.Show("Do you want to replace the name:\r\n\r\n" + LunarObservations.OccMain.Observers[selectedIndex].ToString(), "Confirm replace", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				ChangeName(((ListControl)lstNames).get_SelectedIndex());
			}
		}

		private void ChangeName(int Line)
		{
			LunarObservations.OccMain.Observers[Line].ObserverName = ((Control)txtName).get_Text();
			LunarObservations.OccMain.Observers[Line].ObserverEmail = ((Control)txtObserverEmail).get_Text().Trim();
			LunarObservations.OccMain.Observers[Line].ObserverStation = ((Control)txtILOCStation_Name).get_Text().Trim().PadRight(5)
				.Substring(0, 5);
			LunarObservations.OccMain.Observers[Line].ObserverCode = ((Control)txtILOCName).get_Text().Trim().PadRight(2)
				.Substring(0, 2);
			DisplayReport(AllForms: true, HighlightLast: false);
			DisplayNamesInEditor(Line);
		}

		private void cmdSetDefaultName_Click(object sender, EventArgs e)
		{
			//IL_004b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0051: Invalid comparison between Unknown and I4
			if (((ListControl)lstNames).get_SelectedIndex() >= 0 && (int)MessageBox.Show("Are you sure you want to make the following the Default observer name?:\r\n\r\n" + lstNames.get_Items().get_Item(((ListControl)lstNames).get_SelectedIndex()).ToString(), "Confirm Default name", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				Settings.Default.ILOC_DefaultName = lstNames.get_Items().get_Item(((ListControl)lstNames).get_SelectedIndex()).ToString();
				toolTip.SetToolTip((Control)(object)cmdSetDefaultName, "Current default = " + Settings.Default.ILOC_DefaultName);
				toolTip.SetToolTip((Control)(object)cmdInsertDefaultName, "Current default = " + Settings.Default.ILOC_DefaultName);
			}
		}

		private void cmdInsertDefaultName_Click(object sender, EventArgs e)
		{
			int line = LunarObservations.OccMain.AddNewName(Settings.Default.ILOC_DefaultName);
			DisplayReport(AllForms: true, HighlightLast: false);
			DisplayNamesInEditor(line);
		}

		private void cmdDeleteName_Click(object sender, EventArgs e)
		{
			//IL_008b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_00df: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstNames).get_SelectedIndex();
			if (selectedIndex < 0)
			{
				return;
			}
			string observerCodeForEvent = LunarObservations.OccMain.Observers[selectedIndex].ObserverCodeForEvent;
			for (int i = 0; i < LunarObservations.OccMain.Events.Count; i++)
			{
				if ((LunarObservations.OccMain.Events[i].EventObserver == observerCodeForEvent) | (LunarObservations.OccMain.Events[i].EventRecorder == observerCodeForEvent))
				{
					MessageBox.Show("This Name cannot be deleted. It is used in the following observation:\r\n\r\n" + LunarObservations.OccMain.Events[i].ToString(), "Cannot Delete Name", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
			}
			if ((int)MessageBox.Show("Do you want to delete the name:\r\n\r\n" + LunarObservations.OccMain.Observers[selectedIndex].ToString(), "Confirm deletion", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				LunarObservations.OccMain.Observers.RemoveAt(selectedIndex);
				DisplayReport(AllForms: true, HighlightLast: false);
				DisplayNamesInEditor(selectedIndex);
			}
		}

		private void cmdSortByName_Number_Click(object sender, EventArgs e)
		{
			ObserverLine.SortField = 0;
			LunarObservations.OccMain.Observers.Sort();
			DisplayReport(AllForms: true, HighlightLast: false);
		}

		private void cmdSortByName_Click(object sender, EventArgs e)
		{
			ObserverLine.SortField = 1;
			LunarObservations.OccMain.Observers.Sort();
			DisplayReport(AllForms: true, HighlightLast: false);
		}

		private void cmdSortByFamilyName_Click(object sender, EventArgs e)
		{
			ObserverLine.SortField = 2;
			LunarObservations.OccMain.Observers.Sort();
			DisplayReport(AllForms: true, HighlightLast: false);
		}

		private void cmdRenumberNames_Click(object sender, EventArgs e)
		{
			LunarObservations.OccMain.ReNumberNames("A");
			DisplayReport(AllForms: true, HighlightLast: false);
		}

		private void cmdMoveNameUp_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstNames).get_SelectedIndex();
			if (selectedIndex >= 1)
			{
				LunarObservations.OccMain.MoveNameUp(selectedIndex);
				DisplayNamesInEditor(selectedIndex - 1);
			}
		}

		private void cmdMoveNameDown_Click(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstNames).get_SelectedIndex();
			if (selectedIndex >= 0)
			{
				LunarObservations.OccMain.MoveNameDown(selectedIndex);
				DisplayNamesInEditor(selectedIndex + 1);
			}
		}

		private void lstEvents_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstEvents).get_SelectedIndex() >= 0)
			{
				DecodeEvent(((ListControl)lstEvents).get_SelectedIndex());
				((ToolStripItem)createLightCurveReportToolStripMenuItem).set_Enabled(((ListControl)lstEvents).get_SelectedIndex() >= 0);
				if (LightData.LightCurveForm != null)
				{
					((Form)LightData.LightCurveForm).Close();
				}
			}
		}

		private void DecodeEvent(int Line)
		{
			UpdatingData = true;
			try
			{
				updnYear.set_Value((decimal)LunarObservations.OccMain.Events[Line].Year);
			}
			catch
			{
				updnYear.set_Value(2000m);
			}
			try
			{
				updnMonth.set_Value((decimal)LunarObservations.OccMain.Events[Line].Month);
			}
			catch
			{
				updnMonth.set_Value(1m);
			}
			try
			{
				updnDay.set_Value((decimal)LunarObservations.OccMain.Events[Line].Day);
			}
			catch
			{
				updnDay.set_Value(1m);
			}
			try
			{
				updnHour.set_Value((decimal)LunarObservations.OccMain.Events[Line].Hour);
			}
			catch
			{
				updnHour.set_Value(0m);
			}
			try
			{
				updnMin.set_Value((decimal)LunarObservations.OccMain.Events[Line].Minute);
			}
			catch
			{
				updnMin.set_Value(0m);
			}
			if (LunarObservations.OccMain.Events[Line].Second_DecPlaces > 2)
			{
				((Control)txtSecond).set_Text($"{LunarObservations.OccMain.Events[Line].Second:F3}");
			}
			else if (LunarObservations.OccMain.Events[Line].Second_DecPlaces == 2)
			{
				((Control)txtSecond).set_Text($"{LunarObservations.OccMain.Events[Line].Second:F2}");
			}
			else if (LunarObservations.OccMain.Events[Line].Second_DecPlaces == 1)
			{
				((Control)txtSecond).set_Text($"{LunarObservations.OccMain.Events[Line].Second:F1}");
			}
			else
			{
				((Control)txtSecond).set_Text($"{LunarObservations.OccMain.Events[Line].Second:F0}");
			}
			string occEvent = LunarObservations.OccMain.Events[Line].OccEvent;
			optDisappear.set_Checked(occEvent == "D");
			optReappear.set_Checked(occEvent == "R");
			optBlink.set_Checked(occEvent == "B");
			optFlash.set_Checked(occEvent == "F");
			optMiss.set_Checked(occEvent == "M");
			optFailed.set_Checked(occEvent == "O");
			optStarted.set_Checked(occEvent == "S");
			optStopped.set_Checked(occEvent == "E");
			occEvent = LunarObservations.OccMain.Events[Line].Limb;
			optDarkLimb.set_Checked(occEvent == "D");
			optBrightLimb.set_Checked(occEvent == "B");
			optUmbra.set_Checked(occEvent == "U");
			occEvent = LunarObservations.OccMain.Events[Line].GrazeFlag;
			chkGraze.set_Checked(occEvent == "G");
			int num = LunarObservations.OccMain.Events[Line].Certainty - 1;
			if (num < 0 || num > 2)
			{
				num = 0;
			}
			((ListControl)cmbCertainty).set_SelectedIndex(num);
			num = " WENSBFU".IndexOf(LunarObservations.OccMain.Events[Line].DoubleStarComponent);
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbDoubles).set_SelectedIndex(num);
			num = " GVMSTEPKXC".IndexOf(LunarObservations.OccMain.Events[Line].MethodRecording1);
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbMethod1).set_SelectedIndex(num);
			num = " GVMSTEPKXCA".IndexOf(LunarObservations.OccMain.Events[Line].MethodRecording2);
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbMethod2).set_SelectedIndex(num);
			num = "SUEXAB".IndexOf(LunarObservations.OccMain.Events[Line].PEApplication);
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbPE).set_SelectedIndex(num);
			num = LunarObservations.OccMain.Events[Line].Circumstances;
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbRemarkable).set_SelectedIndex(num);
			num = LunarObservations.OccMain.Events[Line].Stability - 1;
			if (num < 0)
			{
				num = 0;
			}
			if (num > 2)
			{
				num = 2;
			}
			((ListControl)cmbStability).set_SelectedIndex(num);
			num = " GRNCTMO".IndexOf(LunarObservations.OccMain.Events[Line].MethodTiming);
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbTimeKeeping).set_SelectedIndex(num);
			num = LunarObservations.OccMain.Events[Line].Transparency - 1;
			if (num < 0)
			{
				num = 0;
			}
			if (num > 2)
			{
				num = 2;
			}
			((ListControl)cmbTransparency).set_SelectedIndex(num);
			if (LunarObservations.OccMain.Events[Line].PE_DecPlaces > 1)
			{
				((Control)txtPE).set_Text($"{LunarObservations.OccMain.Events[Line].PE:F2}");
			}
			else if (LunarObservations.OccMain.Events[Line].PE_DecPlaces == 1)
			{
				((Control)txtPE).set_Text($"{LunarObservations.OccMain.Events[Line].PE:F1}");
			}
			else
			{
				((Control)txtPE).set_Text("");
			}
			if (LunarObservations.OccMain.Events[Line].Accuracy_Valid)
			{
				if (LunarObservations.OccMain.Events[Line].Accuracy_DecPlaces > 2)
				{
					((Control)txtAccuracy).set_Text($"{LunarObservations.OccMain.Events[Line].Accuracy:F3}");
				}
				else if (LunarObservations.OccMain.Events[Line].Accuracy_DecPlaces == 2)
				{
					((Control)txtAccuracy).set_Text($"{LunarObservations.OccMain.Events[Line].Accuracy:F2}");
				}
				else if (LunarObservations.OccMain.Events[Line].Accuracy_DecPlaces == 1)
				{
					((Control)txtAccuracy).set_Text($"{LunarObservations.OccMain.Events[Line].Accuracy:F1}");
				}
				else
				{
					((Control)txtAccuracy).set_Text($"{LunarObservations.OccMain.Events[Line].Accuracy:F0}");
				}
			}
			else
			{
				((Control)txtAccuracy).set_Text("");
			}
			if (LunarObservations.OccMain.Events[Line].PE_Valid)
			{
				if (LunarObservations.OccMain.Events[Line].PE_DecPlaces > 1)
				{
					((Control)txtPE).set_Text($"{LunarObservations.OccMain.Events[Line].PE:F2}");
				}
				else
				{
					((Control)txtPE).set_Text($"{LunarObservations.OccMain.Events[Line].PE:F1}");
				}
			}
			else
			{
				((Control)txtPE).set_Text("");
			}
			if (LunarObservations.OccMain.Events[Line].SignalToNoise_Valid)
			{
				if (LunarObservations.OccMain.Events[Line].SignalToNoise_DecPlaces > 0)
				{
					((Control)txtSN).set_Text($"{LunarObservations.OccMain.Events[Line].SignalToNoise:F1}");
				}
				else
				{
					((Control)txtSN).set_Text($"{LunarObservations.OccMain.Events[Line].SignalToNoise:F0}");
				}
			}
			else
			{
				((Control)txtSN).set_Text("");
			}
			if (!LunarObservations.OccMain.Events[Line].Temperature_Valid | (LunarObservations.OccMain.Events[Line].Temperature < -20) | (LunarObservations.OccMain.Events[Line].Temperature > 44))
			{
				((ListControl)cmbTemp).set_SelectedIndex(0);
			}
			else
			{
				((ListControl)cmbTemp).set_SelectedIndex(45 - LunarObservations.OccMain.Events[Line].Temperature);
			}
			if (LunarObservations.OccMain.Events[Line].Duration_Valid)
			{
				if (LunarObservations.OccMain.Events[Line].Duration_DecPlaces > 2)
				{
					((Control)txtDuration).set_Text($"{LunarObservations.OccMain.Events[Line].Duration:F3}");
				}
				else if (LunarObservations.OccMain.Events[Line].Duration_DecPlaces == 2)
				{
					((Control)txtDuration).set_Text($"{LunarObservations.OccMain.Events[Line].Duration:F2}");
				}
				else if (LunarObservations.OccMain.Events[Line].Duration_DecPlaces == 1)
				{
					((Control)txtDuration).set_Text($"{LunarObservations.OccMain.Events[Line].Duration:F1}");
				}
				else
				{
					((Control)txtDuration).set_Text($"{LunarObservations.OccMain.Events[Line].Duration:F0}");
				}
			}
			else
			{
				((Control)txtDuration).set_Text("");
			}
			num = " TF".IndexOf(LunarObservations.OccMain.Events[Line].LightLevel);
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbLightLevel).set_SelectedIndex(num);
			((Control)txtComments).set_Text(LunarObservations.OccMain.Events[Line].Comments);
			string starCat = LunarObservations.OccMain.Events[Line].StarCat;
			string text = LunarObservations.OccMain.Events[Line].StarNumber.ToString();
			switch (starCat)
			{
			case "R":
				if (!int.TryParse(text, out num))
				{
					num = 0;
				}
				if (num < 4000)
				{
					((Control)txtZC).set_Text(text);
				}
				else if (num < 4010)
				{
					((Control)txtXZ).set_Text("");
					((ListControl)cmbMoons).set_SelectedIndex(0);
					if (!int.TryParse(text.Trim().PadLeft(6).Substring(5, 1), out num))
					{
						num = 0;
					}
					((ListControl)cmbPlanets).set_SelectedIndex(num);
				}
				else if (num < 4100)
				{
					((Control)txtXZ).set_Text("");
					if (!int.TryParse(text.Trim().PadLeft(6).Substring(4, 1), out num))
					{
						num = 0;
					}
					((ListControl)cmbPlanets).set_SelectedIndex(num);
					if (!int.TryParse(text.Trim().PadLeft(6).Substring(5, 1), out num))
					{
						num = 0;
					}
					((ListControl)cmbMoons).set_SelectedIndex(num);
				}
				break;
			case "S":
				((Control)txtSAO).set_Text(text);
				break;
			case "X":
				((Control)txtXZ).set_Text(text);
				break;
			case "G":
				((Control)txtGSC).set_Text(text);
				break;
			case "A":
				((Control)txtAsteroid).set_Text(text);
				break;
			case "P":
				if (!int.TryParse(text.Trim().PadLeft(6).Substring(2, 1), out num))
				{
					num = 0;
				}
				((ListControl)cmbPlanets).set_SelectedIndex(num);
				if (!int.TryParse(text.Trim().PadLeft(6).Substring(3), out num))
				{
					num = 0;
				}
				((ListControl)cmbMoons).set_SelectedIndex(num);
				break;
			default:
				((Control)txtXZ).set_Text(" ");
				chkUnidentified.set_Checked(true);
				break;
			}
			int num2 = -1;
			if (LunarObservations.OccMain.Events[Line].WDS.ToLower() == LunarObservations.OccMain.Events[Line].WDS)
			{
				num2 = 25;
			}
			((ListControl)cmbWDS).set_SelectedIndex(cmbWDS.FindStringExact(LunarObservations.OccMain.Events[Line].WDS, num2));
			string eventTelescope = LunarObservations.OccMain.Events[Line].EventTelescope;
			for (int i = 0; i < LunarObservations.OccMain.Telescopes.Count; i++)
			{
				if (LunarObservations.OccMain.Telescopes[i].TelescopeCodeForEvent == eventTelescope)
				{
					((ListControl)cmbSites).set_SelectedIndex(i);
					break;
				}
			}
			eventTelescope = LunarObservations.OccMain.Events[Line].EventObserver;
			for (int j = 0; j < LunarObservations.OccMain.Observers.Count; j++)
			{
				if (LunarObservations.OccMain.Observers[j].ObserverCodeForEvent == eventTelescope)
				{
					((ListControl)cmbObservers).set_SelectedIndex(j);
					break;
				}
			}
			eventTelescope = LunarObservations.OccMain.Events[Line].EventRecorder;
			for (int k = 0; k < LunarObservations.OccMain.Observers.Count; k++)
			{
				if (LunarObservations.OccMain.Observers[k].ObserverCodeForEvent == eventTelescope)
				{
					((ListControl)cmbRecorders).set_SelectedIndex(k);
					break;
				}
			}
			((Control)txtILOCEventStation).set_Text(LunarObservations.OccMain.Events[Line].Station);
			((Control)txtILOCEventTelescope).set_Text(LunarObservations.OccMain.Events[Line].Telescope);
			((Control)txtILOCEventObserver).set_Text(LunarObservations.OccMain.Events[Line].Observer);
			((Control)txtILOCEventRecorder).set_Text(LunarObservations.OccMain.Events[Line].Recorder);
			UpdatingData = false;
			GetCurrentResidual();
		}

		internal bool EncodeEvent(int Line)
		{
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_004a: Unknown result type (might be due to invalid IL or missing references)
			//IL_006d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_011a: Unknown result type (might be due to invalid IL or missing references)
			//IL_013d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Unknown result type (might be due to invalid IL or missing references)
			//IL_0183: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_020f: Unknown result type (might be due to invalid IL or missing references)
			string Limb = " ";
			string Event = " ";
			if (((ListControl)cmbCertainty).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid Certainty", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			if (((ListControl)cmbDoubles).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid Doubls star code", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			if (((ListControl)cmbMethod1).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid Method 1", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			if (((ListControl)cmbMethod2).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid Method2", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			if (((ListControl)cmbPE).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid PE application", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			if (((updnYear.get_Value() > 2008m) & (((ListControl)cmbPE).get_SelectedIndex() > 3)) | ((updnYear.get_Value() > 1981m) & (((ListControl)cmbPE).get_SelectedIndex() > 4)))
			{
				MessageBox.Show("Selected PE application is only relevant for historical observations\r\nPlease select a different value", "Incorrect PE application");
				return false;
			}
			if (((ListControl)cmbRemarkable).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid Remarkable circumstance", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			if (((ListControl)cmbStability).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid Stability", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			if (((ListControl)cmbTimeKeeping).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid Time keeping", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			if (((ListControl)cmbTransparency).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid Transparency", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			if (((ListControl)cmbLightLevel).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid Light level", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			if (((ListControl)cmbSites).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid Site", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			if (((ListControl)cmbObservers).get_SelectedIndex() < 0)
			{
				MessageBox.Show("You have't selected a valid Observer", "Data error", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return false;
			}
			LunarObservations.OccMain.Events[Line].Year = (int)updnYear.get_Value();
			LunarObservations.OccMain.Events[Line].Month = (int)updnMonth.get_Value();
			LunarObservations.OccMain.Events[Line].Day = (int)updnDay.get_Value();
			LunarObservations.OccMain.Events[Line].Hour = (int)updnHour.get_Value();
			LunarObservations.OccMain.Events[Line].Minute = (int)updnMin.get_Value();
			if (!double.TryParse(((Control)txtSecond).get_Text(), out var result))
			{
				result = 0.0;
			}
			LunarObservations.OccMain.Events[Line].Second = result;
			LunarObservations.OccMain.Events[Line].Second_DecPlaces = Utilities.DecimalPlaces(((Control)txtSecond).get_Text());
			if (LunarObservations.OccMain.Events[Line].Second_DecPlaces > 3)
			{
				LunarObservations.OccMain.Events[Line].Second_DecPlaces = 3;
			}
			LunarObservations.OccMain.Events[Line].Phase_Old = GetEventPhase(out Event, out Limb);
			LunarObservations.OccMain.Events[Line].OccEvent = Event;
			LunarObservations.OccMain.Events[Line].Limb = Limb;
			if (chkGraze.get_Checked())
			{
				LunarObservations.OccMain.Events[Line].GrazeFlag = "G";
			}
			else
			{
				LunarObservations.OccMain.Events[Line].GrazeFlag = " ";
			}
			LunarObservations.OccMain.Events[Line].GrazeFlag_old = 0;
			if (chkGraze.get_Checked())
			{
				LunarObservations.OccMain.Events[Line].GrazeFlag_old = 6;
			}
			if (optFailed.get_Checked())
			{
				LunarObservations.OccMain.Events[Line].GrazeFlag_old = 7;
			}
			if (optStarted.get_Checked())
			{
				LunarObservations.OccMain.Events[Line].GrazeFlag_old = 8;
			}
			if (optStopped.get_Checked())
			{
				LunarObservations.OccMain.Events[Line].GrazeFlag_old = 9;
			}
			LunarObservations.OccMain.Events[Line].Certainty = ((ListControl)cmbCertainty).get_SelectedIndex() + 1;
			LunarObservations.OccMain.Events[Line].DoubleStarComponent = " WENSBFU".Substring(((ListControl)cmbDoubles).get_SelectedIndex(), 1);
			LunarObservations.OccMain.Events[Line].MethodRecording1 = " GVMSTEPKXC".Substring(((ListControl)cmbMethod1).get_SelectedIndex(), 1);
			LunarObservations.OccMain.Events[Line].MethodRecording2 = " GVMSTEPKXCA".Substring(((ListControl)cmbMethod2).get_SelectedIndex(), 1);
			LunarObservations.OccMain.Events[Line].PEApplication = "SUEXAB".Substring(((ListControl)cmbPE).get_SelectedIndex(), 1);
			LunarObservations.OccMain.Events[Line].Circumstances = ((ListControl)cmbRemarkable).get_SelectedIndex();
			LunarObservations.OccMain.Events[Line].Stability = ((ListControl)cmbStability).get_SelectedIndex() + 1;
			LunarObservations.OccMain.Events[Line].MethodTiming = " GRNCTMO".Substring(((ListControl)cmbTimeKeeping).get_SelectedIndex(), 1);
			LunarObservations.OccMain.Events[Line].Transparency = ((ListControl)cmbTransparency).get_SelectedIndex() + 1;
			result = 0.0;
			LunarObservations.OccMain.Events[Line].Accuracy_Valid = double.TryParse(((Control)txtAccuracy).get_Text(), out result);
			LunarObservations.OccMain.Events[Line].Accuracy = result;
			LunarObservations.OccMain.Events[Line].Accuracy_DecPlaces = Utilities.DecimalPlaces(((Control)txtAccuracy).get_Text());
			if (LunarObservations.OccMain.Events[Line].Accuracy_DecPlaces > 3)
			{
				LunarObservations.OccMain.Events[Line].Accuracy_DecPlaces = 3;
			}
			result = 0.0;
			LunarObservations.OccMain.Events[Line].PE_Valid = double.TryParse(((Control)txtPE).get_Text(), out result);
			if (((ListControl)cmbPE).get_SelectedIndex() == 2)
			{
				LunarObservations.OccMain.Events[Line].PE_Valid = false;
			}
			LunarObservations.OccMain.Events[Line].PE = result;
			LunarObservations.OccMain.Events[Line].PE_DecPlaces = Utilities.DecimalPlaces(((Control)txtPE).get_Text());
			if (LunarObservations.OccMain.Events[Line].PE_DecPlaces > 2)
			{
				LunarObservations.OccMain.Events[Line].PE_DecPlaces = 2;
			}
			result = 0.0;
			LunarObservations.OccMain.Events[Line].SignalToNoise_Valid = double.TryParse(((Control)txtSN).get_Text(), out result);
			LunarObservations.OccMain.Events[Line].SignalToNoise = result;
			LunarObservations.OccMain.Events[Line].SignalToNoise_DecPlaces = Utilities.DecimalPlaces(((Control)txtSN).get_Text());
			if (LunarObservations.OccMain.Events[Line].SignalToNoise_DecPlaces > 1)
			{
				LunarObservations.OccMain.Events[Line].SignalToNoise_DecPlaces = 1;
			}
			result = 0.0;
			LunarObservations.OccMain.Events[Line].Duration_Valid = double.TryParse(((Control)txtDuration).get_Text(), out result);
			LunarObservations.OccMain.Events[Line].Duration = result;
			LunarObservations.OccMain.Events[Line].Duration_DecPlaces = Utilities.DecimalPlaces(((Control)txtDuration).get_Text());
			if (LunarObservations.OccMain.Events[Line].Duration_DecPlaces > 3)
			{
				LunarObservations.OccMain.Events[Line].Duration_DecPlaces = 3;
			}
			LunarObservations.OccMain.Events[Line].LightLevel = " TF".Substring(((ListControl)cmbLightLevel).get_SelectedIndex(), 1);
			result = 0.0;
			LunarObservations.OccMain.Events[Line].Temperature_Valid = ((ListControl)cmbTemp).get_SelectedIndex() > 0;
			LunarObservations.OccMain.Events[Line].Temperature = 45 - ((ListControl)cmbTemp).get_SelectedIndex();
			LunarObservations.OccMain.Events[Line].Comments = ((Control)txtComments).get_Text();
			if (!int.TryParse(((Control)txtZC).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (result2 == 0)
			{
				if (!int.TryParse(((Control)txtSAO).get_Text(), out result2))
				{
					result2 = 0;
				}
				if (result2 == 0)
				{
					if (!int.TryParse(((Control)txtXZ).get_Text(), out result2))
					{
						result2 = 0;
					}
					if (result2 == 0)
					{
						if (!int.TryParse(((Control)txtAsteroid).get_Text(), out result2))
						{
							result2 = 0;
						}
						if (result2 == 0)
						{
							if (((ListControl)cmbPlanets).get_SelectedIndex() > 0)
							{
								LunarObservations.OccMain.Events[Line].StarCat = "P";
								LunarObservations.OccMain.Events[Line].StarNumber = 1000 * ((ListControl)cmbPlanets).get_SelectedIndex() + ((ListControl)cmbMoons).get_SelectedIndex();
							}
							else
							{
								if (!int.TryParse(((Control)txtGSC).get_Text(), out result2))
								{
									result2 = 0;
								}
								if (result2 == 0)
								{
									LunarObservations.OccMain.Events[Line].StarCat = "U";
									LunarObservations.OccMain.Events[Line].StarNumber = 0;
								}
								else
								{
									LunarObservations.OccMain.Events[Line].StarCat = "G";
									LunarObservations.OccMain.Events[Line].StarNumber = result2;
								}
							}
						}
						else
						{
							LunarObservations.OccMain.Events[Line].StarCat = "A";
							LunarObservations.OccMain.Events[Line].StarNumber = result2;
						}
					}
					else
					{
						LunarObservations.OccMain.Events[Line].StarCat = "X";
						LunarObservations.OccMain.Events[Line].StarNumber = result2;
					}
				}
				else
				{
					LunarObservations.OccMain.Events[Line].StarCat = "S";
					LunarObservations.OccMain.Events[Line].StarNumber = result2;
				}
			}
			else
			{
				LunarObservations.OccMain.Events[Line].StarCat = "R";
				LunarObservations.OccMain.Events[Line].StarNumber = result2;
			}
			string starCat = LunarObservations.OccMain.Events[Line].StarCat;
			string text = LunarObservations.OccMain.Events[Line].StarNumber.ToString();
			switch (starCat)
			{
			case "R":
				((Control)txtZC).set_Text(text);
				break;
			case "S":
				((Control)txtSAO).set_Text(text);
				break;
			case "X":
				((Control)txtSAO).set_Text(text);
				break;
			}
			LunarObservations.OccMain.Events[Line].WDS = cmbWDS.get_Items().get_Item(((ListControl)cmbWDS).get_SelectedIndex()).ToString()!.PadRight(1).Substring(0, 1);
			LunarObservations.OccMain.Events[Line].EventTelescope = LunarObservations.OccMain.Telescopes[((ListControl)cmbSites).get_SelectedIndex()].TelescopeCodeForEvent;
			LunarObservations.OccMain.Events[Line].EventObserver = LunarObservations.OccMain.Observers[((ListControl)cmbObservers).get_SelectedIndex()].ObserverCodeForEvent;
			LunarObservations.OccMain.Events[Line].EventRecorder = LunarObservations.OccMain.Events[Line].EventObserver;
			LunarObservations.OccMain.Events[Line].Station = ((Control)txtILOCEventStation).get_Text().Trim().PadRight(5)
				.Substring(0, 5);
			LunarObservations.OccMain.Events[Line].Telescope = ((Control)txtILOCEventTelescope).get_Text().Trim().PadRight(2)
				.Substring(0, 2);
			LunarObservations.OccMain.Events[Line].Observer = ((Control)txtILOCEventObserver).get_Text().Trim().PadRight(2)
				.Substring(0, 2);
			LunarObservations.OccMain.Events[Line].Recorder = ((Control)txtILOCEventRecorder).get_Text().Trim().PadRight(2)
				.Substring(0, 2);
			return true;
		}

		private int GetEventPhase(out string Event, out string Limb)
		{
			Event = (Limb = " ");
			if (optUmbra.get_Checked())
			{
				Limb = "U";
			}
			if (optBrightLimb.get_Checked())
			{
				Limb = "B";
			}
			if (optDarkLimb.get_Checked())
			{
				Limb = "D";
			}
			if (optDisappear.get_Checked())
			{
				Event = "D";
			}
			if (optReappear.get_Checked())
			{
				Event = "R";
			}
			if (optBlink.get_Checked())
			{
				Event = "B";
			}
			if (optFlash.get_Checked())
			{
				Event = "F";
			}
			if (optMiss.get_Checked())
			{
				Event = "M";
			}
			if (optStarted.get_Checked())
			{
				Event = "S";
			}
			if (optStopped.get_Checked())
			{
				Event = "E";
			}
			if (optDisappear.get_Checked())
			{
				if (optUmbra.get_Checked())
				{
					return 5;
				}
				if (optBrightLimb.get_Checked())
				{
					return 3;
				}
				return 1;
			}
			if (optReappear.get_Checked())
			{
				if (optUmbra.get_Checked())
				{
					return 6;
				}
				if (optBrightLimb.get_Checked())
				{
					return 4;
				}
				return 2;
			}
			if (optUmbra.get_Checked())
			{
				return 6;
			}
			if (optBlink.get_Checked())
			{
				return 7;
			}
			if (optFlash.get_Checked())
			{
				return 8;
			}
			if (optMiss.get_Checked())
			{
				return 9;
			}
			return 0;
		}

		private void cmdToday_Click(object sender, EventArgs e)
		{
			updnYear.set_Value((decimal)DateTime.Now.ToUniversalTime().Year);
			updnMonth.set_Value((decimal)DateTime.Now.ToUniversalTime().Month);
			updnDay.set_Value((decimal)DateTime.Now.ToUniversalTime().Day);
		}

		private void cmdAddEvent_Click(object sender, EventArgs e)
		{
			if (CheckForDuplicatedDataItems(Replace: false))
			{
				return;
			}
			int num = LunarObservations.OccMain.AddNewEvent("");
			if (!EncodeEvent(num))
			{
				LunarObservations.OccMain.Events.RemoveAt(num);
				return;
			}
			DisplayReport(AllForms: true, HighlightLast: false);
			DisplayEventsInEditor(num);
			bool editsNotSaved;
			((Control)lblUnsavedEdits).set_Visible(editsNotSaved = true);
			LunarObservations.EditsNotSaved = editsNotSaved;
			if (LightData.LightCurveForm != null)
			{
				((Form)LightData.LightCurveForm).Close();
			}
			SaveReport(sender, e);
		}

		private void cmdReplaceEvent_Click(object sender, EventArgs e)
		{
			//IL_004c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstEvents).get_SelectedIndex();
			if (selectedIndex >= 0 && !CheckForDuplicatedDataItems(Replace: true) && (int)MessageBox.Show("Do you want to replace the event:\r\n\r\n" + LunarObservations.OccMain.Events[selectedIndex].ToString(), "Confirm replace", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2 && EncodeEvent(selectedIndex))
			{
				DisplayReport(AllForms: true, HighlightLast: false);
				if (selectedIndex < lstEvents.get_Items().get_Count() - 2)
				{
					DisplayEventsInEditor(selectedIndex + 1);
				}
				else
				{
					DisplayEventsInEditor(selectedIndex);
				}
				bool editsNotSaved;
				((Control)lblUnsavedEdits).set_Visible(editsNotSaved = true);
				LunarObservations.EditsNotSaved = editsNotSaved;
				if (LightData.LightCurveForm != null)
				{
					((Form)LightData.LightCurveForm).Close();
				}
			}
		}

		private bool CheckForDuplicatedDataItems(bool Replace)
		{
			//IL_0381: Unknown result type (might be due to invalid IL or missing references)
			//IL_0387: Invalid comparison between Unknown and I4
			int num = ((ListControl)lstEvents).get_SelectedIndex();
			if (Replace && num > 0)
			{
				num--;
			}
			string text = "";
			string text2 = "adding";
			string text3 = "selected";
			if (Replace)
			{
				text2 = "replacing";
				text3 = "previous";
			}
			if (((ListControl)cmbMethod1).get_SelectedIndex() < 1 && ((ListControl)cmbMethod2).get_SelectedIndex() > 0)
			{
				text += "\r\n- Please use the first (left) box to specify the Method of timing & recording";
			}
			if (num >= 0)
			{
				if (((LunarObservations.OccMain.Events[num].Circumstances == 1) | (LunarObservations.OccMain.Events[num].Circumstances > 2)) && LunarObservations.OccMain.Events[num].Circumstances == ((ListControl)cmbRemarkable).get_SelectedIndex())
				{
					text = text + "\r\n- Circumstances is same as the " + text3 + " observation.";
				}
				if (LunarObservations.OccMain.Events[num].DoubleStarComponent != " " && LunarObservations.OccMain.Events[num].DoubleStarComponent == " WENSBFU".Substring(((ListControl)cmbDoubles).get_SelectedIndex(), 1))
				{
					text = text + "\r\n- Double star type is same as the " + text3 + " observation.";
				}
				if (LunarObservations.OccMain.Events[num].WDS != " " && LunarObservations.OccMain.Events[num].WDS == cmbWDS.get_Items().get_Item(((ListControl)cmbWDS).get_SelectedIndex()).ToString()!.PadRight(1).Substring(0, 1))
				{
					text = text + "\r\n- WDS identifier is same as the " + text3 + " observation.";
				}
				if (LunarObservations.OccMain.Events[num].Duration_Valid)
				{
					double.TryParse(((Control)txtDuration).get_Text(), out var result);
					if (LunarObservations.OccMain.Events[num].Duration == result)
					{
						text = text + "\r\n- Duration is same as the " + text3 + " observation.";
					}
				}
				if (LunarObservations.OccMain.Events[num].LightLevel != " " && LunarObservations.OccMain.Events[num].LightLevel == " TF".Substring(((ListControl)cmbLightLevel).get_SelectedIndex(), 1))
				{
					text = text + "\r\n- Light level is same as the " + text3 + " observation.";
				}
				if ((LunarObservations.OccMain.Events[num].GrazeFlag == "G") & chkGraze.get_Checked())
				{
					int num2 = 0;
					int result2 = 0;
					string starCat = LunarObservations.OccMain.Events[num].StarCat;
					num2 = LunarObservations.OccMain.Events[num].StarNumber;
					if (starCat == "S")
					{
						if (XZ80Q.Get_SAO_Star(num2))
						{
							num2 = XZ80Q.XZ;
						}
					}
					else if (starCat == "R" && XZ80Q.Get_ZC_Star(num2))
					{
						num2 = XZ80Q.XZ;
					}
					if (!int.TryParse(((Control)txtXZ).get_Text(), out result2))
					{
						result2 = 0;
					}
					if (num2 != result2)
					{
						text = text + "\r\n- Graze is for a different star than the " + text3 + " observation.";
					}
				}
			}
			if (text.Length > 0 && (int)MessageBox.Show("The following potential errors have been identified:\r\n" + text + "\r\n\r\nDo you want to continue with " + text2 + " this event?", "Possible data errors", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return true;
			}
			return false;
		}

		private void cmdDeleteEvent_Click(object sender, EventArgs e)
		{
			//IL_0028: Unknown result type (might be due to invalid IL or missing references)
			//IL_002e: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstEvents).get_SelectedIndex();
			if (selectedIndex >= 0 && (int)MessageBox.Show("Do you want to delete this event?", "Confirm deletion", (MessageBoxButtons)1, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				LunarObservations.OccMain.Events.RemoveAt(selectedIndex);
				DisplayReport(AllForms: true, HighlightLast: false);
				DisplayEventsInEditor(selectedIndex);
				bool editsNotSaved;
				((Control)lblUnsavedEdits).set_Visible(editsNotSaved = true);
				LunarObservations.EditsNotSaved = editsNotSaved;
				((ToolStripItem)createLightCurveReportToolStripMenuItem).set_Enabled(((ListControl)lstEvents).get_SelectedIndex() >= 0);
			}
		}

		private void cmdRenumberEvents_Click(object sender, EventArgs e)
		{
			LunarObservations.OccMain.ReNumberEvents();
			DisplayReport(AllForms: true, HighlightLast: false);
			DisplayEventsInEditor(0);
		}

		private void cmdSortEvents_Click(object sender, EventArgs e)
		{
			if (optByNumber.get_Checked())
			{
				EventLine.SortField = 0;
			}
			else if (optByTEL.get_Checked())
			{
				EventLine.SortField = 1;
			}
			else if (optByOBS.get_Checked())
			{
				EventLine.SortField = 2;
			}
			else
			{
				EventLine.SortField = 3;
			}
			LunarObservations.OccMain.Events.Sort();
			DisplayReport(AllForms: true, HighlightLast: false);
			DisplayEventsInEditor(0);
			bool editsNotSaved;
			((Control)lblUnsavedEdits).set_Visible(editsNotSaved = true);
			LunarObservations.EditsNotSaved = editsNotSaved;
		}

		private void txtZC_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtZC).get_Text(), out var result))
				{
					result = 0;
				}
				if (result == 0)
				{
					TextBox obj = txtZC;
					TextBox obj2 = txtSAO;
					string text;
					((Control)txtXZ).set_Text(text = "");
					string text2;
					((Control)obj2).set_Text(text2 = text);
					((Control)obj).set_Text(text2);
				}
				else if ((result > 4000 && result < 4009) || (result > 4051 && result <= 4054) || (result > 4061 && result <= 4068))
				{
					TextBox obj3 = txtSAO;
					string text2;
					((Control)txtXZ).set_Text(text2 = "");
					((Control)obj3).set_Text(text2);
				}
				else
				{
					XZ80Q.Get_ZC_Star(result);
					((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
					((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				}
				((Control)lblSAOValid).set_Visible(false);
				ComboBox obj4 = cmbPlanets;
				int selectedIndex;
				((ListControl)cmbMoons).set_SelectedIndex(selectedIndex = 0);
				((ListControl)obj4).set_SelectedIndex(selectedIndex);
				((Control)txtAsteroid).set_Text("");
				Set_WDS_Label(XZ80Q.XZ);
				ChangingStar = false;
				GetCurrentResidual();
				if (((Control)txtXZ).get_Text().Trim() != "")
				{
					chkUnidentified.set_Checked(false);
				}
			}
		}

		private void txtSAO_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtSAO).get_Text(), out var result))
				{
					result = 0;
				}
				if (result == 0)
				{
					TextBox obj = txtZC;
					TextBox obj2 = txtSAO;
					string text;
					((Control)txtXZ).set_Text(text = "");
					string text2;
					((Control)obj2).set_Text(text2 = text);
					((Control)obj).set_Text(text2);
				}
				else if (XZ80Q.Get_SAO_Star(result))
				{
					((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
					((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
					((Control)lblSAOValid).set_Visible(false);
					((Control)lblIsDouble).set_Visible(false);
				}
				else
				{
					TextBox obj3 = txtZC;
					string text2;
					((Control)txtXZ).set_Text(text2 = "0");
					((Control)obj3).set_Text(text2);
					((Control)lblSAOValid).set_Visible(true);
				}
				ComboBox obj4 = cmbPlanets;
				int selectedIndex;
				((ListControl)cmbMoons).set_SelectedIndex(selectedIndex = 0);
				((ListControl)obj4).set_SelectedIndex(selectedIndex);
				((Control)txtAsteroid).set_Text("");
				Set_WDS_Label(XZ80Q.XZ);
				ChangingStar = false;
				GetCurrentResidual();
				if (((Control)txtXZ).get_Text().Trim() != "")
				{
					chkUnidentified.set_Checked(false);
				}
			}
		}

		private void txtXZ_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtXZ).get_Text(), out var result))
				{
					result = 0;
				}
				if (result == 0)
				{
					TextBox obj = txtZC;
					TextBox obj2 = txtSAO;
					string text;
					((Control)txtXZ).set_Text(text = "");
					string text2;
					((Control)obj2).set_Text(text2 = text);
					((Control)obj).set_Text(text2);
				}
				else
				{
					XZ80Q.Get_XZ_Star(result);
					((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
					((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				}
				((Control)lblSAOValid).set_Visible(false);
				ComboBox obj3 = cmbPlanets;
				int selectedIndex;
				((ListControl)cmbMoons).set_SelectedIndex(selectedIndex = 0);
				((ListControl)obj3).set_SelectedIndex(selectedIndex);
				((Control)txtAsteroid).set_Text("");
				Set_WDS_Label(result);
				ChangingStar = false;
				GetCurrentResidual();
				if (((Control)txtXZ).get_Text().Trim() != "")
				{
					chkUnidentified.set_Checked(false);
				}
			}
		}

		private void Set_WDS_Label(int XZ)
		{
			string[] array = new string[4] { "", "", "", "" };
			bool[] isMean = new bool[4];
			double[] posnAngle = new double[4];
			double[] separation = new double[4];
			string ComponentIDs = "";
			string DiscovererName = "";
			if (XZ > 0)
			{
				DoubleStars.GetXZDoubleMatches(XZ, 2450000.0, ForReductions: false, out var _, array, posnAngle, separation, isMean, out ComponentIDs, out DiscovererName, out var _, out var _);
			}
			((Control)lblWDS).set_Text("WDS [" + ComponentIDs + "]");
			((Control)lblIsDouble).set_Visible(ComponentIDs.Trim().Length > 0);
			if (XZ > 0)
			{
				((Control)lblK2Star).set_Visible(XZ80Q.DoubleFlag.ToUpper() == "K");
			}
			else
			{
				((Control)lblK2Star).set_Visible(false);
			}
		}

		private void chkUnidentified_CheckedChanged(object sender, EventArgs e)
		{
			if (!ChangingStar && chkUnidentified.get_Checked())
			{
				ChangingStar = true;
				TextBox obj = txtZC;
				TextBox obj2 = txtSAO;
				TextBox obj3 = txtXZ;
				string text;
				((Control)txtGSC).set_Text(text = "");
				string text2;
				((Control)obj3).set_Text(text2 = text);
				string text3;
				((Control)obj2).set_Text(text3 = text2);
				((Control)obj).set_Text(text3);
				ComboBox obj4 = cmbPlanets;
				int selectedIndex;
				((ListControl)cmbMoons).set_SelectedIndex(selectedIndex = 0);
				((ListControl)obj4).set_SelectedIndex(selectedIndex);
				Set_WDS_Label(0);
				ChangingStar = false;
			}
		}

		private void cmbPlanets_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				cmbMoons.get_Items().Clear();
				cmbMoons.get_Items().Add((object)"");
				((ListControl)cmbMoons).set_SelectedIndex(0);
				switch (((ListControl)cmbPlanets).get_SelectedIndex())
				{
				case 4:
					cmbMoons.get_Items().Add((object)"Phobos");
					cmbMoons.get_Items().Add((object)"Deimos");
					break;
				case 5:
					cmbMoons.get_Items().Add((object)"Io");
					cmbMoons.get_Items().Add((object)"Europa");
					cmbMoons.get_Items().Add((object)"Ganymede");
					cmbMoons.get_Items().Add((object)"Callisto");
					cmbMoons.get_Items().Add((object)"Amalthea");
					cmbMoons.get_Items().Add((object)"Himalia");
					cmbMoons.get_Items().Add((object)"Elara");
					cmbMoons.get_Items().Add((object)"Pasiphae");
					cmbMoons.get_Items().Add((object)"Sinope");
					cmbMoons.get_Items().Add((object)"Lysithea");
					cmbMoons.get_Items().Add((object)"Carme");
					cmbMoons.get_Items().Add((object)"Ananke");
					cmbMoons.get_Items().Add((object)"Leda");
					cmbMoons.get_Items().Add((object)"Thebe");
					cmbMoons.get_Items().Add((object)"Adrastea");
					cmbMoons.get_Items().Add((object)"Metis");
					break;
				case 6:
					cmbMoons.get_Items().Add((object)"Mimas");
					cmbMoons.get_Items().Add((object)"Enceladus");
					cmbMoons.get_Items().Add((object)"Tethys");
					cmbMoons.get_Items().Add((object)"Dione");
					cmbMoons.get_Items().Add((object)"Rhea");
					cmbMoons.get_Items().Add((object)"Titan");
					cmbMoons.get_Items().Add((object)"Hyperion");
					cmbMoons.get_Items().Add((object)"Iapetus");
					cmbMoons.get_Items().Add((object)"Phoebe");
					break;
				case 7:
					cmbMoons.get_Items().Add((object)"Ariel");
					cmbMoons.get_Items().Add((object)"Umbriel");
					cmbMoons.get_Items().Add((object)"Titania");
					cmbMoons.get_Items().Add((object)"Oberon");
					cmbMoons.get_Items().Add((object)"Miranda");
					cmbMoons.get_Items().Add((object)"Cordelia");
					cmbMoons.get_Items().Add((object)"Orphelia");
					cmbMoons.get_Items().Add((object)"Bianca");
					cmbMoons.get_Items().Add((object)"Cressida");
					cmbMoons.get_Items().Add((object)"Desdemona");
					cmbMoons.get_Items().Add((object)"Juliet");
					cmbMoons.get_Items().Add((object)"Portia");
					cmbMoons.get_Items().Add((object)"Rosalind");
					cmbMoons.get_Items().Add((object)"Belinda");
					cmbMoons.get_Items().Add((object)"Puck");
					break;
				case 8:
					cmbMoons.get_Items().Add((object)"Triton");
					cmbMoons.get_Items().Add((object)"Nereid");
					break;
				case 9:
					cmbMoons.get_Items().Add((object)"Charon");
					cmbMoons.get_Items().Add((object)"Hydra");
					cmbMoons.get_Items().Add((object)"Nix");
					break;
				}
				TextBox obj = txtZC;
				TextBox obj2 = txtSAO;
				TextBox obj3 = txtXZ;
				string text;
				((Control)txtAsteroid).set_Text(text = "");
				string text2;
				((Control)obj3).set_Text(text2 = text);
				string text3;
				((Control)obj2).set_Text(text3 = text2);
				((Control)obj).set_Text(text3);
				Set_WDS_Label(0);
				ChangingStar = false;
				GetCurrentResidual();
				if (((ListControl)cmbPlanets).get_SelectedIndex() > 0)
				{
					chkUnidentified.set_Checked(false);
				}
			}
		}

		private void cmbMoons_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				TextBox obj = txtZC;
				TextBox obj2 = txtSAO;
				TextBox obj3 = txtXZ;
				string text;
				((Control)txtAsteroid).set_Text(text = "");
				string text2;
				((Control)obj3).set_Text(text2 = text);
				string text3;
				((Control)obj2).set_Text(text3 = text2);
				((Control)obj).set_Text(text3);
				Set_WDS_Label(0);
				ChangingStar = false;
				GetCurrentResidual();
				if (((ListControl)cmbMoons).get_SelectedIndex() > 0)
				{
					chkUnidentified.set_Checked(false);
				}
			}
		}

		private void txtAsteroid_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				ComboBox obj = cmbPlanets;
				int selectedIndex;
				((ListControl)cmbMoons).set_SelectedIndex(selectedIndex = 0);
				((ListControl)obj).set_SelectedIndex(selectedIndex);
				TextBox obj2 = txtZC;
				TextBox obj3 = txtSAO;
				string text;
				((Control)txtXZ).set_Text(text = "");
				string text2;
				((Control)obj3).set_Text(text2 = text);
				((Control)obj2).set_Text(text2);
				Set_WDS_Label(0);
				ChangingStar = false;
				GetCurrentResidual();
				if (((Control)txtAsteroid).get_Text().Trim() != "")
				{
					chkUnidentified.set_Checked(false);
				}
			}
		}

		private void cmdIdentify_Click(object sender, EventArgs e)
		{
			IdentifyStar();
		}

		internal void IdentifyStar()
		{
			double num = 0.0;
			string Event = " ";
			string Limb = " ";
			if (!double.TryParse(((Control)txtSecond).get_Text(), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtPE).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			if (((ListControl)cmbPE).get_SelectedIndex() == 1)
			{
				num = result2;
			}
			else if (((ListControl)cmbPE).get_SelectedIndex() == 3)
			{
				num = ((!(optDisappear.get_Checked() | optBlink.get_Checked() | optFlash.get_Checked())) ? 0.99 : 0.48);
			}
			double jD_atEvent = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value()) + ((double)(int)updnHour.get_Value() + (double)(int)updnMin.get_Value() / 60.0 + (result + num) / 3600.0) / 24.0;
			int selectedIndex = ((ListControl)cmbSites).get_SelectedIndex();
			if (selectedIndex >= 0)
			{
				double longitude = LunarObservations.OccMain.Telescopes[selectedIndex].Longitude;
				double latitude = LunarObservations.OccMain.Telescopes[selectedIndex].Latitude;
				double altitude = LunarObservations.OccMain.Telescopes[selectedIndex].Altitude;
				int datumNumber = LunarObservations.OccMain.Telescopes[selectedIndex].DatumNumber;
				GetEventPhase(out Event, out Limb);
				LunarObservations.IdentifyXZStar(jD_atEvent, longitude, latitude, altitude, datumNumber, Event, 30.0, fromAutoCorrect: false, fromManualDetect: false);
			}
		}

		private void GetCurrentResidual()
		{
			if (!UpdatingData)
			{
				double Residual = 0.0;
				double PA = 0.0;
				double MagStar = 0.0;
				double num = 0.0;
				bool flag = false;
				if (!double.TryParse(((Control)txtSecond).get_Text(), out var result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtPE).get_Text(), out var result2))
				{
					result2 = 0.0;
				}
				flag = false;
				if (((ListControl)cmbPE).get_SelectedIndex() == 1)
				{
					num = result2;
				}
				else if (((ListControl)cmbPE).get_SelectedIndex() == 3)
				{
					num = ((!(optDisappear.get_Checked() | optBlink.get_Checked() | optFlash.get_Checked())) ? 0.99 : 0.48);
					flag = true;
				}
				if (!int.TryParse(((Control)txtXZ).get_Text(), out var result3))
				{
					result3 = 0;
				}
				if (!int.TryParse(((Control)txtAsteroid).get_Text(), out var result4))
				{
					result4 = 0;
				}
				double num2 = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value()) + ((double)(int)updnHour.get_Value() + (double)(int)updnMin.get_Value() / 60.0 + (result - num) / 3600.0) / 24.0;
				if (((ListControl)cmbWDS).get_SelectedIndex() < 0)
				{
					((ListControl)cmbWDS).set_SelectedIndex(0);
				}
				double MoonRadius;
				if (((ListControl)cmbPlanets).get_SelectedIndex() > 0)
				{
					LunarObservations.Reduce_CurrentEntry(num2, ((ListControl)cmbSites).get_SelectedIndex(), "P", 1000 * ((ListControl)cmbPlanets).get_SelectedIndex() + ((ListControl)cmbMoons).get_SelectedIndex(), flag, " ", out Residual, out PA, out MagStar, out MoonRadius);
				}
				else if (result4 > 0 && result3 == 0)
				{
					LunarObservations.Reduce_CurrentEntry(num2, ((ListControl)cmbSites).get_SelectedIndex(), "A", result4, flag, " ", out Residual, out PA, out MagStar, out MoonRadius);
				}
				else
				{
					LunarObservations.Reduce_CurrentEntry(num2, ((ListControl)cmbSites).get_SelectedIndex(), "X", result3, flag, cmbWDS.get_Items().get_Item(((ListControl)cmbWDS).get_SelectedIndex()).ToString(), out Residual, out PA, out MagStar, out MoonRadius);
				}
				((Control)txtResidual).set_Text($"{Residual:F2}");
				((Control)txtPA).set_Text($"{PA * 180.0 / Math.PI:F2}");
				((Control)txtMag).set_Text($"{MagStar:F1}");
				ShowCurrentDouble(result3, num2, cmbWDS.get_Items().get_Item(((ListControl)cmbWDS).get_SelectedIndex()).ToString());
			}
		}

		private void ShowCurrentDouble(int XZ, double JD, string WDScode)
		{
			if ((XZ > 0) & (WDScode != " "))
			{
				LunarObservations.Get_DoubleStar_Sep_PA(XZ, JD, WDScode, out var ComponentSep, out var ComponentPA);
				((Control)lblSep).set_Text(string.Format("Sep = {0,2:F2}", ComponentSep));
				((Control)lblPA).set_Text(string.Format("PA = {0,2:F1}", ComponentPA));
			}
			else
			{
				Label obj = lblSep;
				string text;
				((Control)lblPA).set_Text(text = "...");
				((Control)obj).set_Text(text);
			}
		}

		private void cmbPE_SelectedIndexChanged(object sender, EventArgs e)
		{
			//IL_0096: Unknown result type (might be due to invalid IL or missing references)
			if (((ListControl)cmbPE).get_SelectedIndex() == 2)
			{
				((Control)txtPE).set_Text("0.0");
				((Control)txtPE).set_Enabled(false);
				return;
			}
			((Control)txtPE).set_Enabled(true);
			if (((updnYear.get_Value() > 2008m) & (((ListControl)cmbPE).get_SelectedIndex() > 3)) | ((updnYear.get_Value() > 1981m) & (((ListControl)cmbPE).get_SelectedIndex() > 4)))
			{
				MessageBox.Show("Selected PE application is only relevant for historical observations\r\nPlease select a different value", "Incorrect PE application");
				((Control)cmbPE).Focus();
			}
			GetCurrentResidual();
		}

		private void updnMonth_ValueChanged(object sender, EventArgs e)
		{
			GetCurrentResidual();
		}

		private void updnDay_ValueChanged(object sender, EventArgs e)
		{
			GetCurrentResidual();
		}

		private void updnHour_ValueChanged(object sender, EventArgs e)
		{
			GetCurrentResidual();
		}

		private void updnMin_ValueChanged(object sender, EventArgs e)
		{
			GetCurrentResidual();
		}

		private void txtSecond_TextChanged(object sender, EventArgs e)
		{
			GetCurrentResidual();
		}

		private void cmbSites_SelectedIndexChanged(object sender, EventArgs e)
		{
			GetCurrentResidual();
		}

		private void txtPE_TextChanged(object sender, EventArgs e)
		{
			GetCurrentResidual();
		}

		private void cmbWDS_SelectedIndexChanged(object sender, EventArgs e)
		{
			GetCurrentResidual();
		}

		private void cmdReduce_Click(object sender, EventArgs e)
		{
			Reduce();
		}

		internal void Reduce()
		{
			//IL_009f: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Invalid comparison between Unknown and I4
			DateTime value = new DateTime(1962, 1, 1);
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\EOP_present.dat"))
			{
				value = File.GetLastWriteTime(Utilities.AppPath + "\\Resource Files\\EOP_present.dat");
			}
			DateTime value2 = DateTime.Now.AddDays(-(int)Settings.Default.EOPReminder);
			if (value.CompareTo(value2) < 0 && (int)MessageBox.Show("The file of Earth Orientation Parameters is " + DateTime.Now.Subtract(value).Days + " days old\r\n\r\nFor accurate reductions you need to download the file before it is more than 20 days old\r\n\r\nDo you want to download the file now?", "Download EOP file", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				((Control)this).set_Cursor(Cursors.get_WaitCursor());
				ftp.Download_EOP_current(SupressMessages: true);
				((Control)this).set_Cursor(Cursors.get_Default());
			}
			try
			{
				((Form)ReductionProfile.ObservedProfile).Close();
				((Component)(object)ReductionProfile.ObservedProfile).Dispose();
			}
			catch
			{
			}
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			LunarObservations.Reduce_Displayed_File();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void grpHeader_Leave(object sender, EventArgs e)
		{
			DisplayReport(AllForms: false, HighlightLast: false);
		}

		private void ObservationsEditor_Resize(object sender, EventArgs e)
		{
			((Control)grpEvents).set_Height(((Control)this).get_Height() - 110);
			((Control)lstEvents).set_Height(((Control)grpEvents).get_Height() - 360);
			((Control)grpSites).set_Height(((Control)this).get_Height() - 110);
			((Control)lstSites).set_Height(((Control)grpSites).get_Height() - 360);
			((Control)grpNames).set_Height(((Control)this).get_Height() - 110);
			((Control)lstNames).set_Height(((Control)grpNames).get_Height() - 360);
			((Control)lstReport).set_Height(((Control)this).get_Height() - 106);
		}

		private void displayCurrentSitesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			if (LunarObservations.OccMain.Telescopes.Count < 0)
			{
				return;
			}
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				if (!GoogleEarth.Create_New_GoogleEarthKMZ_File(Utilities.AppPath + "\\Observations\\LunarSites.KML", "Sites", AutoOpenFile: true, out var CreatedFile))
				{
					return;
				}
				for (int i = 0; i < LunarObservations.OccMain.Telescopes.Count; i++)
				{
					int num = LunarObservations.OccMain.Telescopes[i].DatumNumber;
					double longitudeDeg = LunarObservations.OccMain.Telescopes[i].LongitudeDeg;
					double latitudeDeg = LunarObservations.OccMain.Telescopes[i].LatitudeDeg;
					double DLatitude_arcsec;
					double DLongitude_arcsec = (DLatitude_arcsec = 0.0);
					if (num > 0 && num < 6)
					{
						num = Utilities.RGOdatum_to_ILOCdatum(num, longitudeDeg, latitudeDeg);
					}
					Utilities.GetDatumToWGS84Corrections(num, longitudeDeg / (180.0 / Math.PI), latitudeDeg / (180.0 / Math.PI), 0.0, out DLongitude_arcsec, out DLatitude_arcsec);
					longitudeDeg += DLongitude_arcsec / 3600.0;
					latitudeDeg += DLatitude_arcsec / 3600.0;
					GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML(LunarObservations.OccMain.Telescopes[i].TelescopeCodeForEvent, longitudeDeg, latitudeDeg);
				}
				CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
				GoogleEarth.DisplayGoogleMap(CreatedFile);
			}
		}

		private void plotMoonFirst10EventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.GoogleEarthMoonOutline_DisplayedFile(9, GetHeight(), includeLimbProfile.get_Checked());
		}

		private void plotMoonFirst20EventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.GoogleEarthMoonOutline_DisplayedFile(19, GetHeight(), includeLimbProfile.get_Checked());
		}

		private void plotMoonFirst50EventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.GoogleEarthMoonOutline_DisplayedFile(49, GetHeight(), includeLimbProfile.get_Checked());
		}

		private void plotMoonAllEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.GoogleEarthMoonOutline_DisplayedFile(-1, GetHeight(), includeLimbProfile.get_Checked());
		}

		private int GetHeight()
		{
			if (!int.TryParse(((ToolStripItem)mnuGoogleHeight).get_Text(), out var result))
			{
				return 0;
			}
			return result;
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void addGrazeToRecentGrazesFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AddGrazesToFiles(RecentGrazes: true);
		}

		private void AddGrazesToFiles(bool RecentGrazes)
		{
			//IL_016b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0171: Invalid comparison between Unknown and I4
			//IL_0191: Unknown result type (might be due to invalid IL or missing references)
			//IL_0197: Invalid comparison between Unknown and I4
			string text = "";
			bool flag = false;
			bool flag2 = false;
			string text2 = ((!RecentGrazes) ? "OccultGrazes.dat" : "RecentGrazes.dat");
			string text3 = LunarObservations.OccMain.Events[1].Year + LunarObservations.OccMain.Events[1].Month.ToString().PadLeft(2, '0') + LunarObservations.OccMain.Events[1].Day.ToString().PadLeft(2, '0');
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\" + text2, Encoding.ASCII))
			{
				flag = false;
				flag2 = false;
				do
				{
					string text4 = streamReader.ReadLine()!.PadRight(3);
					if (!flag2 && int.TryParse(text4.Substring(0, 2), out var _))
					{
						if (text4.Substring(2, 8).Replace(" ", "0") == text3)
						{
							flag = true;
							text = text + text4 + "\r\n";
						}
						flag2 = true;
					}
					if (text4.Substring(0, 3) == "***")
					{
						flag2 = false;
					}
				}
				while (!streamReader.EndOfStream);
			}
			if (flag)
			{
				text = "Events for " + text3 + " have been found; the first record in each is as follows:\r\n" + text;
			}
			if (RecentGrazes)
			{
				if ((int)MessageBox.Show(text + "Do you want to add this event to the file of Recent Graze observations?", "Confirm Add", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
			}
			else if ((int)MessageBox.Show(text + "Do you want to add this event to the Main file of Graze observations?", "Confirm Add", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			LunarObservations.OccMain.AppendReportToGrazeFiles(text2);
		}

		private void addGrazeToMainFileOfGrazesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			AddGrazesToFiles(RecentGrazes: false);
		}

		private void pasteToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PasteObservations();
		}

		internal void PasteObservations()
		{
			if (UnsavedDataNeedsToBeSaved())
			{
				return;
			}
			string text = Clipboard.GetText();
			int num = -1;
			int num2 = 0;
			ReSetForm();
			((Control)this).set_Text("Observations editor : PastedEvents");
			LunarObservations.OccMain.Reported = "";
			LunarObservations.OccMain.EMail = "";
			LunarObservations.OccMain.Address = "";
			LunarObservations.OccMain.Representative = "Pasted report";
			LunarObservations.OccMain.Place = "";
			while (num2 >= 0)
			{
				num = text.IndexOf("PLACE", num2 + 1);
				if (num < 0)
				{
					Clipboard.SetText(text.Substring(num2));
				}
				else
				{
					Clipboard.SetText(text.Substring(num2, num - num2));
				}
				if (num2 == 0)
				{
					LunarObservations.OccMain.ReadPastedReport();
				}
				else
				{
					PasteMergeObservations();
				}
				num2 = num;
			}
			LunarObservations.OccMain.ReNumberEvents();
			DisplayReport(AllForms: true, HighlightLast: true);
			ToolStripMenuItem obj = importToolStripMenuItem;
			bool enabled;
			((ToolStripItem)pasteMergeToolStripMenuItem).set_Enabled(enabled = true);
			((ToolStripItem)obj).set_Enabled(enabled);
			LunarObservations.CurrentSourceFile = Path.GetFileName(Current_OCC_File);
			LunarObservations.VerifyReportData(ShowIfNoErrors: false);
			((Control)lblUnsavedEdits).set_Visible(enabled = true);
			LunarObservations.EditsNotSaved = enabled;
			optEvents.set_Checked(true);
			SetEditFrame();
		}

		private void pasteMergeToolStripMenuItem_Click(object sender, EventArgs e)
		{
			PasteMergeObservations();
			DisplayReport(AllForms: true, HighlightLast: false);
			LunarObservations.VerifyReportData(ShowIfNoErrors: false);
			bool editsNotSaved;
			((Control)lblUnsavedEdits).set_Visible(editsNotSaved = true);
			LunarObservations.EditsNotSaved = editsNotSaved;
		}

		private void PasteMergeObservations()
		{
			string Line = "";
			string Line2 = "";
			EventLine.UseOldFormat = chkFormat.get_Checked();
			TelescopeLine.UseOldFormat = chkFormat.get_Checked();
			ObserverLine.UseOldFormat = chkFormat.get_Checked();
			LunarObservations.OccMerge.Events.Clear();
			LunarObservations.OccMerge.Observers.Clear();
			LunarObservations.OccMerge.Telescopes.Clear();
			((Control)this).set_Text(((Control)this).get_Text() + " + PastedEvents");
			LunarObservations.OccMerge.ReadPastedReport();
			LunarObservations.OccMain.ReNumberSites("A");
			LunarObservations.OccMain.ReNumberNames("A");
			LunarObservations.OccMerge.ReNumberSites(LunarObservations.OccMain.GetNextSiteID());
			LunarObservations.OccMerge.ReNumberNames(LunarObservations.OccMain.GetNextNameID());
			int count = LunarObservations.OccMain.Telescopes.Count;
			for (int i = 0; i < LunarObservations.OccMerge.Telescopes.Count; i++)
			{
				LunarObservations.OccMain.AddNewTelescopeLine(LunarObservations.OccMerge.Telescopes[i].ToString());
				LunarObservations.OccMain.Telescopes[count + i].TelescopePlace = LunarObservations.OccMerge.Place;
			}
			for (int j = 0; j < LunarObservations.OccMerge.Observers.Count; j++)
			{
				LunarObservations.OccMain.AddNewObserverLine(LunarObservations.OccMerge.Observers[j].ToString());
			}
			for (int k = 0; k < LunarObservations.OccMerge.Events.Count; k++)
			{
				LunarObservations.OccMerge.Events[k].GetEventLines(out Line, out Line2);
				if (Line2.Trim().Length < 1)
				{
					Line2 = "";
				}
				LunarObservations.OccMain.AddNewEventLine(Line, Line2);
			}
			EventLine.SortField = 3;
			LunarObservations.OccMain.Events.Sort();
			LunarObservations.OccMain.ReNumberEvents();
			DisplaySitesInEditor(0);
			verifyDataEntryToolStripMenuItem.set_Checked(false);
			identifyUnidentifiedStarsInReportToolStripMenuItem.set_Checked(false);
			listPossibleDoubleStarsToolStripMenuItem.set_Checked(false);
			verifyAccuracyValuesToolStripMenuItem.set_Checked(false);
			optEvents.set_Checked(true);
			SetEditFrame();
		}

		private void chkFormat_CheckedChanged(object sender, EventArgs e)
		{
			OccultationReport.UseOldFormat = chkFormat.get_Checked();
			EventLine.UseOldFormat = (TelescopeLine.UseOldFormat = (ObserverLine.UseOldFormat = chkFormat.get_Checked()));
			((Control)optWGS).set_Enabled(!chkFormat.get_Checked());
			DisplayReport(AllForms: true, HighlightLast: false);
			((ToolStripItem)addGrazeToRecentGrazesFileToolStripMenuItem).set_Visible(chkFormat.get_Checked());
			((ToolStripItem)addGrazeToMainFileOfGrazesToolStripMenuItem).set_Visible(chkFormat.get_Checked());
		}

		private void txtSN_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtSN).SelectAll();
		}

		private void txtDuration_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtDuration).SelectAll();
		}

		private void txtSecond_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtSecond).SelectAll();
		}

		private void txtZC_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtZC).SelectAll();
		}

		private void txtSAO_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtSAO).SelectAll();
		}

		private void txtXZ_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtXZ).SelectAll();
		}

		private void txtAsteroid_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtAsteroid).SelectAll();
		}

		private void txtGSC_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtGSC).SelectAll();
		}

		private void txtPE_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtPE).SelectAll();
		}

		private void txtAccuracy_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtAccuracy).SelectAll();
		}

		private void txtILOCStation_Name_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtILOCStation).SelectAll();
		}

		private void txtILOCName_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtILOCName).SelectAll();
		}

		private void txtILOCStation_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtILOCStation).SelectAll();
		}

		private void txtILOCTelescope_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtILOCTelescope).SelectAll();
		}

		private void optBlink_Click(object sender, EventArgs e)
		{
			if (optBlink.get_Checked())
			{
				chkGraze.set_Checked(true);
			}
		}

		private void optFlash_Click(object sender, EventArgs e)
		{
			if (optFlash.get_Checked())
			{
				chkGraze.set_Checked(true);
			}
		}

		private void optMiss_Click(object sender, EventArgs e)
		{
			if (optMiss.get_Checked())
			{
				chkGraze.set_Checked(true);
			}
		}

		private void optStarted_Click(object sender, EventArgs e)
		{
			if (optStarted.get_Checked())
			{
				chkGraze.set_Checked(true);
			}
		}

		private void optStopped_Click(object sender, EventArgs e)
		{
			if (optStopped.get_Checked())
			{
				chkGraze.set_Checked(true);
			}
		}

		private void optFailed_Click(object sender, EventArgs e)
		{
			if (optFailed.get_Checked())
			{
				chkGraze.set_Checked(true);
			}
		}

		private void chkHighPrecision_CheckedChanged(object sender, EventArgs e)
		{
			int style = 0;
			if (optDMM.get_Checked())
			{
				style = 1;
			}
			if (optDDD.get_Checked())
			{
				style = 2;
			}
			UpdateLongitudes(style);
			UpdateLatitudes(style);
		}

		private void cmbMethod1_SelectedIndexChanged(object sender, EventArgs e)
		{
			string text = cmbMethod1.get_Items().get_Item(((ListControl)cmbMethod1).get_SelectedIndex()).ToString();
			if (text.Contains("Video") | text.Contains("Photoe") | text.Contains("Eye and"))
			{
				((ListControl)cmbPE).set_SelectedIndex(cmbPE.FindString("PE not required"));
			}
		}

		private void cmbMethod2_SelectedIndexChanged(object sender, EventArgs e)
		{
			string text = cmbMethod2.get_Items().get_Item(((ListControl)cmbMethod2).get_SelectedIndex()).ToString();
			if (text.Contains("Video") | text.Contains("Photoe") | text.Contains("Eye and"))
			{
				((ListControl)cmbPE).set_SelectedIndex(cmbPE.FindString("PE not required"));
			}
		}

		private void convertOldGrazeFilesToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void submitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Unknown result type (might be due to invalid IL or missing references)
			//IL_006a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0070: Invalid comparison between Unknown and I4
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b1: Invalid comparison between Unknown and I4
			//IL_00cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e0: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else if (LunarObservations.OccMain.Events.Count < 1)
			{
				MessageBox.Show("There are no events in the editor to report", "No events", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else if ((!((Settings.Default.FTP_AnonymousPassword.Length < 5) | (Settings.Default.EMailServerName.Length < 5)) || (int)((Form)new SetEmailAndServer()).ShowDialog() != 2) && (int)MessageBox.Show("There are " + LunarObservations.OccMain.Events.Count + " events in the editor to report\r\n\r\nDo you want to report these events?", "Confirm report", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				string text = Emails.Email_Observation_Report();
				if (text.Length > 5)
				{
					MessageBox.Show(text, "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)48);
				}
				else
				{
					MessageBox.Show("Report has been Emailed\r\n\r\nYou should receive a BCC copy in your email\r\n\r\nPlease do not send your observations to any other reporting address.\r\nThat will only create confusion when the observations are combined.", "Successful email", (MessageBoxButtons)0, (MessageBoxIcon)48);
				}
			}
		}

		private void downloadCurrentEmailAddressesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
			}
			else
			{
				Emails.GetCurrentAddresses();
			}
		}

		private void verifyDataEntryToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.VerifyReportData(ShowIfNoErrors: true);
			verifyDataEntryToolStripMenuItem.set_Checked(true);
		}

		private void identifyUnidentifiedStarsInReportToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.IdentifyXZstarsInReport();
			identifyUnidentifiedStarsInReportToolStripMenuItem.set_Checked(true);
		}

		private void ObservationsEditor_FormClosing(object sender, FormClosingEventArgs e)
		{
			if (UnsavedDataNeedsToBeSaved())
			{
				((CancelEventArgs)(object)e).Cancel = true;
			}
			Kepler2.XZinK2.Clear();
		}

		private void listPossibleDoubleStarsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.ListPossibledoubles();
			listPossibleDoubleStarsToolStripMenuItem.set_Checked(true);
		}

		private void verifyAccuracyValuesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.VerifyAccuracyValues();
			verifyAccuracyValuesToolStripMenuItem.set_Checked(true);
		}

		private void doAllToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.IdentifyXZstarsInReport();
			identifyUnidentifiedStarsInReportToolStripMenuItem.set_Checked(true);
			LunarObservations.ListPossibledoubles();
			listPossibleDoubleStarsToolStripMenuItem.set_Checked(true);
			LunarObservations.VerifyAccuracyValues();
			verifyAccuracyValuesToolStripMenuItem.set_Checked(true);
			LunarObservations.VerifyTemperatureValues();
			checkTemperatureValuesToolStripMenuItem.set_Checked(true);
			LunarObservations.VerifyReportData(ShowIfNoErrors: true);
			verifyDataEntryToolStripMenuItem.set_Checked(true);
		}

		private void checkTemperatureValuesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.VerifyTemperatureValues();
			checkTemperatureValuesToolStripMenuItem.set_Checked(true);
		}

		private void createReportForSelectedStarToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0cca: Unknown result type (might be due to invalid IL or missing references)
			double l = 0.0;
			double b = 0.0;
			double AA = 0.0;
			double num = 1.0;
			double Residual = 0.0;
			double PA = 0.0;
			double Residual2 = 0.0;
			double PA2 = 0.0;
			double MagStar = 0.0;
			double num2 = 0.0;
			string text = " ";
			string CA = "";
			bool pE_AdjustIfBrightStar = false;
			if (!double.TryParse(((Control)txtSecond).get_Text(), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtPE).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			if (!int.TryParse(((Control)txtXZ).get_Text(), out var result3))
			{
				result3 = 0;
			}
			else if (((ListControl)cmbPE).get_SelectedIndex() == 1)
			{
				num2 = result2;
			}
			else if (((ListControl)cmbPE).get_SelectedIndex() == 3)
			{
				num2 = ((!(optDisappear.get_Checked() | optBlink.get_Checked() | optFlash.get_Checked())) ? 0.99 : 0.48);
				pE_AdjustIfBrightStar = true;
			}
			if (((ListControl)cmbWDS).get_SelectedIndex() < 0)
			{
				((ListControl)cmbWDS).set_SelectedIndex(0);
			}
			double jD_atEvent = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value()) + ((double)(int)updnHour.get_Value() + (double)(int)updnMin.get_Value() / 60.0 + (result + num2 + 10.0) / 3600.0) / 24.0;
			double MoonRadius;
			double Illum;
			double StarAlt;
			if (((ListControl)cmbPlanets).get_SelectedIndex() > 0)
			{
				LunarObservations.Reduce_CurrentEntry_Librations(jD_atEvent, ((ListControl)cmbSites).get_SelectedIndex(), "P", 1000 * ((ListControl)cmbPlanets).get_SelectedIndex() + ((ListControl)cmbMoons).get_SelectedIndex(), pE_AdjustIfBrightStar, " ", ApplyLimbCorrection: false, out Residual, out PA, out MagStar, out MoonRadius, out l, out b, out AA, Include_LimbIllumAlt: true, out CA, out Illum, out StarAlt);
			}
			else
			{
				LunarObservations.Reduce_CurrentEntry_Librations(jD_atEvent, ((ListControl)cmbSites).get_SelectedIndex(), "X", result3, pE_AdjustIfBrightStar, cmbWDS.get_Items().get_Item(((ListControl)cmbWDS).get_SelectedIndex()).ToString(), ApplyLimbCorrection: false, out Residual, out PA, out MagStar, out MoonRadius, out l, out b, out AA, Include_LimbIllumAlt: true, out CA, out Illum, out StarAlt);
			}
			jD_atEvent = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value()) + ((double)(int)updnHour.get_Value() + (double)(int)updnMin.get_Value() / 60.0 + (result + num2) / 3600.0) / 24.0;
			if (((ListControl)cmbPlanets).get_SelectedIndex() > 0)
			{
				LunarObservations.Reduce_CurrentEntry_Librations(jD_atEvent, ((ListControl)cmbSites).get_SelectedIndex(), "P", 1000 * ((ListControl)cmbPlanets).get_SelectedIndex() + ((ListControl)cmbMoons).get_SelectedIndex(), pE_AdjustIfBrightStar, " ", ApplyLimbCorrection: false, out Residual2, out PA2, out MagStar, out MoonRadius, out l, out b, out AA, Include_LimbIllumAlt: true, out CA, out Illum, out StarAlt);
			}
			else
			{
				LunarObservations.Reduce_CurrentEntry_Librations(jD_atEvent, ((ListControl)cmbSites).get_SelectedIndex(), "X", result3, pE_AdjustIfBrightStar, cmbWDS.get_Items().get_Item(((ListControl)cmbWDS).get_SelectedIndex()).ToString(), ApplyLimbCorrection: false, out Residual2, out PA2, out MagStar, out MoonRadius, out l, out b, out AA, Include_LimbIllumAlt: true, out CA, out Illum, out StarAlt);
			}
			double num3 = (Residual - Residual2) / 10.0;
			double num4 = PA - PA2;
			if (num4 > 3.0)
			{
				num4 -= Math.PI * 2.0;
			}
			if (num4 < -3.0)
			{
				num4 += Math.PI * 2.0;
			}
			double num5 = Math.Atan2(Math.Abs(MoonRadius * (180.0 / Math.PI) * 3600.0 * num4 / 10.0), Math.Abs(num3)) * (180.0 / Math.PI);
			if (num3 > 0.0)
			{
				num5 = 180.0 - num5;
			}
			if (num4 > 0.0)
			{
				num5 = 0.0 - num5;
			}
			if (AA < 0.0)
			{
				AA += 360.0;
			}
			if (AA >= 360.0)
			{
				AA -= 360.0;
			}
			num = MoonRadius * (180.0 / Math.PI) * 3600.0 / 932.58;
			LunarObservations.Show_DoubleStarReport();
			if (((ListControl)cmbObservers).get_SelectedIndex() >= 0)
			{
				LunarObservations.DoubleStarReport.Observer = cmbObservers.get_Items().get_Item(((ListControl)cmbObservers).get_SelectedIndex()).ToString();
			}
			if (((ListControl)cmbSites).get_SelectedIndex() >= 0)
			{
				LunarObservations.DoubleStarReport.Site = cmbSites.get_Items().get_Item(((ListControl)cmbSites).get_SelectedIndex()).ToString();
			}
			((Control)LunarObservations.DoubleStarReport.txtMag1).set_Text(string.Format("{0,1:F2}", MagStar));
			GetEventPhase(out var Event, out var Limb);
			if (chkGraze.get_Checked())
			{
				text = "G";
			}
			((Control)LunarObservations.DoubleStarReport.lblEvent).set_Text(Event + " " + Limb + " " + text);
			((Control)LunarObservations.DoubleStarReport.txtPA).set_Text(string.Format("{0,1:F3}", PA2 * (180.0 / Math.PI)));
			((Control)LunarObservations.DoubleStarReport.txtAA).set_Text(string.Format("{0,1:F3}", AA));
			((Control)LunarObservations.DoubleStarReport.txtCA).set_Text(CA);
			((Control)LunarObservations.DoubleStarReport.txtIllum).set_Text(string.Format("{0,1:F0}%", Illum));
			((Control)LunarObservations.DoubleStarReport.txtL).set_Text(string.Format("{0,1:F3}", l));
			((Control)LunarObservations.DoubleStarReport.txtB).set_Text(string.Format("{0,1:F3}", b));
			((Control)LunarObservations.DoubleStarReport.txtMoonScale).set_Text(string.Format("{0,5:F3}", num));
			((Control)LunarObservations.DoubleStarReport.txtAlt).set_Text(string.Format("{0,1:F0}", StarAlt));
			((Control)LunarObservations.DoubleStarReport.txtRV).set_Text(string.Format("{0,1:F4}", Math.Abs(num3)));
			((Control)LunarObservations.DoubleStarReport.txtCCT).set_Text(string.Format("{0,1:F2}", num5));
			((Control)LunarObservations.DoubleStarReport.txtT1).set_Text(((Control)txtSecond).get_Text());
			((Control)LunarObservations.DoubleStarReport.lblDate).set_Text(updnYear.get_Value() + " " + Utilities.ShortMonths[(int)updnMonth.get_Value()] + string.Format("{0,3:F0}", updnDay.get_Value()));
			LunarObservations.DoubleStarReport.EventDateYYYYMMDD = updnYear.get_Value() + updnMonth.get_Value().ToString().PadLeft(2, '0') + updnDay.get_Value().ToString().PadLeft(2, '0');
			((Control)LunarObservations.DoubleStarReport.lblTime).set_Text("at " + updnHour.get_Value() + "h " + updnMin.get_Value() + "m");
			((Control)LunarObservations.DoubleStarReport.lblTime).set_Left(((Control)LunarObservations.DoubleStarReport.lblDate).get_Left() + ((Control)LunarObservations.DoubleStarReport.lblDate).get_Width());
			string text2 = "";
			if (((Control)txtZC).get_Text().Trim() != "0")
			{
				text2 = "R" + ((Control)txtZC).get_Text().Trim();
			}
			else if (((Control)txtSAO).get_Text().Trim() != "0")
			{
				text2 = "S" + ((Control)txtSAO).get_Text().Trim();
			}
			else if (((Control)txtXZ).get_Text().Trim() != "0")
			{
				text2 = "X" + ((Control)txtXZ).get_Text().Trim();
			}
			((Control)LunarObservations.DoubleStarReport.lblStar).set_Text(text2);
			string text3 = "";
			XZ80Q.Get_XZ_Star(result3);
			if (XZ80Q.DoubleFlag != " ")
			{
				DoubleStars.GetXZDoubleList(result3);
				for (int i = 0; i < DoubleStars.XZDoubleList.Count; i++)
				{
					text3 = text3 + "\r\n" + DoubleStars.XZDoubleList[i]!.ToString();
				}
				for (int j = 0; j < DoubleStars.XZDoubleList.Count; j++)
				{
					if (!(DoubleStars.XZDoubleList[j]!.ToString()!.Substring(0, 3) == "OCC"))
					{
						continue;
					}
					StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\xzdoubles discoveries.dat", Encoding.ASCII);
					int num6 = int.Parse(DoubleStars.XZDoubleList[j]!.ToString()!.Substring(3, 4));
					string text4;
					try
					{
						streamReader.BaseStream.Seek(151 * (num6 - 1), SeekOrigin.Begin);
						text4 = streamReader.ReadLine();
					}
					catch
					{
						text4 = "".PadRight(149);
					}
					streamReader.Close();
					string text5 = DoubleStars.XZDoubleList[j]!.ToString()!.Substring(0, 7);
					if (!int.TryParse(text4.Substring(29, 2), out var result4))
					{
						result4 = 0;
					}
					text3 = text3 + "\r\n" + text5 + " observed by " + text4.Substring(34, 24).Trim() + " on " + text4.Substring(25, 4) + " " + Utilities.ShortMonths[result4] + " " + text4.Substring(31, 2);
					StreamReader streamReader2 = new StreamReader(Utilities.AppPath + "\\Resource Files\\xzConfirmations.dat", Encoding.ASCII);
					do
					{
						string text6 = streamReader2.ReadLine();
						if (text6.Substring(0, 7) == text5)
						{
							if (!int.TryParse(text6.Substring(18, 2), out result4))
							{
								result4 = 0;
							}
							text3 = text3 + "\r\n        ........ by " + text6.Substring(24, 24).Trim() + " on " + text6.Substring(14, 4) + " " + Utilities.ShortMonths[result4] + " " + text6.Substring(20, 2);
						}
					}
					while (int.Parse(DoubleStars.XZDoubleList[j]!.ToString()!.Substring(3, 4)) >= num6 && !streamReader2.EndOfStream);
					streamReader2.Close();
				}
				if (DoubleStars.XZOrbitsList.Count > 0)
				{
					text3 += "\r\n\r\nPositions from orbit, on event date";
					for (int k = 0; k < DoubleStars.XZOrbitsList.Count; k++)
					{
						DoubleStars.XZOrbitsList[k].PAandSep(Utilities.BesselianYear((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value() + (double)updnHour.get_Value() / 24.0), out var PA3, out var Sep);
						text3 += string.Format("\r\nPA = {0,6:F2},  Separation  = {1,2:F3}\"", PA3, Sep);
					}
				}
			}
			LunarObservations.DoubleStarReport.DoubleStarDetails = text3;
			if (((Control)LunarObservations.DoubleStarReport.txtEmailAddress).get_Text() == "")
			{
				MessageBox.Show("The file of addresses needs to be updated before the Double Star report form can be completed.\r\n\r\nPlease go to the 'General Downloads' page under 'Maintenance', and download item #10 Reporting addresses for occultations", "No email address", (MessageBoxButtons)0, (MessageBoxIcon)16);
				((Form)LunarObservations.DoubleStarReport).Close();
				((Component)(object)LunarObservations.DoubleStarReport).Dispose();
			}
		}

		private void saveGraphicFromClipboardToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0078: Unknown result type (might be due to invalid IL or missing references)
			if (Clipboard.ContainsImage())
			{
				((Control)picLiMovie).set_Width(Clipboard.GetImage().Width);
				((Control)picLiMovie).set_Height(Clipboard.GetImage().Height);
				picLiMovie.set_Image(Clipboard.GetImage());
				Settings.Default.Save_LightCurve = Output.SaveGraphic(picLiMovie.get_Image(), "Image file name", Settings.Default.Save_LightCurve);
			}
			else
			{
				MessageBox.Show("No image on clipboard", "No image", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 5);
		}

		private void updnYear_MouseClick(object sender, MouseEventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 5);
		}

		private void updnMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMonth).Select(0, 2);
		}

		private void updnMonth_MouseClick(object sender, MouseEventArgs e)
		{
			((UpDownBase)updnMonth).Select(0, 2);
		}

		private void updnDay_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDay).Select(0, 2);
		}

		private void updnDay_MouseClick(object sender, MouseEventArgs e)
		{
			((UpDownBase)updnDay).Select(0, 2);
		}

		private void updnHour_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnHour).Select(0, 2);
		}

		private void updnHour_MouseClick(object sender, MouseEventArgs e)
		{
			((UpDownBase)updnHour).Select(0, 2);
		}

		private void updnMin_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMin).Select(0, 2);
		}

		private void updnMin_MouseClick(object sender, MouseEventArgs e)
		{
			((UpDownBase)updnMin).Select(0, 2);
		}

		private void txtLongDeg_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLongDeg).SelectAll();
		}

		private void txtLongDeg_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLongDeg).SelectAll();
		}

		private void txtLongMin_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLongMin).SelectAll();
		}

		private void txtLongMin_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLongMin).SelectAll();
		}

		private void txtLongSec_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLongSec).SelectAll();
		}

		private void txtLongSec_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLongSec).SelectAll();
		}

		private void txtLatDeg_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatDeg).SelectAll();
		}

		private void txtLatDeg_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLatDeg).SelectAll();
		}

		private void txtLatMin_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatMin).SelectAll();
		}

		private void txtLatMin_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLatMin).SelectAll();
		}

		private void txtLatSec_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatSec).SelectAll();
		}

		private void txtLatSec_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLatSec).SelectAll();
		}

		private void txtAlt_m_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtAlt_m).SelectAll();
		}

		private void txtAlt_m_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtAlt_m).SelectAll();
		}

		private void txtAlt_ft_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtAlt_ft).SelectAll();
		}

		private void txtAlt_ft_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtAlt_ft).SelectAll();
		}

		private void txtLongDmm_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLongDmm).SelectAll();
		}

		private void txtLongDmm_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLongDmm).SelectAll();
		}

		private void txtLongDDD_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLongDDD).SelectAll();
		}

		private void txtLongDDD_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLongDDD).SelectAll();
		}

		private void txtAperture_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtAperture).SelectAll();
		}

		private void txtAperture_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtAperture).SelectAll();
		}

		private void txtFocalLength_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtFocalLength).SelectAll();
		}

		private void txtFocalLength_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtFocalLength).SelectAll();
		}

		private void cmdGetDoubles_Click(object sender, EventArgs e)
		{
			Get_WDS_IF();
		}

		private void cmdGetWDS_Click(object sender, EventArgs e)
		{
			Get_WDS_IF();
		}

		private void Get_WDS_IF()
		{
			if (int.TryParse(((Control)txtXZ).get_Text(), out var result))
			{
				XZ80Q.Get_XZ_Star(result);
				Interferometric_Plus_WDS.Find_WDS_IF_Matches(XZ80Q.RA_rad * (180.0 / Math.PI) / 15.0, XZ80Q.Dec_rad * (180.0 / Math.PI), HighPrecision: false, ShowResults: true, out WDS_Interferometer_Display.VariableID);
			}
		}

		private void generalHelpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - Editor");
		}

		private void fresnelDiffractionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Fresnel diffraction - lunar occultations");
		}

		private void cameraCorrectionsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			LunarObservations.Show_CameraCorrections();
		}

		private void cmdCamera_Click(object sender, EventArgs e)
		{
			LunarObservations.Show_CameraCorrections();
		}

		private void txtDuration_Leave(object sender, EventArgs e)
		{
			//IL_0097: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			if (!(((Control)txtDuration).get_Text().Trim() == ""))
			{
				if (!double.TryParse(((Control)txtDuration).get_Text(), out var result))
				{
					result = 0.0;
				}
				if (result < 0.0)
				{
					text += "* Duration can't be a negative number\r\n";
				}
				if (Math.Abs(result) > 9.99)
				{
					text += "* Duration can't be greater than 9.999 secs\r\n";
				}
				if (text.Length != 0)
				{
					MessageBox.Show("Invalid data for Duration:\r\n" + text + "\r\nPlease reenter\r\n\r\nNOTE: Duration relates to fades, not to step events\r\n\r\nNOTE 2: For occultations of planets, specify a zero duration and report the time of the 2nd (or 3rd) contact", "Invalid duration", (MessageBoxButtons)0, (MessageBoxIcon)48);
					((Control)txtDuration).Focus();
				}
			}
		}

		private void createLightCurveReportToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateLightCurveReport();
		}

		private void CreateLightCurveReport()
		{
			//IL_00a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_040a: Unknown result type (might be due to invalid IL or missing references)
			int result = 0;
			double l = 0.0;
			double b = 0.0;
			double AA = 0.0;
			double num = 1.0;
			double Residual = 0.0;
			double PA = 0.0;
			double Residual2 = 0.0;
			double PA2 = 0.0;
			double MagStar = 0.0;
			double num2 = 0.0;
			string CA = "";
			bool pE_AdjustIfBrightStar = false;
			if (((Control)txtXZ).get_Text().Trim() == "")
			{
				MessageBox.Show("A light curve can only be submitted for an event involving an XZ star. An XZ star has not been selected.", "Must specify an XZ star", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			if (!double.TryParse(((Control)txtSecond).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			if (!double.TryParse(((Control)txtPE).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			if (!int.TryParse(((Control)txtXZ).get_Text(), out result))
			{
				result = 0;
			}
			else if (((ListControl)cmbPE).get_SelectedIndex() == 1)
			{
				num2 = result3;
			}
			else if (((ListControl)cmbPE).get_SelectedIndex() == 3)
			{
				num2 = ((!(optDisappear.get_Checked() | optBlink.get_Checked() | optFlash.get_Checked())) ? 0.99 : 0.48);
				pE_AdjustIfBrightStar = true;
			}
			if (((ListControl)cmbWDS).get_SelectedIndex() < 0)
			{
				((ListControl)cmbWDS).set_SelectedIndex(0);
			}
			double jD_atEvent = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value()) + ((double)(int)updnHour.get_Value() + (double)(int)updnMin.get_Value() / 60.0 + (result2 + num2 + 10.0) / 3600.0) / 24.0;
			double MoonRadius;
			double Illum;
			double StarAlt;
			if (((ListControl)cmbPlanets).get_SelectedIndex() > 0)
			{
				LunarObservations.Reduce_CurrentEntry_Librations(jD_atEvent, ((ListControl)cmbSites).get_SelectedIndex(), "P", 1000 * ((ListControl)cmbPlanets).get_SelectedIndex() + ((ListControl)cmbMoons).get_SelectedIndex(), pE_AdjustIfBrightStar, " ", ApplyLimbCorrection: false, out Residual, out PA, out MagStar, out MoonRadius, out l, out b, out AA, Include_LimbIllumAlt: true, out CA, out Illum, out StarAlt);
			}
			else
			{
				LunarObservations.Reduce_CurrentEntry_Librations(jD_atEvent, ((ListControl)cmbSites).get_SelectedIndex(), "X", result, pE_AdjustIfBrightStar, cmbWDS.get_Items().get_Item(((ListControl)cmbWDS).get_SelectedIndex()).ToString(), ApplyLimbCorrection: false, out Residual, out PA, out MagStar, out MoonRadius, out l, out b, out AA, Include_LimbIllumAlt: true, out CA, out Illum, out StarAlt);
			}
			jD_atEvent = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value()) + ((double)(int)updnHour.get_Value() + (double)(int)updnMin.get_Value() / 60.0 + (result2 + num2) / 3600.0) / 24.0;
			if (((ListControl)cmbPlanets).get_SelectedIndex() > 0)
			{
				LunarObservations.Reduce_CurrentEntry_Librations(jD_atEvent, ((ListControl)cmbSites).get_SelectedIndex(), "P", 1000 * ((ListControl)cmbPlanets).get_SelectedIndex() + ((ListControl)cmbMoons).get_SelectedIndex(), pE_AdjustIfBrightStar, " ", ApplyLimbCorrection: false, out Residual2, out PA2, out MagStar, out MoonRadius, out l, out b, out AA, Include_LimbIllumAlt: true, out CA, out Illum, out StarAlt);
			}
			else
			{
				LunarObservations.Reduce_CurrentEntry_Librations(jD_atEvent, ((ListControl)cmbSites).get_SelectedIndex(), "X", result, pE_AdjustIfBrightStar, cmbWDS.get_Items().get_Item(((ListControl)cmbWDS).get_SelectedIndex()).ToString(), ApplyLimbCorrection: false, out Residual2, out PA2, out MagStar, out MoonRadius, out l, out b, out AA, Include_LimbIllumAlt: true, out CA, out Illum, out StarAlt);
			}
			if (Math.Abs(Residual2) > 3.0)
			{
				MessageBox.Show("The residual for the selected event is greater than 3\", which indicates that one or more of the date/time, star number, or site location, is in error. Please review and correct before creating the light curve report.", "Bad residuals", (MessageBoxButtons)0, (MessageBoxIcon)16);
				Cursor.set_Current(Cursors.get_Default());
				return;
			}
			double num3 = (Residual - Residual2) / 10.0;
			double num4 = PA - PA2;
			if (num4 > 3.0)
			{
				num4 -= Math.PI * 2.0;
			}
			if (num4 < -3.0)
			{
				num4 += Math.PI * 2.0;
			}
			double num5 = Math.Atan2(Math.Abs(MoonRadius * (180.0 / Math.PI) * 3600.0 * num4 / 10.0), Math.Abs(num3)) * (180.0 / Math.PI);
			if (num3 > 0.0)
			{
				num5 = 180.0 - num5;
			}
			if (num4 > 0.0)
			{
				num5 = 0.0 - num5;
			}
			if (AA < 0.0)
			{
				AA += 360.0;
			}
			if (AA >= 360.0)
			{
				AA -= 360.0;
			}
			num = MoonRadius * (180.0 / Math.PI) * 3600.0 / 932.58;
			LightData.ShowLightCurveForm();
			AOTAData.StarCount = (AOTAData.StarCountToPlot = 0);
			LightData.LightCurveForm.SetAsLunar();
			LightData.LightCurveForm.LCD = new LightCurveData();
			LightData.LightCurveForm.LCD.SetLongitude_fromDeg = LunarObservations.OccMain.Telescopes[((ListControl)cmbSites).get_SelectedIndex()].Longitude * (180.0 / Math.PI);
			LightData.LightCurveForm.LCD.SetLatitude_fromDeg = LunarObservations.OccMain.Telescopes[((ListControl)cmbSites).get_SelectedIndex()].Latitude * (180.0 / Math.PI);
			LightData.LightCurveForm.LCD.AltM = (int)LunarObservations.OccMain.Telescopes[((ListControl)cmbSites).get_SelectedIndex()].Altitude;
			LightData.LightCurveForm.LCD.Observer = LunarObservations.OccMain.Observers[((ListControl)cmbObservers).get_SelectedIndex()].ObserverName;
			LightData.LightCurveForm.LCD.PosAngle = PA2 * (180.0 / Math.PI);
			LightData.LightCurveForm.LCD.AxisAngle = AA;
			LightData.LightCurveForm.LCD.CuspAngle = CA;
			LightData.LightCurveForm.LCD.Illumination = (int)Illum;
			LightData.LightCurveForm.LCD.Lib_L = l;
			LightData.LightCurveForm.LCD.Lib_B = b;
			LightData.LightCurveForm.LCD.MoonSize = num;
			LightData.LightCurveForm.LCD.MoonALt = (int)StarAlt;
			LightData.LightCurveForm.LCD.NormalMotion = Math.Abs(num3);
			LightData.LightCurveForm.LCD.ContactAngle = num5;
			LightData.LightCurveForm.LCD.Year = (int)updnYear.get_Value();
			LightData.LightCurveForm.LCD.Month = (int)updnMonth.get_Value();
			LightData.LightCurveForm.LCD.Day = (int)updnDay.get_Value();
			LightData.LightCurveForm.LCD.Hr = (int)updnHour.get_Value();
			LightData.LightCurveForm.LCD.Min = (int)updnMin.get_Value();
			LightData.LightCurveForm.LCD.Sec = 0.0;
			if (Utilities.LOLAFileExists)
			{
				LOLAHiRes.LimbHeight_Slope(AA, l, b, num, IncludeSlope: true, WideSlope: false, out var SlopeBefore_Deg, out var SlopeAfter_Deg, out var _, out var _);
				LightData.LightCurveForm.LCD.LimbSlope = (SlopeBefore_Deg + SlopeAfter_Deg) / 2.0;
			}
			else
			{
				LightData.LightCurveForm.LCD.LimbSlope = 0.0;
			}
			int Cadence = 0;
			LightData.LightCurveForm.LCD.SAO = int.Parse(((Control)txtSAO).get_Text());
			LightData.LightCurveForm.LCD.XZ = int.Parse(((Control)txtXZ).get_Text());
			LightCurveData lCD = LightData.LightCurveForm.LCD;
			LightCurveData lCD2 = LightData.LightCurveForm.LCD;
			short num7 = (LightData.LightCurveForm.LCD.Tyc2B = 0);
			short hipparcos = (lCD2.Tyc2A = num7);
			lCD.Hipparcos = hipparcos;
			Kepler2.GetKepler2InfoForXZ(XZ80Q.XZ, out var ID, out Cadence);
			LightData.LightCurveForm.LCD.Kepler2 = ID;
			if (Utilities.InternetIsAvailable())
			{
				XZ80Q.Get_XZ_Star(LightData.LightCurveForm.LCD.XZ);
				Vizier_Sesame.GetXZ80Equivalents("X", result.ToString());
				if (Vizier_Sesame.StarList != null)
				{
					for (int i = 0; i < Vizier_Sesame.StarList.lstStars.get_Items().get_Count(); i++)
					{
						string text = Vizier_Sesame.StarList.lstStars.get_Items().get_Item(i).ToString();
						if (text.Length >= 6)
						{
							if (text.Substring(0, 4) == "SAO ")
							{
								LightData.LightCurveForm.LCD.SAO = int.Parse(text.Substring(4));
							}
							if (text.Substring(0, 4) == "HIP ")
							{
								LightData.LightCurveForm.LCD.Hipparcos = int.Parse(text.Substring(4));
							}
							if (text.Substring(0, 4) == "TYC ")
							{
								LightData.LightCurveForm.LCD.SetTycho2 = text.Substring(4);
							}
						}
					}
					((Form)Vizier_Sesame.StarList).Close();
					((Component)(object)Vizier_Sesame.StarList).Dispose();
				}
			}
			if (((Control)txtZC).get_Text().Trim() != "0")
			{
				LightData.LightCurveForm.LCD.StarForFileName = "R" + ((Control)txtZC).get_Text().Trim();
			}
			else if (((Control)txtSAO).get_Text().Trim().Length > 3)
			{
				LightData.LightCurveForm.LCD.StarForFileName = "S" + ((Control)txtSAO).get_Text().Trim();
			}
			else if (result > 0)
			{
				LightData.LightCurveForm.LCD.StarForFileName = "X" + ((Control)txtXZ).get_Text().Trim();
			}
			LightData.LightCurveForm.SetTextBoxes();
			AOTAData.PlotLightCurve();
			Cursor.set_Current(Cursors.get_Default());
			LightData.LightCurveForm.GetPotentialDuplicateFiles();
			((Control)LightData.LightCurveForm).Focus();
		}

		private void emailLightCurveReportsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			http.Email_LightCurveReports();
		}

		private void lstEvents_MouseUp(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 2097152)
			{
				CreateLightCurveReport();
			}
		}

		private void txtComments_Leave(object sender, EventArgs e)
		{
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtComments).get_Text(), "Comments", CheckForPipes: false, out var RevisedText))
			{
				((Control)txtComments).set_Text(RevisedText);
				((Control)txtComments).Focus();
				((TextBoxBase)txtComments).set_SelectionStart(0);
			}
		}

		private void txtMessage_Leave(object sender, EventArgs e)
		{
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtMessage).get_Text(), "Special messages", CheckForPipes: false, out var RevisedText))
			{
				((Control)txtMessage).set_Text(RevisedText);
				((Control)txtMessage).Focus();
				((TextBoxBase)txtMessage).set_SelectionStart(0);
			}
		}

		private void txtAccuracy_Leave(object sender, EventArgs e)
		{
			//IL_00e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_010e: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			if (((Control)txtAccuracy).get_Text().Trim() == "")
			{
				return;
			}
			if (!double.TryParse(((Control)txtAccuracy).get_Text(), out var result))
			{
				result = 0.0;
			}
			bool num = (updnYear.get_Value() < 1751m) & (updnYear.get_Value() > 1600m);
			if (result < 0.0)
			{
				text = "* Accuracy can't be a negative number\r\n";
			}
			if (num)
			{
				if (Math.Abs(result) > 99.99)
				{
					text += "* For observations between 1600 and 1750, the Accuracy value can't be greater than 99.99 secs\r\n";
				}
			}
			else if (Math.Abs(result) > 9.99)
			{
				text += "* Accuracy value can't be greater than 9.999 secs\r\n";
			}
			else if (Math.Abs(result) > 1.0)
			{
				MessageBox.Show("An observation with an Accuracy worse than 1.0 secs is of little value. It should not be reported unless there are very special reasons.\r\n\r\nPlease consider removing this event.", "Large uncertainty", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			if (text.Length != 0)
			{
				MessageBox.Show("Invalid data for Accuracy:\r\n" + text + "\r\nPlease reenter\r\n\r\n", "Invalid accuracy", (MessageBoxButtons)0, (MessageBoxIcon)48);
				((Control)txtDuration).Focus();
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
			//IL_0657: Unknown result type (might be due to invalid IL or missing references)
			//IL_0661: Expected O, but got Unknown
			//IL_0662: Unknown result type (might be due to invalid IL or missing references)
			//IL_066c: Expected O, but got Unknown
			//IL_066d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0677: Expected O, but got Unknown
			//IL_0678: Unknown result type (might be due to invalid IL or missing references)
			//IL_0682: Expected O, but got Unknown
			//IL_0683: Unknown result type (might be due to invalid IL or missing references)
			//IL_068d: Expected O, but got Unknown
			//IL_068e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0698: Expected O, but got Unknown
			//IL_0699: Unknown result type (might be due to invalid IL or missing references)
			//IL_06a3: Expected O, but got Unknown
			//IL_06a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ae: Expected O, but got Unknown
			//IL_06af: Unknown result type (might be due to invalid IL or missing references)
			//IL_06b9: Expected O, but got Unknown
			//IL_06ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_06c4: Expected O, but got Unknown
			//IL_06c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_06cf: Expected O, but got Unknown
			//IL_06d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06da: Expected O, but got Unknown
			//IL_06db: Unknown result type (might be due to invalid IL or missing references)
			//IL_06e5: Expected O, but got Unknown
			//IL_06e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_06f0: Expected O, but got Unknown
			//IL_06f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_06fb: Expected O, but got Unknown
			//IL_06fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0706: Expected O, but got Unknown
			//IL_0707: Unknown result type (might be due to invalid IL or missing references)
			//IL_0711: Expected O, but got Unknown
			//IL_0712: Unknown result type (might be due to invalid IL or missing references)
			//IL_071c: Expected O, but got Unknown
			//IL_071d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0727: Expected O, but got Unknown
			//IL_0728: Unknown result type (might be due to invalid IL or missing references)
			//IL_0732: Expected O, but got Unknown
			//IL_0733: Unknown result type (might be due to invalid IL or missing references)
			//IL_073d: Expected O, but got Unknown
			//IL_073e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0748: Expected O, but got Unknown
			//IL_0749: Unknown result type (might be due to invalid IL or missing references)
			//IL_0753: Expected O, but got Unknown
			//IL_0754: Unknown result type (might be due to invalid IL or missing references)
			//IL_075e: Expected O, but got Unknown
			//IL_075f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0769: Expected O, but got Unknown
			//IL_076a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0774: Expected O, but got Unknown
			//IL_0775: Unknown result type (might be due to invalid IL or missing references)
			//IL_077f: Expected O, but got Unknown
			//IL_0780: Unknown result type (might be due to invalid IL or missing references)
			//IL_078a: Expected O, but got Unknown
			//IL_078b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0795: Expected O, but got Unknown
			//IL_0796: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a0: Expected O, but got Unknown
			//IL_07a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_07ab: Expected O, but got Unknown
			//IL_07ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b6: Expected O, but got Unknown
			//IL_07b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c1: Expected O, but got Unknown
			//IL_07c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_07cc: Expected O, but got Unknown
			//IL_07cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_07d7: Expected O, but got Unknown
			//IL_07d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_07e2: Expected O, but got Unknown
			//IL_07e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_07ed: Expected O, but got Unknown
			//IL_07ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_07f8: Expected O, but got Unknown
			//IL_07f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0803: Expected O, but got Unknown
			//IL_0804: Unknown result type (might be due to invalid IL or missing references)
			//IL_080e: Expected O, but got Unknown
			//IL_080f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0819: Expected O, but got Unknown
			//IL_081a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0824: Expected O, but got Unknown
			//IL_0825: Unknown result type (might be due to invalid IL or missing references)
			//IL_082f: Expected O, but got Unknown
			//IL_0830: Unknown result type (might be due to invalid IL or missing references)
			//IL_083a: Expected O, but got Unknown
			//IL_083b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0845: Expected O, but got Unknown
			//IL_0846: Unknown result type (might be due to invalid IL or missing references)
			//IL_0850: Expected O, but got Unknown
			//IL_0851: Unknown result type (might be due to invalid IL or missing references)
			//IL_085b: Expected O, but got Unknown
			//IL_085c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0866: Expected O, but got Unknown
			//IL_0867: Unknown result type (might be due to invalid IL or missing references)
			//IL_0871: Expected O, but got Unknown
			//IL_0872: Unknown result type (might be due to invalid IL or missing references)
			//IL_087c: Expected O, but got Unknown
			//IL_087d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0887: Expected O, but got Unknown
			//IL_0888: Unknown result type (might be due to invalid IL or missing references)
			//IL_0892: Expected O, but got Unknown
			//IL_0893: Unknown result type (might be due to invalid IL or missing references)
			//IL_089d: Expected O, but got Unknown
			//IL_089e: Unknown result type (might be due to invalid IL or missing references)
			//IL_08a8: Expected O, but got Unknown
			//IL_08a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_08b3: Expected O, but got Unknown
			//IL_08b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_08be: Expected O, but got Unknown
			//IL_08bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_08c9: Expected O, but got Unknown
			//IL_08ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_08d4: Expected O, but got Unknown
			//IL_08d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_08df: Expected O, but got Unknown
			//IL_08e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_08ea: Expected O, but got Unknown
			//IL_08eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_08f5: Expected O, but got Unknown
			//IL_08f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0900: Expected O, but got Unknown
			//IL_0901: Unknown result type (might be due to invalid IL or missing references)
			//IL_090b: Expected O, but got Unknown
			//IL_090c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0916: Expected O, but got Unknown
			//IL_0917: Unknown result type (might be due to invalid IL or missing references)
			//IL_0921: Expected O, but got Unknown
			//IL_0922: Unknown result type (might be due to invalid IL or missing references)
			//IL_092c: Expected O, but got Unknown
			//IL_092d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0937: Expected O, but got Unknown
			//IL_0938: Unknown result type (might be due to invalid IL or missing references)
			//IL_0942: Expected O, but got Unknown
			//IL_0943: Unknown result type (might be due to invalid IL or missing references)
			//IL_094d: Expected O, but got Unknown
			//IL_094e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0958: Expected O, but got Unknown
			//IL_0959: Unknown result type (might be due to invalid IL or missing references)
			//IL_0963: Expected O, but got Unknown
			//IL_0964: Unknown result type (might be due to invalid IL or missing references)
			//IL_096e: Expected O, but got Unknown
			//IL_096f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0979: Expected O, but got Unknown
			//IL_097a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0984: Expected O, but got Unknown
			//IL_0985: Unknown result type (might be due to invalid IL or missing references)
			//IL_098f: Expected O, but got Unknown
			//IL_0990: Unknown result type (might be due to invalid IL or missing references)
			//IL_099a: Expected O, but got Unknown
			//IL_099b: Unknown result type (might be due to invalid IL or missing references)
			//IL_09a5: Expected O, but got Unknown
			//IL_09a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_09b0: Expected O, but got Unknown
			//IL_09b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_09bb: Expected O, but got Unknown
			//IL_09bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_09c6: Expected O, but got Unknown
			//IL_09c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_09d1: Expected O, but got Unknown
			//IL_09d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_09dc: Expected O, but got Unknown
			//IL_09dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_09e7: Expected O, but got Unknown
			//IL_09e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_09f2: Expected O, but got Unknown
			//IL_09f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_09fd: Expected O, but got Unknown
			//IL_09fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a08: Expected O, but got Unknown
			//IL_0a09: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a13: Expected O, but got Unknown
			//IL_0a14: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a1e: Expected O, but got Unknown
			//IL_0a1f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a29: Expected O, but got Unknown
			//IL_0a2a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a34: Expected O, but got Unknown
			//IL_0a35: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a3f: Expected O, but got Unknown
			//IL_0a40: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a4a: Expected O, but got Unknown
			//IL_0a4b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a55: Expected O, but got Unknown
			//IL_0a56: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a60: Expected O, but got Unknown
			//IL_0a61: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a6b: Expected O, but got Unknown
			//IL_0a6c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a76: Expected O, but got Unknown
			//IL_0a77: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a81: Expected O, but got Unknown
			//IL_0a82: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a8c: Expected O, but got Unknown
			//IL_0a8d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a97: Expected O, but got Unknown
			//IL_0a98: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aa2: Expected O, but got Unknown
			//IL_0aa3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aad: Expected O, but got Unknown
			//IL_0aae: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ab8: Expected O, but got Unknown
			//IL_0ab9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ac3: Expected O, but got Unknown
			//IL_0ac4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ace: Expected O, but got Unknown
			//IL_0acf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ad9: Expected O, but got Unknown
			//IL_0ada: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ae4: Expected O, but got Unknown
			//IL_0ae5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aef: Expected O, but got Unknown
			//IL_0af0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0afa: Expected O, but got Unknown
			//IL_0afb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b05: Expected O, but got Unknown
			//IL_0b06: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b10: Expected O, but got Unknown
			//IL_0b11: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b1b: Expected O, but got Unknown
			//IL_0b1c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b26: Expected O, but got Unknown
			//IL_0b27: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b31: Expected O, but got Unknown
			//IL_0b32: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b3c: Expected O, but got Unknown
			//IL_0b3d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b47: Expected O, but got Unknown
			//IL_0b48: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b52: Expected O, but got Unknown
			//IL_0b53: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b5d: Expected O, but got Unknown
			//IL_0b5e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b68: Expected O, but got Unknown
			//IL_0b69: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b73: Expected O, but got Unknown
			//IL_0b74: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b7e: Expected O, but got Unknown
			//IL_0b7f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b89: Expected O, but got Unknown
			//IL_0b8a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b94: Expected O, but got Unknown
			//IL_0b95: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b9f: Expected O, but got Unknown
			//IL_0ba0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0baa: Expected O, but got Unknown
			//IL_0bab: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bb5: Expected O, but got Unknown
			//IL_0bb6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bc0: Expected O, but got Unknown
			//IL_0bc1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bcb: Expected O, but got Unknown
			//IL_0bcc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bd6: Expected O, but got Unknown
			//IL_0bd7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0be1: Expected O, but got Unknown
			//IL_0be2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bec: Expected O, but got Unknown
			//IL_0bed: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bf7: Expected O, but got Unknown
			//IL_0bf8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c02: Expected O, but got Unknown
			//IL_0c03: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c0d: Expected O, but got Unknown
			//IL_0c0e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c18: Expected O, but got Unknown
			//IL_0c19: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c23: Expected O, but got Unknown
			//IL_0c24: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c2e: Expected O, but got Unknown
			//IL_0c2f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c39: Expected O, but got Unknown
			//IL_0c3a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c44: Expected O, but got Unknown
			//IL_0c45: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c4f: Expected O, but got Unknown
			//IL_0c50: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c5a: Expected O, but got Unknown
			//IL_0c5b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c65: Expected O, but got Unknown
			//IL_0c66: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c70: Expected O, but got Unknown
			//IL_0c71: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c7b: Expected O, but got Unknown
			//IL_0c7c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c86: Expected O, but got Unknown
			//IL_0c87: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c91: Expected O, but got Unknown
			//IL_0c92: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c9c: Expected O, but got Unknown
			//IL_0c9d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ca7: Expected O, but got Unknown
			//IL_0ca8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cb2: Expected O, but got Unknown
			//IL_0cb3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cbd: Expected O, but got Unknown
			//IL_0cbe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cc8: Expected O, but got Unknown
			//IL_0cc9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cd3: Expected O, but got Unknown
			//IL_0cd4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cde: Expected O, but got Unknown
			//IL_0cdf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ce9: Expected O, but got Unknown
			//IL_0cea: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cf4: Expected O, but got Unknown
			//IL_0cf5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cff: Expected O, but got Unknown
			//IL_0d00: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d0a: Expected O, but got Unknown
			//IL_0d0b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d15: Expected O, but got Unknown
			//IL_0d16: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d20: Expected O, but got Unknown
			//IL_0d21: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d2b: Expected O, but got Unknown
			//IL_0d2c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d36: Expected O, but got Unknown
			//IL_0d37: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d41: Expected O, but got Unknown
			//IL_0d42: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d4c: Expected O, but got Unknown
			//IL_0d4d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d57: Expected O, but got Unknown
			//IL_0d58: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d62: Expected O, but got Unknown
			//IL_0d63: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d6d: Expected O, but got Unknown
			//IL_0d6e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d78: Expected O, but got Unknown
			//IL_0d79: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d83: Expected O, but got Unknown
			//IL_0d84: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d8e: Expected O, but got Unknown
			//IL_0d8f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d99: Expected O, but got Unknown
			//IL_0d9a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0da4: Expected O, but got Unknown
			//IL_0da5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0daf: Expected O, but got Unknown
			//IL_0db0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dba: Expected O, but got Unknown
			//IL_0dbb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dc5: Expected O, but got Unknown
			//IL_0dc6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dd0: Expected O, but got Unknown
			//IL_0dd1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ddb: Expected O, but got Unknown
			//IL_0de2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dec: Expected O, but got Unknown
			//IL_0ded: Unknown result type (might be due to invalid IL or missing references)
			//IL_0df7: Expected O, but got Unknown
			//IL_0df8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e02: Expected O, but got Unknown
			//IL_0e03: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e0d: Expected O, but got Unknown
			//IL_0e0e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e18: Expected O, but got Unknown
			//IL_1046: Unknown result type (might be due to invalid IL or missing references)
			//IL_1050: Expected O, but got Unknown
			//IL_1e1b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1e25: Expected O, but got Unknown
			//IL_2061: Unknown result type (might be due to invalid IL or missing references)
			//IL_206b: Expected O, but got Unknown
			//IL_2573: Unknown result type (might be due to invalid IL or missing references)
			//IL_257d: Expected O, but got Unknown
			//IL_25ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_25d8: Expected O, but got Unknown
			//IL_2921: Unknown result type (might be due to invalid IL or missing references)
			//IL_292b: Expected O, but got Unknown
			//IL_2bd5: Unknown result type (might be due to invalid IL or missing references)
			//IL_2bdf: Expected O, but got Unknown
			//IL_2d41: Unknown result type (might be due to invalid IL or missing references)
			//IL_2d4b: Expected O, but got Unknown
			//IL_2de7: Unknown result type (might be due to invalid IL or missing references)
			//IL_2df1: Expected O, but got Unknown
			//IL_2e8d: Unknown result type (might be due to invalid IL or missing references)
			//IL_2e97: Expected O, but got Unknown
			//IL_2f32: Unknown result type (might be due to invalid IL or missing references)
			//IL_2f3c: Expected O, but got Unknown
			//IL_2fd7: Unknown result type (might be due to invalid IL or missing references)
			//IL_2fe1: Expected O, but got Unknown
			//IL_307c: Unknown result type (might be due to invalid IL or missing references)
			//IL_3086: Expected O, but got Unknown
			//IL_3b06: Unknown result type (might be due to invalid IL or missing references)
			//IL_3b10: Expected O, but got Unknown
			//IL_3d87: Unknown result type (might be due to invalid IL or missing references)
			//IL_3d91: Expected O, but got Unknown
			//IL_42b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_42bf: Expected O, but got Unknown
			//IL_431c: Unknown result type (might be due to invalid IL or missing references)
			//IL_4326: Expected O, but got Unknown
			//IL_4eed: Unknown result type (might be due to invalid IL or missing references)
			//IL_4ef7: Expected O, but got Unknown
			//IL_61c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_61d3: Expected O, but got Unknown
			//IL_6656: Unknown result type (might be due to invalid IL or missing references)
			//IL_6660: Expected O, but got Unknown
			//IL_6762: Unknown result type (might be due to invalid IL or missing references)
			//IL_676c: Expected O, but got Unknown
			//IL_67f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_67fb: Expected O, but got Unknown
			//IL_6880: Unknown result type (might be due to invalid IL or missing references)
			//IL_688a: Expected O, but got Unknown
			//IL_77b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_77c1: Expected O, but got Unknown
			//IL_7ec4: Unknown result type (might be due to invalid IL or missing references)
			//IL_7ece: Expected O, but got Unknown
			//IL_7f3f: Unknown result type (might be due to invalid IL or missing references)
			//IL_7f49: Expected O, but got Unknown
			//IL_7fd1: Unknown result type (might be due to invalid IL or missing references)
			//IL_7fdb: Expected O, but got Unknown
			//IL_9eb7: Unknown result type (might be due to invalid IL or missing references)
			//IL_9ec1: Expected O, but got Unknown
			//IL_9fac: Unknown result type (might be due to invalid IL or missing references)
			//IL_9fb6: Expected O, but got Unknown
			//IL_a0a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_a0ab: Expected O, but got Unknown
			//IL_a163: Unknown result type (might be due to invalid IL or missing references)
			//IL_a16d: Expected O, but got Unknown
			//IL_a225: Unknown result type (might be due to invalid IL or missing references)
			//IL_a22f: Expected O, but got Unknown
			//IL_a315: Unknown result type (might be due to invalid IL or missing references)
			//IL_a31f: Expected O, but got Unknown
			//IL_d296: Unknown result type (might be due to invalid IL or missing references)
			//IL_d2a0: Expected O, but got Unknown
			//IL_d77e: Unknown result type (might be due to invalid IL or missing references)
			//IL_d788: Expected O, but got Unknown
			//IL_d7ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_d7f9: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(ObservationsEditor));
			lstEvents = new ListBox();
			lstSites = new ListBox();
			lstNames = new ListBox();
			grpSites = new GroupBox();
			grpSortSites = new GroupBox();
			cmdMoveSiteDown = new Button();
			cmdSortByLatitude = new Button();
			label64 = new Label();
			cmdSortByLongitude = new Button();
			cmdMoveSiteUp = new Button();
			cmdSortByNumber = new Button();
			cmdAddDefaultSite = new Button();
			groupBox3 = new GroupBox();
			label5 = new Label();
			cmbMount = new ComboBox();
			label4 = new Label();
			cmbDrive = new ComboBox();
			label3 = new Label();
			label1 = new Label();
			label2 = new Label();
			txtFocalLength = new TextBox();
			label20 = new Label();
			label16 = new Label();
			cmbTelescope = new ComboBox();
			txtAperture = new TextBox();
			cmdDefaultSite = new Button();
			cmdDeleteSites = new Button();
			cmdRenumberSites = new Button();
			cmdReplaceSites = new Button();
			cmdAddSites = new Button();
			grpILOCstations = new GroupBox();
			label7 = new Label();
			label6 = new Label();
			txtILOCTelescope = new TextBox();
			txtILOCStation = new TextBox();
			groupBox2 = new GroupBox();
			chkHighPrecision = new CheckBox();
			label26 = new Label();
			txtAlt_m = new TextBox();
			panelHeightDatum = new Panel();
			optWGS = new RadioButton();
			optMSL = new RadioButton();
			txtAlt_ft = new TextBox();
			panelDMS = new Panel();
			txtLongDeg = new TextBox();
			txtLatSec = new TextBox();
			txtLatMin = new TextBox();
			txtLatDeg = new TextBox();
			txtLongSec = new TextBox();
			txtLongMin = new TextBox();
			label19 = new Label();
			label18 = new Label();
			label15 = new Label();
			panel2 = new Panel();
			optFeet = new RadioButton();
			optMeters = new RadioButton();
			panel1 = new Panel();
			optDMM = new RadioButton();
			optDDD = new RadioButton();
			optDMS = new RadioButton();
			cmbDatum = new ComboBox();
			panelDMM = new Panel();
			txtLongDmm = new TextBox();
			txtLatMM = new TextBox();
			txtLatDmm = new TextBox();
			txtLongMM = new TextBox();
			panelDDD = new Panel();
			txtLongDDD = new TextBox();
			txtLatDDD = new TextBox();
			label17 = new Label();
			grpNames = new GroupBox();
			grpILOCnames = new GroupBox();
			label23 = new Label();
			label24 = new Label();
			txtILOCName = new TextBox();
			txtILOCStation_Name = new TextBox();
			label77 = new Label();
			txtObserverEmail = new TextBox();
			label70 = new Label();
			lblCharsLeftName = new Label();
			label69 = new Label();
			cmdRenumberNames = new Button();
			groupBox5 = new GroupBox();
			cmdMoveNameDown = new Button();
			cmdSortByFamilyName = new Button();
			label65 = new Label();
			cmdSortByName = new Button();
			cmdMoveNameUp = new Button();
			cmdSortByName_Number = new Button();
			cmdInsertDefaultName = new Button();
			cmdSetDefaultName = new Button();
			cmdDeleteName = new Button();
			cmdReplaceName = new Button();
			cmdAddName = new Button();
			label25 = new Label();
			txtName = new TextBox();
			grpEvents = new GroupBox();
			lblUnsavedEdits = new Label();
			picLiMovie = new PictureBox();
			grpEditControl = new GroupBox();
			optSortEventsByDate = new RadioButton();
			optByNumber = new RadioButton();
			cmdRenumberEvents = new Button();
			cmdDeleteEvent = new Button();
			cmdSortEvents = new Button();
			optByOBS = new RadioButton();
			optByTEL = new RadioButton();
			label60 = new Label();
			label55 = new Label();
			cmdReplaceEvent = new Button();
			label54 = new Label();
			label53 = new Label();
			cmdAddEvent = new Button();
			groupBox8 = new GroupBox();
			lblK2Star = new Label();
			cmdGetWDS = new Button();
			chkUnidentified = new CheckBox();
			grpPlanets = new GroupBox();
			label58 = new Label();
			label59 = new Label();
			label57 = new Label();
			cmbMoons = new ComboBox();
			txtAsteroid = new TextBox();
			cmbPlanets = new ComboBox();
			label71 = new Label();
			cmdIdentify = new Button();
			lblSAOValid = new Label();
			label52 = new Label();
			label51 = new Label();
			label50 = new Label();
			txtGSC = new TextBox();
			label49 = new Label();
			txtZC = new TextBox();
			txtSAO = new TextBox();
			txtXZ = new TextBox();
			label56 = new Label();
			lblIsDouble = new Label();
			grpMethods = new GroupBox();
			label72 = new Label();
			lblWDS = new Label();
			cmdGetDoubles = new Button();
			cmbTemp = new ComboBox();
			lblSep = new Label();
			lblPA = new Label();
			cmbWDS = new ComboBox();
			label67 = new Label();
			cmbLightLevel = new ComboBox();
			label66 = new Label();
			txtDuration = new TextBox();
			label48 = new Label();
			label47 = new Label();
			label46 = new Label();
			label45 = new Label();
			label44 = new Label();
			label43 = new Label();
			label42 = new Label();
			label41 = new Label();
			label40 = new Label();
			label39 = new Label();
			label38 = new Label();
			label37 = new Label();
			label36 = new Label();
			txtPE = new TextBox();
			txtSN = new TextBox();
			txtAccuracy = new TextBox();
			txtComments = new TextBox();
			cmbRemarkable = new ComboBox();
			cmbDoubles = new ComboBox();
			cmbStability = new ComboBox();
			cmbTransparency = new ComboBox();
			cmbCertainty = new ComboBox();
			cmbPE = new ComboBox();
			cmbTimeKeeping = new ComboBox();
			cmbMethod2 = new ComboBox();
			cmbMethod1 = new ComboBox();
			grpObserver = new GroupBox();
			label63 = new Label();
			txtRecorder = new Label();
			label61 = new Label();
			grpILOCevents = new GroupBox();
			txtILOCEventObserver = new TextBox();
			txtILOCEventRecorder = new TextBox();
			txtILOCEventTelescope = new TextBox();
			txtILOCEventStation = new TextBox();
			cmbObservers = new ComboBox();
			cmbRecorders = new ComboBox();
			cmbSites = new ComboBox();
			groupBox6 = new GroupBox();
			cmdCamera = new Button();
			panel4 = new Panel();
			optUmbra = new RadioButton();
			optBrightLimb = new RadioButton();
			optDarkLimb = new RadioButton();
			label28 = new Label();
			label35 = new Label();
			label34 = new Label();
			label33 = new Label();
			label32 = new Label();
			label31 = new Label();
			label30 = new Label();
			label29 = new Label();
			cmdToday = new Button();
			chkGraze = new CheckBox();
			optReappear = new RadioButton();
			optFailed = new RadioButton();
			optStarted = new RadioButton();
			optMiss = new RadioButton();
			optStopped = new RadioButton();
			optFlash = new RadioButton();
			optBlink = new RadioButton();
			optDisappear = new RadioButton();
			label27 = new Label();
			txtSecond = new TextBox();
			updnMonth = new NumericUpDown();
			updnDay = new NumericUpDown();
			updnHour = new NumericUpDown();
			updnMin = new NumericUpDown();
			updnYear = new NumericUpDown();
			txtResidual = new TextBox();
			txtPA = new TextBox();
			txtMag = new TextBox();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			newToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem2 = new ToolStripSeparator();
			openToolStripMenuItem = new ToolStripMenuItem();
			importToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem1 = new ToolStripSeparator();
			pasteToolStripMenuItem = new ToolStripMenuItem();
			pasteMergeToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyReportToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveAsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			cameraCorrectionsToolStripMenuItem = new ToolStripMenuItem();
			doAllToolStripMenuItem = new ToolStripMenuItem();
			identifyUnidentifiedStarsInReportToolStripMenuItem = new ToolStripMenuItem();
			listPossibleDoubleStarsToolStripMenuItem = new ToolStripMenuItem();
			verifyAccuracyValuesToolStripMenuItem = new ToolStripMenuItem();
			checkTemperatureValuesToolStripMenuItem = new ToolStripMenuItem();
			verifyDataEntryToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			saveoldFormatToolStripMenuItem = new ToolStripMenuItem();
			saveAsoldFormatToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			addGrazeToRecentGrazesFileToolStripMenuItem = new ToolStripMenuItem();
			addGrazeToMainFileOfGrazesToolStripMenuItem = new ToolStripMenuItem();
			withReportToolStripMenuItem = new ToolStripMenuItem();
			submitToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			downloadCurrentEmailAddressesToolStripMenuItem = new ToolStripMenuItem();
			doubleStarReportToolStripMenuItem = new ToolStripMenuItem();
			createReportForSelectedStarToolStripMenuItem = new ToolStripMenuItem();
			selectTheBrighterComponentToolStripMenuItem = new ToolStripMenuItem();
			lightCurveReportToolStripMenuItem = new ToolStripMenuItem();
			createLightCurveReportToolStripMenuItem = new ToolStripMenuItem();
			emailLightCurveReportsToolStripMenuItem = new ToolStripMenuItem();
			displayOnGoogleEarthToolStripMenuItem = new ToolStripMenuItem();
			displayCurrentSitesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator6 = new ToolStripSeparator();
			plotMoonFirst10EventsToolStripMenuItem = new ToolStripMenuItem();
			plotMoonFirst20EventsToolStripMenuItem = new ToolStripMenuItem();
			plotMoonFirst50EventsToolStripMenuItem = new ToolStripMenuItem();
			plotMoonAllEventsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator7 = new ToolStripSeparator();
			includeLimbProfile = new ToolStripMenuItem();
			altitudemToUseInLimbPlotsToolStripMenuItem = new ToolStripMenuItem();
			mnuGoogleHeight = new ToolStripTextBox();
			toolStripSeparator8 = new ToolStripSeparator();
			liMovieToolStripMenuItem = new ToolStripMenuItem();
			saveGraphicFromClipboardToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			generalHelpToolStripMenuItem = new ToolStripMenuItem();
			fresnelDiffractionToolStripMenuItem = new ToolStripMenuItem();
			panel3 = new Panel();
			lblLimbBasis = new Label();
			chkFormat = new CheckBox();
			cmdReduce = new Button();
			label10 = new Label();
			optView = new RadioButton();
			optHeader = new RadioButton();
			label8 = new Label();
			optEvents = new RadioButton();
			optSites = new RadioButton();
			grpHeader = new GroupBox();
			label76 = new Label();
			label73 = new Label();
			txtMessage = new TextBox();
			label75 = new Label();
			label74 = new Label();
			label62 = new Label();
			lblCharsLeft = new Label();
			label68 = new Label();
			panelRepresentative = new Panel();
			cmdGetRep = new Button();
			cmdSetRep = new Button();
			txtRepresentative = new TextBox();
			label12 = new Label();
			panelReportedTo = new Panel();
			cmdGetReported = new Button();
			cmdSetReported = new Button();
			txtReported = new TextBox();
			label11 = new Label();
			panelAddress = new Panel();
			cmdGetAddress = new Button();
			cmdSetAddress = new Button();
			txtAddress = new TextBox();
			label14 = new Label();
			label22 = new Label();
			label21 = new Label();
			cmdSetPlace = new Button();
			cmdGetEMail = new Button();
			cmdSetEmail = new Button();
			cmdGetPlace = new Button();
			txtEmail = new TextBox();
			txtPlace = new TextBox();
			label13 = new Label();
			label9 = new Label();
			lstReport = new ListBox();
			toolTip = new ToolTip(components);
			label78 = new Label();
			label79 = new Label();
			label80 = new Label();
			label81 = new Label();
			((Control)grpSites).SuspendLayout();
			((Control)grpSortSites).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)grpILOCstations).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)panelHeightDatum).SuspendLayout();
			((Control)panelDMS).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)panelDMM).SuspendLayout();
			((Control)panelDDD).SuspendLayout();
			((Control)grpNames).SuspendLayout();
			((Control)grpILOCnames).SuspendLayout();
			((Control)groupBox5).SuspendLayout();
			((Control)grpEvents).SuspendLayout();
			((ISupportInitialize)picLiMovie).BeginInit();
			((Control)grpEditControl).SuspendLayout();
			((Control)groupBox8).SuspendLayout();
			((Control)grpPlanets).SuspendLayout();
			((Control)grpMethods).SuspendLayout();
			((Control)grpObserver).SuspendLayout();
			((Control)grpILOCevents).SuspendLayout();
			((Control)groupBox6).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnHour).BeginInit();
			((ISupportInitialize)updnMin).BeginInit();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)grpHeader).SuspendLayout();
			((Control)panelRepresentative).SuspendLayout();
			((Control)panelReportedTo).SuspendLayout();
			((Control)panelAddress).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstEvents).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEvents).set_FormattingEnabled(true);
			lstEvents.set_ItemHeight(14);
			((Control)lstEvents).set_Location(new Point(6, 357));
			((Control)lstEvents).set_Name("lstEvents");
			((Control)lstEvents).set_Size(new Size(990, 228));
			((Control)lstEvents).set_TabIndex(13);
			lstEvents.add_SelectedIndexChanged((EventHandler)lstEvents_SelectedIndexChanged);
			((Control)lstEvents).add_MouseUp(new MouseEventHandler(lstEvents_MouseUp));
			((Control)lstSites).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstSites).set_FormattingEnabled(true);
			lstSites.set_ItemHeight(14);
			((Control)lstSites).set_Location(new Point(6, 352));
			((Control)lstSites).set_Name("lstSites");
			((Control)lstSites).set_Size(new Size(600, 228));
			((Control)lstSites).set_TabIndex(10);
			lstSites.add_SelectedIndexChanged((EventHandler)lstSites_SelectedIndexChanged);
			((Control)lstNames).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstNames).set_FormattingEnabled(true);
			lstNames.set_HorizontalExtent(550);
			lstNames.set_HorizontalScrollbar(true);
			lstNames.set_ItemHeight(14);
			((Control)lstNames).set_Location(new Point(8, 352));
			((Control)lstNames).set_Name("lstNames");
			((Control)lstNames).set_Size(new Size(360, 228));
			((Control)lstNames).set_TabIndex(15);
			lstNames.add_SelectedIndexChanged((EventHandler)lstNames_SelectedIndexChanged);
			((Control)grpSites).set_Anchor((AnchorStyles)1);
			((Control)grpSites).get_Controls().Add((Control)(object)grpSortSites);
			((Control)grpSites).get_Controls().Add((Control)(object)cmdAddDefaultSite);
			((Control)grpSites).get_Controls().Add((Control)(object)groupBox3);
			((Control)grpSites).get_Controls().Add((Control)(object)cmdDefaultSite);
			((Control)grpSites).get_Controls().Add((Control)(object)cmdDeleteSites);
			((Control)grpSites).get_Controls().Add((Control)(object)cmdRenumberSites);
			((Control)grpSites).get_Controls().Add((Control)(object)cmdReplaceSites);
			((Control)grpSites).get_Controls().Add((Control)(object)cmdAddSites);
			((Control)grpSites).get_Controls().Add((Control)(object)lstSites);
			((Control)grpSites).get_Controls().Add((Control)(object)grpILOCstations);
			((Control)grpSites).get_Controls().Add((Control)(object)groupBox2);
			((Control)grpSites).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpSites).set_Location(new Point(10, 69));
			((Control)grpSites).set_Name("grpSites");
			((Control)grpSites).set_Size(new Size(612, 582));
			((Control)grpSites).set_TabIndex(2);
			grpSites.set_TabStop(false);
			((Control)grpSites).set_Text("Sites / Telescopes");
			((Control)grpSortSites).get_Controls().Add((Control)(object)cmdMoveSiteDown);
			((Control)grpSortSites).get_Controls().Add((Control)(object)cmdSortByLatitude);
			((Control)grpSortSites).get_Controls().Add((Control)(object)label64);
			((Control)grpSortSites).get_Controls().Add((Control)(object)cmdSortByLongitude);
			((Control)grpSortSites).get_Controls().Add((Control)(object)cmdMoveSiteUp);
			((Control)grpSortSites).get_Controls().Add((Control)(object)cmdSortByNumber);
			((Control)grpSortSites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpSortSites).set_Location(new Point(375, 239));
			((Control)grpSortSites).set_Name("grpSortSites");
			((Control)grpSortSites).set_Size(new Size(223, 101));
			((Control)grpSortSites).set_TabIndex(9);
			grpSortSites.set_TabStop(false);
			((Control)grpSortSites).set_Text("Sort / move   Sites");
			((Control)cmdMoveSiteDown).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMoveSiteDown).set_Location(new Point(171, 58));
			((Control)cmdMoveSiteDown).set_Name("cmdMoveSiteDown");
			((Control)cmdMoveSiteDown).set_Size(new Size(42, 21));
			((Control)cmdMoveSiteDown).set_TabIndex(5);
			((Control)cmdMoveSiteDown).set_Text("down");
			((ButtonBase)cmdMoveSiteDown).set_UseVisualStyleBackColor(true);
			((Control)cmdMoveSiteDown).add_Click((EventHandler)cmdMoveSiteDown_Click);
			((Control)cmdSortByLatitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSortByLatitude).set_Location(new Point(8, 74));
			((Control)cmdSortByLatitude).set_Name("cmdSortByLatitude");
			((Control)cmdSortByLatitude).set_Size(new Size(87, 21));
			((Control)cmdSortByLatitude).set_TabIndex(2);
			((Control)cmdSortByLatitude).set_Text("by Latitude");
			((ButtonBase)cmdSortByLatitude).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdSortByLatitude).set_UseVisualStyleBackColor(true);
			((Control)cmdSortByLatitude).add_Click((EventHandler)cmdSortByLatitude_Click);
			((Control)label64).set_AutoSize(true);
			((Control)label64).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label64).set_Location(new Point(116, 49));
			((Control)label64).set_Name("label64");
			((Control)label64).set_Size(new Size(56, 13));
			((Control)label64).set_TabIndex(3);
			((Control)label64).set_Text("Move site:");
			((Control)cmdSortByLongitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSortByLongitude).set_Location(new Point(8, 45));
			((Control)cmdSortByLongitude).set_Name("cmdSortByLongitude");
			((Control)cmdSortByLongitude).set_Size(new Size(87, 21));
			((Control)cmdSortByLongitude).set_TabIndex(1);
			((Control)cmdSortByLongitude).set_Text("by Longitude");
			((ButtonBase)cmdSortByLongitude).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdSortByLongitude).set_UseVisualStyleBackColor(true);
			((Control)cmdSortByLongitude).add_Click((EventHandler)cmdSortByLongitude_Click);
			((Control)cmdMoveSiteUp).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMoveSiteUp).set_Location(new Point(171, 37));
			((Control)cmdMoveSiteUp).set_Name("cmdMoveSiteUp");
			((Control)cmdMoveSiteUp).set_Size(new Size(42, 21));
			((Control)cmdMoveSiteUp).set_TabIndex(4);
			((Control)cmdMoveSiteUp).set_Text("up");
			((ButtonBase)cmdMoveSiteUp).set_UseVisualStyleBackColor(true);
			((Control)cmdMoveSiteUp).add_Click((EventHandler)cmdMoveSiteUp_Click);
			((Control)cmdSortByNumber).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSortByNumber).set_Location(new Point(8, 16));
			((Control)cmdSortByNumber).set_Name("cmdSortByNumber");
			((Control)cmdSortByNumber).set_Size(new Size(87, 21));
			((Control)cmdSortByNumber).set_TabIndex(0);
			((Control)cmdSortByNumber).set_Text("by Number");
			((ButtonBase)cmdSortByNumber).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdSortByNumber).set_UseVisualStyleBackColor(true);
			((Control)cmdSortByNumber).add_Click((EventHandler)cmdSortByNumber_Click);
			((Control)cmdAddDefaultSite).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAddDefaultSite).set_Location(new Point(139, 300));
			((Control)cmdAddDefaultSite).set_Name("cmdAddDefaultSite");
			((Control)cmdAddDefaultSite).set_Size(new Size(78, 35));
			((Control)cmdAddDefaultSite).set_TabIndex(6);
			((Control)cmdAddDefaultSite).set_Text("Add default\r\nas new site");
			((ButtonBase)cmdAddDefaultSite).set_UseVisualStyleBackColor(true);
			((Control)cmdAddDefaultSite).add_Click((EventHandler)cmdAddDefaultSite_Click);
			((Control)groupBox3).get_Controls().Add((Control)(object)label5);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmbMount);
			((Control)groupBox3).get_Controls().Add((Control)(object)label4);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmbDrive);
			((Control)groupBox3).get_Controls().Add((Control)(object)label3);
			((Control)groupBox3).get_Controls().Add((Control)(object)label1);
			((Control)groupBox3).get_Controls().Add((Control)(object)label2);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtFocalLength);
			((Control)groupBox3).get_Controls().Add((Control)(object)label20);
			((Control)groupBox3).get_Controls().Add((Control)(object)label16);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmbTelescope);
			((Control)groupBox3).get_Controls().Add((Control)(object)txtAperture);
			((Control)groupBox3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)groupBox3).set_Location(new Point(353, 21));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(226, 158));
			((Control)groupBox3).set_TabIndex(1);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Telescope");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(16, 101));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(59, 13));
			((Control)label5).set_TabIndex(8);
			((Control)label5).set_Text("Mounting");
			cmbMount.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbMount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbMount).set_FormattingEnabled(true);
			cmbMount.get_Items().AddRange(new object[3] { "", "Equatorial", "Altazimuth" });
			((Control)cmbMount).set_Location(new Point(77, 97));
			((Control)cmbMount).set_Name("cmbMount");
			((Control)cmbMount).set_Size(new Size(131, 21));
			((Control)cmbMount).set_TabIndex(9);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(38, 128));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(37, 13));
			((Control)label4).set_TabIndex(10);
			((Control)label4).set_Text("Drive");
			cmbDrive.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbDrive).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbDrive).set_FormattingEnabled(true);
			cmbDrive.get_Items().AddRange(new object[3] { "", "Clock Driven", "Manual" });
			((Control)cmbDrive).set_Location(new Point(77, 124));
			((Control)cmbDrive).set_Name("cmbDrive");
			((Control)cmbDrive).set_Size(new Size(131, 21));
			((Control)cmbDrive).set_TabIndex(11);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(32, 72));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(43, 13));
			((Control)label3).set_TabIndex(6);
			((Control)label3).set_Text("Optics");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(147, 46));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(23, 13));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("cm");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(23, 46));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(77, 13));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("Focal length");
			((Control)txtFocalLength).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFocalLength).set_Location(new Point(102, 42));
			((Control)txtFocalLength).set_Name("txtFocalLength");
			((Control)txtFocalLength).set_Size(new Size(43, 20));
			((Control)txtFocalLength).set_TabIndex(4);
			((TextBoxBase)txtFocalLength).add_MouseClick(new MouseEventHandler(txtFocalLength_MouseClick));
			((Control)txtFocalLength).add_Enter((EventHandler)txtFocalLength_Enter);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(143, 20));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(23, 13));
			((Control)label20).set_TabIndex(2);
			((Control)label20).set_Text("cm");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(43, 20));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(55, 13));
			((Control)label16).set_TabIndex(0);
			((Control)label16).set_Text("Aperture");
			cmbTelescope.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbTelescope).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbTelescope).set_FormattingEnabled(true);
			cmbTelescope.get_Items().AddRange(new object[5] { "", "Refractor", "Newtonian reflector", "Cassegrain or Schmidt", "Other " });
			((Control)cmbTelescope).set_Location(new Point(77, 68));
			((Control)cmbTelescope).set_Name("cmbTelescope");
			((Control)cmbTelescope).set_Size(new Size(131, 21));
			((Control)cmbTelescope).set_TabIndex(7);
			((Control)txtAperture).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAperture).set_Location(new Point(102, 16));
			((Control)txtAperture).set_Name("txtAperture");
			((Control)txtAperture).set_Size(new Size(38, 20));
			((Control)txtAperture).set_TabIndex(1);
			((TextBoxBase)txtAperture).add_MouseClick(new MouseEventHandler(txtAperture_MouseClick));
			((Control)txtAperture).add_Enter((EventHandler)txtAperture_Enter);
			((Control)cmdDefaultSite).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDefaultSite).set_Location(new Point(139, 261));
			((Control)cmdDefaultSite).set_Name("cmdDefaultSite");
			((Control)cmdDefaultSite).set_Size(new Size(78, 35));
			((Control)cmdDefaultSite).set_TabIndex(5);
			((Control)cmdDefaultSite).set_Text("Set selected as default");
			((ButtonBase)cmdDefaultSite).set_UseVisualStyleBackColor(true);
			((Control)cmdDefaultSite).add_Click((EventHandler)cmdDefaultSite_Click);
			((Control)cmdDeleteSites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDeleteSites).set_Location(new Point(262, 261));
			((Control)cmdDeleteSites).set_Name("cmdDeleteSites");
			((Control)cmdDeleteSites).set_Size(new Size(78, 35));
			((Control)cmdDeleteSites).set_TabIndex(7);
			((Control)cmdDeleteSites).set_Text("Delete\r\nselected");
			((ButtonBase)cmdDeleteSites).set_UseVisualStyleBackColor(true);
			((Control)cmdDeleteSites).add_Click((EventHandler)cmdDeleteSites_Click);
			((Control)cmdRenumberSites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRenumberSites).set_Location(new Point(262, 300));
			((Control)cmdRenumberSites).set_Name("cmdRenumberSites");
			((Control)cmdRenumberSites).set_Size(new Size(78, 35));
			((Control)cmdRenumberSites).set_TabIndex(8);
			((Control)cmdRenumberSites).set_Text("ReNumber");
			((ButtonBase)cmdRenumberSites).set_UseVisualStyleBackColor(true);
			((Control)cmdRenumberSites).add_Click((EventHandler)cmdRenumberSites_Click);
			((Control)cmdReplaceSites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdReplaceSites).set_Location(new Point(16, 300));
			((Control)cmdReplaceSites).set_Name("cmdReplaceSites");
			((Control)cmdReplaceSites).set_Size(new Size(78, 35));
			((Control)cmdReplaceSites).set_TabIndex(4);
			((Control)cmdReplaceSites).set_Text("Replace\r\nselected");
			((ButtonBase)cmdReplaceSites).set_UseVisualStyleBackColor(true);
			((Control)cmdReplaceSites).add_Click((EventHandler)cmdReplaceSites_Click);
			((Control)cmdAddSites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAddSites).set_Location(new Point(16, 261));
			((Control)cmdAddSites).set_Name("cmdAddSites");
			((Control)cmdAddSites).set_Size(new Size(78, 35));
			((Control)cmdAddSites).set_TabIndex(3);
			((Control)cmdAddSites).set_Text("Add as \r\nnew site");
			((ButtonBase)cmdAddSites).set_UseVisualStyleBackColor(true);
			((Control)cmdAddSites).add_Click((EventHandler)cmdAddSites_Click);
			((Control)grpILOCstations).get_Controls().Add((Control)(object)label7);
			((Control)grpILOCstations).get_Controls().Add((Control)(object)label6);
			((Control)grpILOCstations).get_Controls().Add((Control)(object)txtILOCTelescope);
			((Control)grpILOCstations).get_Controls().Add((Control)(object)txtILOCStation);
			((Control)grpILOCstations).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)grpILOCstations).set_Location(new Point(339, 185));
			((Control)grpILOCstations).set_Name("grpILOCstations");
			((Control)grpILOCstations).set_Size(new Size(255, 48));
			((Control)grpILOCstations).set_TabIndex(2);
			grpILOCstations.set_TabStop(false);
			((Control)grpILOCstations).set_Text("ILOC Station code  (if known)");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(131, 22));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(57, 13));
			((Control)label7).set_TabIndex(2);
			((Control)label7).set_Text("Telescope");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(9, 22));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(40, 13));
			((Control)label6).set_TabIndex(0);
			((Control)label6).set_Text("Station");
			((Control)txtILOCTelescope).set_Location(new Point(190, 18));
			((Control)txtILOCTelescope).set_Name("txtILOCTelescope");
			((Control)txtILOCTelescope).set_Size(new Size(42, 20));
			((Control)txtILOCTelescope).set_TabIndex(3);
			((Control)txtILOCTelescope).add_MouseUp(new MouseEventHandler(txtILOCTelescope_MouseUp));
			((Control)txtILOCStation).set_Location(new Point(51, 19));
			((Control)txtILOCStation).set_Name("txtILOCStation");
			((Control)txtILOCStation).set_Size(new Size(61, 20));
			((Control)txtILOCStation).set_TabIndex(1);
			((Control)txtILOCStation).add_MouseUp(new MouseEventHandler(txtILOCStation_MouseUp));
			((Control)groupBox2).get_Controls().Add((Control)(object)chkHighPrecision);
			((Control)groupBox2).get_Controls().Add((Control)(object)label26);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtAlt_m);
			((Control)groupBox2).get_Controls().Add((Control)(object)panelHeightDatum);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtAlt_ft);
			((Control)groupBox2).get_Controls().Add((Control)(object)panelDMS);
			((Control)groupBox2).get_Controls().Add((Control)(object)label19);
			((Control)groupBox2).get_Controls().Add((Control)(object)label18);
			((Control)groupBox2).get_Controls().Add((Control)(object)label15);
			((Control)groupBox2).get_Controls().Add((Control)(object)panel2);
			((Control)groupBox2).get_Controls().Add((Control)(object)panel1);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmbDatum);
			((Control)groupBox2).get_Controls().Add((Control)(object)panelDMM);
			((Control)groupBox2).get_Controls().Add((Control)(object)panelDDD);
			((Control)groupBox2).get_Controls().Add((Control)(object)label17);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(33, 21));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(272, 209));
			((Control)groupBox2).set_TabIndex(0);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Location");
			((Control)chkHighPrecision).set_AutoSize(true);
			((Control)chkHighPrecision).set_Location(new Point(9, 20));
			((Control)chkHighPrecision).set_Name("chkHighPrecision");
			((Control)chkHighPrecision).set_Size(new Size(223, 17));
			((Control)chkHighPrecision).set_TabIndex(40);
			((Control)chkHighPrecision).set_Text("Coordinates are high-precision (surveyed) ");
			((ButtonBase)chkHighPrecision).set_UseVisualStyleBackColor(true);
			chkHighPrecision.add_CheckedChanged((EventHandler)chkHighPrecision_CheckedChanged);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(182, 142));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(88, 13));
			((Control)label26).set_TabIndex(11);
			((Control)label26).set_Text("Altitude datum");
			((Control)txtAlt_m).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt_m).set_Location(new Point(71, 155));
			((Control)txtAlt_m).set_Name("txtAlt_m");
			((Control)txtAlt_m).set_Size(new Size(53, 20));
			((Control)txtAlt_m).set_TabIndex(8);
			((TextBoxBase)txtAlt_m).add_MouseClick(new MouseEventHandler(txtAlt_m_MouseClick));
			((Control)txtAlt_m).add_Enter((EventHandler)txtAlt_m_Enter);
			((Control)txtAlt_m).add_Leave((EventHandler)txtAlt_m_Leave);
			((Control)panelHeightDatum).get_Controls().Add((Control)(object)optWGS);
			((Control)panelHeightDatum).get_Controls().Add((Control)(object)optMSL);
			((Control)panelHeightDatum).set_Location(new Point(187, 154));
			((Control)panelHeightDatum).set_Name("panelHeightDatum");
			((Control)panelHeightDatum).set_Size(new Size(71, 34));
			((Control)panelHeightDatum).set_TabIndex(39);
			toolTip.SetToolTip((Control)(object)panelHeightDatum, componentResourceManager.GetString("panelHeightDatum.ToolTip"));
			((Control)optWGS).set_AutoSize(true);
			((Control)optWGS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optWGS).set_Location(new Point(2, 16));
			((Control)optWGS).set_Name("optWGS");
			((Control)optWGS).set_Size(new Size(73, 17));
			((Control)optWGS).set_TabIndex(1);
			optWGS.set_TabStop(true);
			((Control)optWGS).set_Text("spheroid");
			toolTip.SetToolTip((Control)(object)optWGS, componentResourceManager.GetString("optWGS.ToolTip"));
			((ButtonBase)optWGS).set_UseVisualStyleBackColor(true);
			((Control)optMSL).set_AutoSize(true);
			optMSL.set_Checked(true);
			((Control)optMSL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optMSL).set_Location(new Point(2, 1));
			((Control)optMSL).set_Name("optMSL");
			((Control)optMSL).set_Size(new Size(50, 17));
			((Control)optMSL).set_TabIndex(0);
			optMSL.set_TabStop(true);
			((Control)optMSL).set_Text("MSL");
			toolTip.SetToolTip((Control)(object)optMSL, componentResourceManager.GetString("optMSL.ToolTip"));
			((ButtonBase)optMSL).set_UseVisualStyleBackColor(true);
			((Control)txtAlt_ft).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt_ft).set_Location(new Point(71, 162));
			((Control)txtAlt_ft).set_Name("txtAlt_ft");
			((Control)txtAlt_ft).set_Size(new Size(53, 20));
			((Control)txtAlt_ft).set_TabIndex(9);
			((Control)txtAlt_ft).set_Visible(false);
			((TextBoxBase)txtAlt_ft).add_MouseClick(new MouseEventHandler(txtAlt_ft_MouseClick));
			((Control)txtAlt_ft).add_Enter((EventHandler)txtAlt_ft_Enter);
			((Control)txtAlt_ft).add_Leave((EventHandler)txtAlt_ft_Leave);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongDeg);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatSec);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatMin);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatDeg);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongSec);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongMin);
			((Control)panelDMS).set_Location(new Point(80, 54));
			((Control)panelDMS).set_Name("panelDMS");
			((Control)panelDMS).set_Size(new Size(113, 52));
			((Control)panelDMS).set_TabIndex(1);
			((Control)txtLongDeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDeg).set_Location(new Point(4, 2));
			((Control)txtLongDeg).set_Name("txtLongDeg");
			((Control)txtLongDeg).set_Size(new Size(34, 20));
			((Control)txtLongDeg).set_TabIndex(0);
			((TextBoxBase)txtLongDeg).add_MouseClick(new MouseEventHandler(txtLongDeg_MouseClick));
			((Control)txtLongDeg).add_Enter((EventHandler)txtLongDeg_Enter);
			((Control)txtLongDeg).add_Leave((EventHandler)txtLongDeg_Leave);
			((Control)txtLatSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatSec).set_Location(new Point(71, 28));
			((Control)txtLatSec).set_Name("txtLatSec");
			((Control)txtLatSec).set_Size(new Size(39, 20));
			((Control)txtLatSec).set_TabIndex(5);
			((TextBoxBase)txtLatSec).add_MouseClick(new MouseEventHandler(txtLatSec_MouseClick));
			((Control)txtLatSec).add_Enter((EventHandler)txtLatSec_Enter);
			((Control)txtLatSec).add_Leave((EventHandler)txtLatSec_Leave);
			((Control)txtLatMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatMin).set_Location(new Point(44, 28));
			((Control)txtLatMin).set_Name("txtLatMin");
			((Control)txtLatMin).set_Size(new Size(21, 20));
			((Control)txtLatMin).set_TabIndex(4);
			((TextBoxBase)txtLatMin).add_MouseClick(new MouseEventHandler(txtLatMin_MouseClick));
			((Control)txtLatMin).add_Enter((EventHandler)txtLatMin_Enter);
			((Control)txtLatMin).add_Leave((EventHandler)txtLatMin_Leave);
			((Control)txtLatDeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDeg).set_Location(new Point(4, 28));
			((Control)txtLatDeg).set_Name("txtLatDeg");
			((Control)txtLatDeg).set_Size(new Size(34, 20));
			((Control)txtLatDeg).set_TabIndex(3);
			((TextBoxBase)txtLatDeg).add_MouseClick(new MouseEventHandler(txtLatDeg_MouseClick));
			((Control)txtLatDeg).add_Enter((EventHandler)txtLatDeg_Enter);
			((Control)txtLatDeg).add_Leave((EventHandler)txtLatDeg_Leave);
			((Control)txtLongSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongSec).set_Location(new Point(71, 2));
			((Control)txtLongSec).set_Name("txtLongSec");
			((Control)txtLongSec).set_Size(new Size(40, 20));
			((Control)txtLongSec).set_TabIndex(2);
			((TextBoxBase)txtLongSec).add_MouseClick(new MouseEventHandler(txtLongSec_MouseClick));
			((Control)txtLongSec).add_Enter((EventHandler)txtLongSec_Enter);
			((Control)txtLongSec).add_Leave((EventHandler)txtLongSec_Leave);
			((Control)txtLongMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongMin).set_Location(new Point(44, 2));
			((Control)txtLongMin).set_Name("txtLongMin");
			((Control)txtLongMin).set_Size(new Size(21, 20));
			((Control)txtLongMin).set_TabIndex(1);
			((TextBoxBase)txtLongMin).add_MouseClick(new MouseEventHandler(txtLongMin_MouseClick));
			((Control)txtLongMin).add_Enter((EventHandler)txtLongMin_Enter);
			((Control)txtLongMin).add_Leave((EventHandler)txtLongMin_Leave);
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(6, 105));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(57, 24));
			((Control)label19).set_TabIndex(4);
			((Control)label19).set_Text("Horizontal\r\ndatum");
			label19.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(28, 85));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(53, 13));
			((Control)label18).set_TabIndex(2);
			((Control)label18).set_Text("Latitude");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(18, 158));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(50, 13));
			((Control)label15).set_TabIndex(7);
			((Control)label15).set_Text("Altitude");
			((Control)panel2).get_Controls().Add((Control)(object)optFeet);
			((Control)panel2).get_Controls().Add((Control)(object)optMeters);
			((Control)panel2).set_Location(new Point(128, 151));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(42, 34));
			((Control)panel2).set_TabIndex(10);
			((Control)optFeet).set_AutoSize(true);
			((Control)optFeet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optFeet).set_Location(new Point(2, 16));
			((Control)optFeet).set_Name("optFeet");
			((Control)optFeet).set_Size(new Size(33, 17));
			((Control)optFeet).set_TabIndex(1);
			optFeet.set_TabStop(true);
			((Control)optFeet).set_Text("ft");
			((ButtonBase)optFeet).set_UseVisualStyleBackColor(true);
			optFeet.add_CheckedChanged((EventHandler)optFeet_CheckedChanged);
			((Control)optMeters).set_AutoSize(true);
			optMeters.set_Checked(true);
			((Control)optMeters).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optMeters).set_Location(new Point(2, 1));
			((Control)optMeters).set_Name("optMeters");
			((Control)optMeters).set_Size(new Size(34, 17));
			((Control)optMeters).set_TabIndex(0);
			optMeters.set_TabStop(true);
			((Control)optMeters).set_Text("m");
			((ButtonBase)optMeters).set_UseVisualStyleBackColor(true);
			((Control)panel1).get_Controls().Add((Control)(object)optDMM);
			((Control)panel1).get_Controls().Add((Control)(object)optDDD);
			((Control)panel1).get_Controls().Add((Control)(object)optDMS);
			((Control)panel1).set_Location(new Point(206, 53));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(61, 53));
			((Control)panel1).set_TabIndex(3);
			((Control)optDMM).set_AutoSize(true);
			((Control)optDMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optDMM).set_Location(new Point(3, 20));
			((Control)optDMM).set_Name("optDMM");
			((Control)optDMM).set_Size(new Size(54, 17));
			((Control)optDMM).set_TabIndex(1);
			optDMM.set_TabStop(true);
			((Control)optDMM).set_Text("dm.m");
			((ButtonBase)optDMM).set_UseVisualStyleBackColor(true);
			optDMM.add_CheckedChanged((EventHandler)optDMM_CheckedChanged);
			((Control)optDDD).set_AutoSize(true);
			((Control)optDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optDDD).set_Location(new Point(3, 35));
			((Control)optDDD).set_Name("optDDD");
			((Control)optDDD).set_Size(new Size(50, 17));
			((Control)optDDD).set_TabIndex(2);
			optDDD.set_TabStop(true);
			((Control)optDDD).set_Text("d.dd");
			((ButtonBase)optDDD).set_UseVisualStyleBackColor(true);
			optDDD.add_CheckedChanged((EventHandler)optDDD_CheckedChanged);
			((Control)optDMS).set_AutoSize(true);
			optDMS.set_Checked(true);
			((Control)optDMS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optDMS).set_Location(new Point(3, 5));
			((Control)optDMS).set_Name("optDMS");
			((Control)optDMS).set_Size(new Size(47, 17));
			((Control)optDMS).set_TabIndex(0);
			optDMS.set_TabStop(true);
			((Control)optDMS).set_Text("dms");
			((ButtonBase)optDMS).set_UseVisualStyleBackColor(true);
			optDMS.add_CheckedChanged((EventHandler)optDMS_CheckedChanged);
			cmbDatum.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbDatum).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbDatum).set_FormattingEnabled(true);
			cmbDatum.get_Items().AddRange(new object[82]
			{
				"84  WGS84", "82  NAD83/NAD1983 (=WGS84)", "85  JGD2000 (=WGS84)", "86  GDA94 (=WGS84)", "87  NZGD2000 (=WGS84)", "=================", "10  GoogleEarth (WGS84)", "16  European 1950  ", "28  New Zealand 1949 ", "29  North American 1927  ",
				"66  Australian Geodetic", "=================", "** for historical events only**", " 0  unknown (WGS84 used)", " 1  RGO code 1", " 2  RGO code 2", " 3  RGO code 3", " 4  RGO code 4", " 5  RGO code 5", "12  Christmas Island Astro 1967",
				"13  Chua Astro (Brazil Geodetic)", "14  Corrego Alegre (Brazil)", "15  Easter Island Astro 1967", "17  Graciosa Island (Azores)", "18  Gizo, Provisional DOS  ", "19  Guam", "20  Heard Astro 1969", "21  Iben Astro, Navy 1947 (Truk) ", "22  Indian", "23  Isla Socorro Astro",
				"24  Johnston Island 1961 ", "25  Kusaie Astro 1962,1965", "26  Luzon 1911 (Philippines) ", "27  Midway Astro 1961", "30 *Cape Canaveral ", "31 *White Sands", "32  Old Bavarian ", "33  Old Hawaiian ", "34  Ordnance Survey of Great Britain 1936 ", "35  Pico de las Nieves (Canaries) ",
				"36  Pitcairn Island Astro ", "37  Potsdam", "38  Provisional South American 1956", "39  Provisional South Chile 1963  ", "40  Pulkovo 1942  ", "41  South American 1969", "42  Southeast Island (Mahe)", "43  South Georgia Astro", "44  Swallow Islands (Solomons)", "45  Tananarive",
				"46  Tokyo ", "47  Tristan Astro 1968", "48  Viti Levu 1916 (Fiji) ", "49  Wake Island Astro 1952", "50  Yof Astro 1967 (Dakar)", "51  Palmer Astro 1969 (Antarctica)", "52  Efate (New Hebrides)", "53  Marcus Island 1965", "54  Canton Astro 1966  ", "56  Yap Island",
				"58  Kourou (French Guiana)", "59  Ordnance Survey of Great Britain 1970 ", "60  Qornoq (Greenland)", "61  Adindan (Ethiopia) ", "62  American Samoa 1962", "63  Arc-Cape (South Africa)", "64  Argentine  ", "65  Ascension Island 1958  ", "67  Bermuda 1957", "68  Berne 1898 ",
				"69  Betio Island 1966  ", "70  Camp Area Astro 1961-62 USGS", "71  Batavia (Java)", "72  Palestine (Israel,Jordan)", "73  Hermannskogel (Austria,Czech.,Yugoslavia)", "74  Kandawala (Ceylon)", "80  ETRS89 (European Terrestial Reference System)", "81  Amersfoort 1885 (Netherlands)", "88  NGRF2000 (=WGS84)", "89  KDG2000 (=WGS84)",
				"90  Hartebeesthoek94 (=WGS84)", "91  TWD94 (=WGS84)"
			});
			((Control)cmbDatum).set_Location(new Point(67, 108));
			((Control)cmbDatum).set_Name("cmbDatum");
			((Control)cmbDatum).set_Size(new Size(200, 21));
			((Control)cmbDatum).set_TabIndex(5);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLongDmm);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLatMM);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLatDmm);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLongMM);
			((Control)panelDMM).set_Location(new Point(7, 180));
			((Control)panelDMM).set_Name("panelDMM");
			((Control)panelDMM).set_Size(new Size(113, 52));
			((Control)panelDMM).set_TabIndex(6);
			((Control)txtLongDmm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDmm).set_Location(new Point(4, 2));
			((Control)txtLongDmm).set_Name("txtLongDmm");
			((Control)txtLongDmm).set_Size(new Size(34, 20));
			((Control)txtLongDmm).set_TabIndex(0);
			((TextBoxBase)txtLongDmm).add_MouseClick(new MouseEventHandler(txtLongDmm_MouseClick));
			((Control)txtLongDmm).add_Enter((EventHandler)txtLongDmm_Enter);
			((Control)txtLongDmm).add_Leave((EventHandler)txtLongDmm_Leave);
			((Control)txtLatMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatMM).set_Location(new Point(44, 28));
			((Control)txtLatMM).set_Name("txtLatMM");
			((Control)txtLatMM).set_Size(new Size(47, 20));
			((Control)txtLatMM).set_TabIndex(3);
			((Control)txtLatMM).add_Leave((EventHandler)txtLatMM_Leave);
			((Control)txtLatDmm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDmm).set_Location(new Point(4, 28));
			((Control)txtLatDmm).set_Name("txtLatDmm");
			((Control)txtLatDmm).set_Size(new Size(34, 20));
			((Control)txtLatDmm).set_TabIndex(2);
			((Control)txtLatDmm).add_Leave((EventHandler)txtLatDmm_Leave);
			((Control)txtLongMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongMM).set_Location(new Point(44, 2));
			((Control)txtLongMM).set_Name("txtLongMM");
			((Control)txtLongMM).set_Size(new Size(47, 20));
			((Control)txtLongMM).set_TabIndex(1);
			((Control)txtLongMM).add_Leave((EventHandler)txtLongMM_Leave);
			((Control)panelDDD).get_Controls().Add((Control)(object)txtLongDDD);
			((Control)panelDDD).get_Controls().Add((Control)(object)txtLatDDD);
			((Control)panelDDD).set_Location(new Point(133, 184));
			((Control)panelDDD).set_Name("panelDDD");
			((Control)panelDDD).set_Size(new Size(113, 52));
			((Control)panelDDD).set_TabIndex(26);
			((Control)txtLongDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDDD).set_Location(new Point(4, 2));
			((Control)txtLongDDD).set_Name("txtLongDDD");
			((Control)txtLongDDD).set_Size(new Size(82, 20));
			((Control)txtLongDDD).set_TabIndex(0);
			((TextBoxBase)txtLongDDD).add_MouseClick(new MouseEventHandler(txtLongDDD_MouseClick));
			((Control)txtLongDDD).add_Enter((EventHandler)txtLongDDD_Enter);
			((Control)txtLongDDD).add_Leave((EventHandler)txtLongDDD_Leave);
			((Control)txtLatDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDDD).set_Location(new Point(4, 28));
			((Control)txtLatDDD).set_Name("txtLatDDD");
			((Control)txtLatDDD).set_Size(new Size(82, 20));
			((Control)txtLatDDD).set_TabIndex(0);
			((Control)txtLatDDD).add_Leave((EventHandler)txtLatDDD_Leave);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(2, 59));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(79, 13));
			((Control)label17).set_TabIndex(0);
			((Control)label17).set_Text("E. Longitude");
			((Control)grpNames).set_Anchor((AnchorStyles)1);
			((Control)grpNames).get_Controls().Add((Control)(object)label81);
			((Control)grpNames).get_Controls().Add((Control)(object)grpILOCnames);
			((Control)grpNames).get_Controls().Add((Control)(object)label77);
			((Control)grpNames).get_Controls().Add((Control)(object)txtObserverEmail);
			((Control)grpNames).get_Controls().Add((Control)(object)label70);
			((Control)grpNames).get_Controls().Add((Control)(object)lblCharsLeftName);
			((Control)grpNames).get_Controls().Add((Control)(object)label69);
			((Control)grpNames).get_Controls().Add((Control)(object)cmdRenumberNames);
			((Control)grpNames).get_Controls().Add((Control)(object)groupBox5);
			((Control)grpNames).get_Controls().Add((Control)(object)cmdInsertDefaultName);
			((Control)grpNames).get_Controls().Add((Control)(object)cmdSetDefaultName);
			((Control)grpNames).get_Controls().Add((Control)(object)cmdDeleteName);
			((Control)grpNames).get_Controls().Add((Control)(object)cmdReplaceName);
			((Control)grpNames).get_Controls().Add((Control)(object)cmdAddName);
			((Control)grpNames).get_Controls().Add((Control)(object)label25);
			((Control)grpNames).get_Controls().Add((Control)(object)txtName);
			((Control)grpNames).get_Controls().Add((Control)(object)lstNames);
			((Control)grpNames).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpNames).set_Location(new Point(699, 72));
			((Control)grpNames).set_Name("grpNames");
			((Control)grpNames).set_Size(new Size(377, 582));
			((Control)grpNames).set_TabIndex(3);
			grpNames.set_TabStop(false);
			((Control)grpNames).set_Text("Names of Observers");
			((Control)grpILOCnames).get_Controls().Add((Control)(object)label23);
			((Control)grpILOCnames).get_Controls().Add((Control)(object)label24);
			((Control)grpILOCnames).get_Controls().Add((Control)(object)txtILOCName);
			((Control)grpILOCnames).get_Controls().Add((Control)(object)txtILOCStation_Name);
			((Control)grpILOCnames).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)grpILOCnames).set_Location(new Point(25, 90));
			((Control)grpILOCnames).set_Name("grpILOCnames");
			((Control)grpILOCnames).set_Size(new Size(336, 46));
			((Control)grpILOCnames).set_TabIndex(7);
			grpILOCnames.set_TabStop(false);
			((Control)grpILOCnames).set_Text("ILOC Station code  (if known)");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(181, 22));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(50, 13));
			((Control)label23).set_TabIndex(2);
			((Control)label23).set_Text("Observer");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Location(new Point(42, 21));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(40, 13));
			((Control)label24).set_TabIndex(0);
			((Control)label24).set_Text("Station");
			((Control)txtILOCName).set_Location(new Point(240, 18));
			((TextBoxBase)txtILOCName).set_MaxLength(4);
			((Control)txtILOCName).set_Name("txtILOCName");
			((Control)txtILOCName).set_Size(new Size(39, 20));
			((Control)txtILOCName).set_TabIndex(3);
			((Control)txtILOCName).add_MouseUp(new MouseEventHandler(txtILOCName_MouseUp));
			((Control)txtILOCStation_Name).set_Location(new Point(84, 18));
			((TextBoxBase)txtILOCStation_Name).set_MaxLength(5);
			((Control)txtILOCStation_Name).set_Name("txtILOCStation_Name");
			((Control)txtILOCStation_Name).set_Size(new Size(61, 20));
			((Control)txtILOCStation_Name).set_TabIndex(1);
			((Control)txtILOCStation_Name).add_MouseUp(new MouseEventHandler(txtILOCStation_Name_MouseUp));
			((Control)label77).set_AutoSize(true);
			((Control)label77).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label77).set_Location(new Point(58, 93));
			((Control)label77).set_Name("label77");
			((Control)label77).set_Size(new Size(236, 13));
			((Control)label77).set_TabIndex(5);
			((Control)label77).set_Text("Observer's Email address (optional, not archived)");
			((Control)txtObserverEmail).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtObserverEmail).set_Location(new Point(58, 108));
			((TextBoxBase)txtObserverEmail).set_MaxLength(0);
			((Control)txtObserverEmail).set_Name("txtObserverEmail");
			((Control)txtObserverEmail).set_Size(new Size(273, 20));
			((Control)txtObserverEmail).set_TabIndex(6);
			((Control)label70).set_AutoSize(true);
			((Control)label70).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label70).set_Location(new Point(109, 70));
			((Control)label70).set_Name("label70");
			((Control)label70).set_Size(new Size(163, 13));
			((Control)label70).set_TabIndex(4);
			((Control)label70).set_Text("Initial + Family name. eg 'J. Smith'");
			((Control)lblCharsLeftName).set_AutoSize(true);
			((Control)lblCharsLeftName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCharsLeftName).set_Location(new Point(280, 54));
			((Control)lblCharsLeftName).set_Name("lblCharsLeftName");
			((Control)lblCharsLeftName).set_Size(new Size(19, 13));
			((Control)lblCharsLeftName).set_TabIndex(3);
			((Control)lblCharsLeftName).set_Text("25");
			((Control)label69).set_AutoSize(true);
			((Control)label69).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label69).set_Location(new Point(218, 32));
			((Control)label69).set_Name("label69");
			((Control)label69).set_Size(new Size(70, 13));
			((Control)label69).set_TabIndex(1);
			((Control)label69).set_Text("25 characters");
			((Control)cmdRenumberNames).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRenumberNames).set_Location(new Point(274, 303));
			((Control)cmdRenumberNames).set_Name("cmdRenumberNames");
			((Control)cmdRenumberNames).set_Size(new Size(78, 35));
			((Control)cmdRenumberNames).set_TabIndex(14);
			((Control)cmdRenumberNames).set_Text("ReNumber");
			((ButtonBase)cmdRenumberNames).set_UseVisualStyleBackColor(true);
			((Control)cmdRenumberNames).add_Click((EventHandler)cmdRenumberNames_Click);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdMoveNameDown);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdSortByFamilyName);
			((Control)groupBox5).get_Controls().Add((Control)(object)label65);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdSortByName);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdMoveNameUp);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdSortByName_Number);
			((Control)groupBox5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox5).set_Location(new Point(58, 153));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(260, 101));
			((Control)groupBox5).set_TabIndex(8);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("Sort / move   Names");
			((Control)cmdMoveNameDown).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMoveNameDown).set_Location(new Point(194, 57));
			((Control)cmdMoveNameDown).set_Name("cmdMoveNameDown");
			((Control)cmdMoveNameDown).set_Size(new Size(42, 21));
			((Control)cmdMoveNameDown).set_TabIndex(5);
			((Control)cmdMoveNameDown).set_Text("down");
			((ButtonBase)cmdMoveNameDown).set_UseVisualStyleBackColor(true);
			((Control)cmdMoveNameDown).add_Click((EventHandler)cmdMoveNameDown_Click);
			((Control)cmdSortByFamilyName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSortByFamilyName).set_Location(new Point(8, 74));
			((Control)cmdSortByFamilyName).set_Name("cmdSortByFamilyName");
			((Control)cmdSortByFamilyName).set_Size(new Size(88, 21));
			((Control)cmdSortByFamilyName).set_TabIndex(2);
			((Control)cmdSortByFamilyName).set_Text("by Name -initial");
			((ButtonBase)cmdSortByFamilyName).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdSortByFamilyName).set_UseVisualStyleBackColor(true);
			((Control)cmdSortByFamilyName).add_Click((EventHandler)cmdSortByFamilyName_Click);
			((Control)label65).set_AutoSize(true);
			((Control)label65).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label65).set_Location(new Point(127, 48));
			((Control)label65).set_Name("label65");
			((Control)label65).set_Size(new Size(66, 13));
			((Control)label65).set_TabIndex(3);
			((Control)label65).set_Text("Move name:");
			((Control)cmdSortByName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSortByName).set_Location(new Point(8, 45));
			((Control)cmdSortByName).set_Name("cmdSortByName");
			((Control)cmdSortByName).set_Size(new Size(88, 21));
			((Control)cmdSortByName).set_TabIndex(1);
			((Control)cmdSortByName).set_Text("by Full Name");
			((ButtonBase)cmdSortByName).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdSortByName).set_UseVisualStyleBackColor(true);
			((Control)cmdSortByName).add_Click((EventHandler)cmdSortByName_Click);
			((Control)cmdMoveNameUp).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMoveNameUp).set_Location(new Point(194, 36));
			((Control)cmdMoveNameUp).set_Name("cmdMoveNameUp");
			((Control)cmdMoveNameUp).set_Size(new Size(42, 21));
			((Control)cmdMoveNameUp).set_TabIndex(4);
			((Control)cmdMoveNameUp).set_Text("up");
			((ButtonBase)cmdMoveNameUp).set_UseVisualStyleBackColor(true);
			((Control)cmdMoveNameUp).add_Click((EventHandler)cmdMoveNameUp_Click);
			((Control)cmdSortByName_Number).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSortByName_Number).set_Location(new Point(8, 16));
			((Control)cmdSortByName_Number).set_Name("cmdSortByName_Number");
			((Control)cmdSortByName_Number).set_Size(new Size(88, 21));
			((Control)cmdSortByName_Number).set_TabIndex(0);
			((Control)cmdSortByName_Number).set_Text("by Number");
			((ButtonBase)cmdSortByName_Number).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdSortByName_Number).set_UseVisualStyleBackColor(true);
			((Control)cmdSortByName_Number).add_Click((EventHandler)cmdSortByName_Number_Click);
			((Control)cmdInsertDefaultName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdInsertDefaultName).set_Location(new Point(151, 303));
			((Control)cmdInsertDefaultName).set_Name("cmdInsertDefaultName");
			((Control)cmdInsertDefaultName).set_Size(new Size(78, 35));
			((Control)cmdInsertDefaultName).set_TabIndex(12);
			((Control)cmdInsertDefaultName).set_Text("Add default\r\nas new name");
			((ButtonBase)cmdInsertDefaultName).set_UseVisualStyleBackColor(true);
			((Control)cmdInsertDefaultName).add_Click((EventHandler)cmdInsertDefaultName_Click);
			((Control)cmdSetDefaultName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetDefaultName).set_Location(new Point(151, 264));
			((Control)cmdSetDefaultName).set_Name("cmdSetDefaultName");
			((Control)cmdSetDefaultName).set_Size(new Size(78, 35));
			((Control)cmdSetDefaultName).set_TabIndex(11);
			((Control)cmdSetDefaultName).set_Text("Set selected as default");
			((ButtonBase)cmdSetDefaultName).set_UseVisualStyleBackColor(true);
			((Control)cmdSetDefaultName).add_Click((EventHandler)cmdSetDefaultName_Click);
			((Control)cmdDeleteName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDeleteName).set_Location(new Point(274, 264));
			((Control)cmdDeleteName).set_Name("cmdDeleteName");
			((Control)cmdDeleteName).set_Size(new Size(78, 35));
			((Control)cmdDeleteName).set_TabIndex(13);
			((Control)cmdDeleteName).set_Text("Delete\r\nselected");
			((ButtonBase)cmdDeleteName).set_UseVisualStyleBackColor(true);
			((Control)cmdDeleteName).add_Click((EventHandler)cmdDeleteName_Click);
			((Control)cmdReplaceName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdReplaceName).set_Location(new Point(28, 303));
			((Control)cmdReplaceName).set_Name("cmdReplaceName");
			((Control)cmdReplaceName).set_Size(new Size(78, 35));
			((Control)cmdReplaceName).set_TabIndex(10);
			((Control)cmdReplaceName).set_Text("Replace\r\nselected");
			((ButtonBase)cmdReplaceName).set_UseVisualStyleBackColor(true);
			((Control)cmdReplaceName).add_Click((EventHandler)cmdReplaceName_Click);
			((Control)cmdAddName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAddName).set_Location(new Point(28, 264));
			((Control)cmdAddName).set_Name("cmdAddName");
			((Control)cmdAddName).set_Size(new Size(78, 35));
			((Control)cmdAddName).set_TabIndex(9);
			((Control)cmdAddName).set_Text("Add as\r\nnew name");
			((ButtonBase)cmdAddName).set_UseVisualStyleBackColor(true);
			((Control)cmdAddName).add_Click((EventHandler)cmdAddName_Click);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(101, 32));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(101, 13));
			((Control)label25).set_TabIndex(0);
			((Control)label25).set_Text("Name for Observers");
			((Control)txtName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtName).set_Location(new Point(102, 50));
			((TextBoxBase)txtName).set_MaxLength(25);
			((Control)txtName).set_Name("txtName");
			((Control)txtName).set_Size(new Size(173, 20));
			((Control)txtName).set_TabIndex(2);
			((Control)txtName).add_TextChanged((EventHandler)txtName_TextChanged);
			((Control)txtName).add_Enter((EventHandler)txtName_Enter);
			((Control)txtName).add_KeyUp(new KeyEventHandler(txtName_KeyUp));
			((Control)txtName).add_Leave((EventHandler)txtName_Leave);
			((Control)grpEvents).set_Anchor((AnchorStyles)1);
			((Control)grpEvents).get_Controls().Add((Control)(object)lblUnsavedEdits);
			((Control)grpEvents).get_Controls().Add((Control)(object)picLiMovie);
			((Control)grpEvents).get_Controls().Add((Control)(object)grpEditControl);
			((Control)grpEvents).get_Controls().Add((Control)(object)label60);
			((Control)grpEvents).get_Controls().Add((Control)(object)label55);
			((Control)grpEvents).get_Controls().Add((Control)(object)cmdReplaceEvent);
			((Control)grpEvents).get_Controls().Add((Control)(object)label54);
			((Control)grpEvents).get_Controls().Add((Control)(object)label53);
			((Control)grpEvents).get_Controls().Add((Control)(object)cmdAddEvent);
			((Control)grpEvents).get_Controls().Add((Control)(object)groupBox8);
			((Control)grpEvents).get_Controls().Add((Control)(object)grpMethods);
			((Control)grpEvents).get_Controls().Add((Control)(object)grpObserver);
			((Control)grpEvents).get_Controls().Add((Control)(object)groupBox6);
			((Control)grpEvents).get_Controls().Add((Control)(object)lstEvents);
			((Control)grpEvents).get_Controls().Add((Control)(object)txtResidual);
			((Control)grpEvents).get_Controls().Add((Control)(object)txtPA);
			((Control)grpEvents).get_Controls().Add((Control)(object)txtMag);
			((Control)grpEvents).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpEvents).set_Location(new Point(100, 79));
			((Control)grpEvents).set_Name("grpEvents");
			((Control)grpEvents).set_Size(new Size(1001, 590));
			((Control)grpEvents).set_TabIndex(4);
			grpEvents.set_TabStop(false);
			((Control)grpEvents).set_Text("Events");
			((Control)lblUnsavedEdits).set_AutoSize(true);
			((Control)lblUnsavedEdits).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblUnsavedEdits).set_ForeColor(Color.Red);
			((Control)lblUnsavedEdits).set_Location(new Point(835, 344));
			((Control)lblUnsavedEdits).set_Name("lblUnsavedEdits");
			((Control)lblUnsavedEdits).set_Size(new Size(100, 13));
			((Control)lblUnsavedEdits).set_TabIndex(23);
			((Control)lblUnsavedEdits).set_Text("Unsaved edits...");
			((Control)lblUnsavedEdits).set_Visible(false);
			((Control)picLiMovie).set_Location(new Point(149, 460));
			((Control)picLiMovie).set_Name("picLiMovie");
			((Control)picLiMovie).set_Size(new Size(111, 59));
			picLiMovie.set_TabIndex(22);
			picLiMovie.set_TabStop(false);
			((Control)picLiMovie).set_Visible(false);
			((Control)grpEditControl).get_Controls().Add((Control)(object)optSortEventsByDate);
			((Control)grpEditControl).get_Controls().Add((Control)(object)optByNumber);
			((Control)grpEditControl).get_Controls().Add((Control)(object)cmdRenumberEvents);
			((Control)grpEditControl).get_Controls().Add((Control)(object)cmdDeleteEvent);
			((Control)grpEditControl).get_Controls().Add((Control)(object)cmdSortEvents);
			((Control)grpEditControl).get_Controls().Add((Control)(object)optByOBS);
			((Control)grpEditControl).get_Controls().Add((Control)(object)optByTEL);
			((Control)grpEditControl).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpEditControl).set_Location(new Point(7, 305));
			((Control)grpEditControl).set_Name("grpEditControl");
			((Control)grpEditControl).set_Size(new Size(382, 50));
			((Control)grpEditControl).set_TabIndex(21);
			grpEditControl.set_TabStop(false);
			((Control)grpEditControl).set_Text("Arrange events");
			((Control)optSortEventsByDate).set_AutoSize(true);
			((Control)optSortEventsByDate).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSortEventsByDate).set_Location(new Point(148, 26));
			((Control)optSortEventsByDate).set_Name("optSortEventsByDate");
			((Control)optSortEventsByDate).set_Size(new Size(55, 16));
			((Control)optSortEventsByDate).set_TabIndex(10);
			((Control)optSortEventsByDate).set_Text("by Date");
			((ButtonBase)optSortEventsByDate).set_UseVisualStyleBackColor(true);
			((Control)optByNumber).set_AutoSize(true);
			optByNumber.set_Checked(true);
			((Control)optByNumber).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByNumber).set_Location(new Point(148, 10));
			((Control)optByNumber).set_Name("optByNumber");
			((Control)optByNumber).set_Size(new Size(68, 16));
			((Control)optByNumber).set_TabIndex(9);
			optByNumber.set_TabStop(true);
			((Control)optByNumber).set_Text("by Number");
			((ButtonBase)optByNumber).set_UseVisualStyleBackColor(true);
			((Control)cmdRenumberEvents).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRenumberEvents).set_Location(new Point(13, 15));
			((Control)cmdRenumberEvents).set_Name("cmdRenumberEvents");
			((Control)cmdRenumberEvents).set_Size(new Size(75, 23));
			((Control)cmdRenumberEvents).set_TabIndex(7);
			((Control)cmdRenumberEvents).set_Text("ReNumber");
			((ButtonBase)cmdRenumberEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdRenumberEvents).add_Click((EventHandler)cmdRenumberEvents_Click);
			((Control)cmdDeleteEvent).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDeleteEvent).set_Location(new Point(281, 11));
			((Control)cmdDeleteEvent).set_Name("cmdDeleteEvent");
			((Control)cmdDeleteEvent).set_Size(new Size(78, 35));
			((Control)cmdDeleteEvent).set_TabIndex(6);
			((Control)cmdDeleteEvent).set_Text("Delete\r\nselected");
			((ButtonBase)cmdDeleteEvent).set_UseVisualStyleBackColor(true);
			((Control)cmdDeleteEvent).add_Click((EventHandler)cmdDeleteEvent_Click);
			((Control)cmdSortEvents).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSortEvents).set_Location(new Point(94, 15));
			((Control)cmdSortEvents).set_Name("cmdSortEvents");
			((Control)cmdSortEvents).set_Size(new Size(49, 23));
			((Control)cmdSortEvents).set_TabIndex(8);
			((Control)cmdSortEvents).set_Text("Sort");
			((ButtonBase)cmdSortEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdSortEvents).add_Click((EventHandler)cmdSortEvents_Click);
			((Control)optByOBS).set_AutoSize(true);
			((Control)optByOBS).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByOBS).set_Location(new Point(216, 26));
			((Control)optByOBS).set_Name("optByOBS");
			((Control)optByOBS).set_Size(new Size(54, 16));
			((Control)optByOBS).set_TabIndex(12);
			((Control)optByOBS).set_Text("by OBS");
			((ButtonBase)optByOBS).set_UseVisualStyleBackColor(true);
			((Control)optByTEL).set_AutoSize(true);
			((Control)optByTEL).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optByTEL).set_Location(new Point(216, 10));
			((Control)optByTEL).set_Name("optByTEL");
			((Control)optByTEL).set_Size(new Size(51, 16));
			((Control)optByTEL).set_TabIndex(11);
			((Control)optByTEL).set_Text("by TEL");
			((ButtonBase)optByTEL).set_UseVisualStyleBackColor(true);
			((Control)label60).set_AutoSize(true);
			((Control)label60).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label60).set_Location(new Point(797, 241));
			((Control)label60).set_Name("label60");
			((Control)label60).set_Size(new Size(160, 15));
			((Control)label60).set_TabIndex(14);
			((Control)label60).set_Text("Limb-corrected residual");
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label55).set_Location(new Point(921, 256));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(27, 12));
			((Control)label55).set_TabIndex(19);
			((Control)label55).set_Text("Mag.");
			((Control)cmdReplaceEvent).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReplaceEvent).set_Location(new Point(888, 307));
			((Control)cmdReplaceEvent).set_Name("cmdReplaceEvent");
			((Control)cmdReplaceEvent).set_Size(new Size(78, 35));
			((Control)cmdReplaceEvent).set_TabIndex(5);
			((Control)cmdReplaceEvent).set_Text("Replace\r\nselected");
			((ButtonBase)cmdReplaceEvent).set_UseVisualStyleBackColor(true);
			((Control)cmdReplaceEvent).add_Click((EventHandler)cmdReplaceEvent_Click);
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label54).set_Location(new Point(877, 256));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(24, 12));
			((Control)label54).set_TabIndex(17);
			((Control)label54).set_Text("P.A.");
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label53).set_Location(new Point(819, 256));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(41, 12));
			((Control)label53).set_TabIndex(15);
			((Control)label53).set_Text("Residual");
			((Control)cmdAddEvent).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAddEvent).set_Location(new Point(787, 307));
			((Control)cmdAddEvent).set_Name("cmdAddEvent");
			((Control)cmdAddEvent).set_Size(new Size(78, 35));
			((Control)cmdAddEvent).set_TabIndex(4);
			((Control)cmdAddEvent).set_Text("Add as \r\nnew event");
			((ButtonBase)cmdAddEvent).set_UseVisualStyleBackColor(true);
			((Control)cmdAddEvent).add_Click((EventHandler)cmdAddEvent_Click);
			((Control)groupBox8).get_Controls().Add((Control)(object)lblK2Star);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdGetWDS);
			((Control)groupBox8).get_Controls().Add((Control)(object)chkUnidentified);
			((Control)groupBox8).get_Controls().Add((Control)(object)grpPlanets);
			((Control)groupBox8).get_Controls().Add((Control)(object)label71);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdIdentify);
			((Control)groupBox8).get_Controls().Add((Control)(object)lblSAOValid);
			((Control)groupBox8).get_Controls().Add((Control)(object)label52);
			((Control)groupBox8).get_Controls().Add((Control)(object)label51);
			((Control)groupBox8).get_Controls().Add((Control)(object)label50);
			((Control)groupBox8).get_Controls().Add((Control)(object)txtGSC);
			((Control)groupBox8).get_Controls().Add((Control)(object)label49);
			((Control)groupBox8).get_Controls().Add((Control)(object)txtZC);
			((Control)groupBox8).get_Controls().Add((Control)(object)txtSAO);
			((Control)groupBox8).get_Controls().Add((Control)(object)txtXZ);
			((Control)groupBox8).get_Controls().Add((Control)(object)label56);
			((Control)groupBox8).get_Controls().Add((Control)(object)lblIsDouble);
			((Control)groupBox8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox8).set_Location(new Point(228, 19));
			((Control)groupBox8).set_Name("groupBox8");
			((Control)groupBox8).set_Size(new Size(173, 276));
			((Control)groupBox8).set_TabIndex(1);
			groupBox8.set_TabStop(false);
			((Control)groupBox8).set_Text("2.  Star");
			((Control)lblK2Star).set_AutoSize(true);
			((Control)lblK2Star).set_Location(new Point(117, 11));
			((Control)lblK2Star).set_Name("lblK2Star");
			((Control)lblK2Star).set_Size(new Size(47, 13));
			((Control)lblK2Star).set_TabIndex(41);
			((Control)lblK2Star).set_Text("K2 star");
			((Control)lblK2Star).set_Visible(false);
			((Control)cmdGetWDS).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetWDS).set_Location(new Point(105, 87));
			((Control)cmdGetWDS).set_Name("cmdGetWDS");
			((Control)cmdGetWDS).set_Size(new Size(65, 19));
			((Control)cmdGetWDS).set_TabIndex(39);
			((Control)cmdGetWDS).set_Text("WDS/IF/Var");
			toolTip.SetToolTip((Control)(object)cmdGetWDS, "Retrieves WDS and IF double star details");
			((ButtonBase)cmdGetWDS).set_UseVisualStyleBackColor(true);
			((Control)cmdGetWDS).add_Click((EventHandler)cmdGetWDS_Click);
			((Control)chkUnidentified).set_AutoSize(true);
			((Control)chkUnidentified).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkUnidentified).set_Location(new Point(17, 117));
			((Control)chkUnidentified).set_Name("chkUnidentified");
			((Control)chkUnidentified).set_Size(new Size(118, 17));
			((Control)chkUnidentified).set_TabIndex(13);
			((Control)chkUnidentified).set_Text("Unidentified star");
			((ButtonBase)chkUnidentified).set_UseVisualStyleBackColor(true);
			chkUnidentified.add_CheckedChanged((EventHandler)chkUnidentified_CheckedChanged);
			((Control)grpPlanets).get_Controls().Add((Control)(object)label58);
			((Control)grpPlanets).get_Controls().Add((Control)(object)label59);
			((Control)grpPlanets).get_Controls().Add((Control)(object)label57);
			((Control)grpPlanets).get_Controls().Add((Control)(object)cmbMoons);
			((Control)grpPlanets).get_Controls().Add((Control)(object)txtAsteroid);
			((Control)grpPlanets).get_Controls().Add((Control)(object)cmbPlanets);
			((Control)grpPlanets).set_Location(new Point(6, 186));
			((Control)grpPlanets).set_Name("grpPlanets");
			((Control)grpPlanets).set_Size(new Size(155, 84));
			((Control)grpPlanets).set_TabIndex(12);
			grpPlanets.set_TabStop(false);
			((Control)grpPlanets).set_Text("or  Solar System");
			((Control)label58).set_AutoSize(true);
			((Control)label58).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label58).set_Location(new Point(16, 41));
			((Control)label58).set_Name("label58");
			((Control)label58).set_Size(new Size(38, 13));
			((Control)label58).set_TabIndex(2);
			((Control)label58).set_Text("Moon");
			((Control)label59).set_AutoSize(true);
			((Control)label59).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label59).set_Location(new Point(10, 17));
			((Control)label59).set_Name("label59");
			((Control)label59).set_Size(new Size(43, 13));
			((Control)label59).set_TabIndex(0);
			((Control)label59).set_Text("Planet");
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label57).set_Location(new Point(18, 63));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(64, 13));
			((Control)label57).set_TabIndex(4);
			((Control)label57).set_Text("Asteroid #");
			cmbMoons.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbMoons).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbMoons).set_FormattingEnabled(true);
			((Control)cmbMoons).set_Location(new Point(56, 37));
			cmbMoons.set_MaxDropDownItems(10);
			((Control)cmbMoons).set_Name("cmbMoons");
			((Control)cmbMoons).set_Size(new Size(84, 21));
			((Control)cmbMoons).set_TabIndex(3);
			cmbMoons.add_SelectedIndexChanged((EventHandler)cmbMoons_SelectedIndexChanged);
			((Control)txtAsteroid).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAsteroid).set_Location(new Point(83, 59));
			((TextBoxBase)txtAsteroid).set_MaxLength(6);
			((Control)txtAsteroid).set_Name("txtAsteroid");
			((Control)txtAsteroid).set_Size(new Size(57, 20));
			((Control)txtAsteroid).set_TabIndex(5);
			((Control)txtAsteroid).add_TextChanged((EventHandler)txtAsteroid_TextChanged);
			((Control)txtAsteroid).add_MouseUp(new MouseEventHandler(txtAsteroid_MouseUp));
			cmbPlanets.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbPlanets).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbPlanets).set_FormattingEnabled(true);
			cmbPlanets.get_Items().AddRange(new object[10] { "", "Mercury", "Venus", "(Earth)", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto" });
			((Control)cmbPlanets).set_Location(new Point(56, 14));
			cmbPlanets.set_MaxDropDownItems(10);
			((Control)cmbPlanets).set_Name("cmbPlanets");
			((Control)cmbPlanets).set_Size(new Size(84, 21));
			((Control)cmbPlanets).set_TabIndex(1);
			cmbPlanets.add_SelectedIndexChanged((EventHandler)cmbPlanets_SelectedIndexChanged);
			((Control)label71).set_AutoSize(true);
			((Control)label71).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label71).set_Location(new Point(39, 131));
			((Control)label71).set_Name("label71");
			((Control)label71).set_Size(new Size(126, 13));
			((Control)label71).set_TabIndex(7);
			((Control)label71).set_Text("For unidentified stars only");
			((Control)cmdIdentify).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdIdentify).set_Location(new Point(10, 82));
			((Control)cmdIdentify).set_Name("cmdIdentify");
			((Control)cmdIdentify).set_Size(new Size(91, 28));
			((Control)cmdIdentify).set_TabIndex(11);
			((Control)cmdIdentify).set_Text("Identify star");
			((ButtonBase)cmdIdentify).set_UseVisualStyleBackColor(true);
			((Control)cmdIdentify).add_Click((EventHandler)cmdIdentify_Click);
			((Control)lblSAOValid).set_AutoSize(true);
			((Control)lblSAOValid).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblSAOValid).set_Location(new Point(116, 41));
			((Control)lblSAOValid).set_Name("lblSAOValid");
			((Control)lblSAOValid).set_Size(new Size(33, 12));
			((Control)lblSAOValid).set_TabIndex(4);
			((Control)lblSAOValid).set_Text("Invalid");
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label52).set_Location(new Point(34, 149));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(25, 12));
			((Control)label52).set_TabIndex(8);
			((Control)label52).set_Text("GSC");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label51).set_Location(new Point(29, 41));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(32, 13));
			((Control)label51).set_TabIndex(2);
			((Control)label51).set_Text("SAO");
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label50).set_Location(new Point(38, 63));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(23, 13));
			((Control)label50).set_TabIndex(5);
			((Control)label50).set_Text("XZ");
			((Control)txtGSC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtGSC).set_Location(new Point(63, 145));
			((Control)txtGSC).set_Name("txtGSC");
			((Control)txtGSC).set_Size(new Size(91, 20));
			((Control)txtGSC).set_TabIndex(9);
			((Control)txtGSC).add_MouseUp(new MouseEventHandler(txtGSC_MouseUp));
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label49).set_Location(new Point(38, 19));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(23, 13));
			((Control)label49).set_TabIndex(0);
			((Control)label49).set_Text("ZC");
			((Control)txtZC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtZC).set_Location(new Point(63, 15));
			((Control)txtZC).set_Name("txtZC");
			((Control)txtZC).set_Size(new Size(39, 20));
			((Control)txtZC).set_TabIndex(1);
			((Control)txtZC).add_TextChanged((EventHandler)txtZC_TextChanged);
			((Control)txtZC).add_MouseUp(new MouseEventHandler(txtZC_MouseUp));
			((Control)txtSAO).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSAO).set_Location(new Point(63, 37));
			((Control)txtSAO).set_Name("txtSAO");
			((Control)txtSAO).set_Size(new Size(51, 20));
			((Control)txtSAO).set_TabIndex(3);
			((Control)txtSAO).add_TextChanged((EventHandler)txtSAO_TextChanged);
			((Control)txtSAO).add_MouseUp(new MouseEventHandler(txtSAO_MouseUp));
			((Control)txtXZ).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtXZ).set_Location(new Point(63, 59));
			((Control)txtXZ).set_Name("txtXZ");
			((Control)txtXZ).set_Size(new Size(51, 20));
			((Control)txtXZ).set_TabIndex(6);
			((Control)txtXZ).add_TextChanged((EventHandler)txtXZ_TextChanged);
			((Control)txtXZ).add_MouseUp(new MouseEventHandler(txtXZ_MouseUp));
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label56).set_Location(new Point(48, 166));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(102, 12));
			((Control)label56).set_TabIndex(10);
			((Control)label56).set_Text("format: RRRRNNNNN ");
			((Control)lblIsDouble).set_AutoSize(true);
			((Control)lblIsDouble).set_Location(new Point(119, 29));
			((Control)lblIsDouble).set_Name("lblIsDouble");
			((Control)lblIsDouble).set_Size(new Size(45, 39));
			((Control)lblIsDouble).set_TabIndex(40);
			((Control)lblIsDouble).set_Text("star\r\nis \r\ndouble");
			((Control)lblIsDouble).set_Visible(false);
			((Control)grpMethods).get_Controls().Add((Control)(object)label80);
			((Control)grpMethods).get_Controls().Add((Control)(object)label72);
			((Control)grpMethods).get_Controls().Add((Control)(object)lblWDS);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmdGetDoubles);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbTemp);
			((Control)grpMethods).get_Controls().Add((Control)(object)lblSep);
			((Control)grpMethods).get_Controls().Add((Control)(object)lblPA);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbWDS);
			((Control)grpMethods).get_Controls().Add((Control)(object)label67);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbLightLevel);
			((Control)grpMethods).get_Controls().Add((Control)(object)label66);
			((Control)grpMethods).get_Controls().Add((Control)(object)txtDuration);
			((Control)grpMethods).get_Controls().Add((Control)(object)label48);
			((Control)grpMethods).get_Controls().Add((Control)(object)label47);
			((Control)grpMethods).get_Controls().Add((Control)(object)label46);
			((Control)grpMethods).get_Controls().Add((Control)(object)label45);
			((Control)grpMethods).get_Controls().Add((Control)(object)label44);
			((Control)grpMethods).get_Controls().Add((Control)(object)label43);
			((Control)grpMethods).get_Controls().Add((Control)(object)label42);
			((Control)grpMethods).get_Controls().Add((Control)(object)label41);
			((Control)grpMethods).get_Controls().Add((Control)(object)label40);
			((Control)grpMethods).get_Controls().Add((Control)(object)label39);
			((Control)grpMethods).get_Controls().Add((Control)(object)label38);
			((Control)grpMethods).get_Controls().Add((Control)(object)label37);
			((Control)grpMethods).get_Controls().Add((Control)(object)label36);
			((Control)grpMethods).get_Controls().Add((Control)(object)txtPE);
			((Control)grpMethods).get_Controls().Add((Control)(object)txtSN);
			((Control)grpMethods).get_Controls().Add((Control)(object)txtAccuracy);
			((Control)grpMethods).get_Controls().Add((Control)(object)txtComments);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbRemarkable);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbDoubles);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbStability);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbTransparency);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbCertainty);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbPE);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbTimeKeeping);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbMethod2);
			((Control)grpMethods).get_Controls().Add((Control)(object)cmbMethod1);
			((Control)grpMethods).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpMethods).set_Location(new Point(408, 19));
			((Control)grpMethods).set_Name("grpMethods");
			((Control)grpMethods).set_Size(new Size(344, 334));
			((Control)grpMethods).set_TabIndex(2);
			grpMethods.set_TabStop(false);
			((Control)grpMethods).set_Text("3.  Timing methods, Circumstances");
			((Control)label72).set_AutoSize(true);
			((Control)label72).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label72).set_Location(new Point(181, 99));
			((Control)label72).set_Name("label72");
			((Control)label72).set_Size(new Size(138, 12));
			((Control)label72).set_TabIndex(39);
			((Control)label72).set_Text("[ PE is NOT camera corrections ]");
			((Control)lblWDS).set_AutoSize(true);
			((Control)lblWDS).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblWDS).set_Location(new Point(231, 215));
			((Control)lblWDS).set_Name("lblWDS");
			((Control)lblWDS).set_Size(new Size(75, 12));
			((Control)lblWDS).set_TabIndex(21);
			((Control)lblWDS).set_Text("WDS component");
			((Control)cmdGetDoubles).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetDoubles).set_Location(new Point(317, 214));
			((Control)cmdGetDoubles).set_Name("cmdGetDoubles");
			((Control)cmdGetDoubles).set_Size(new Size(23, 14));
			((Control)cmdGetDoubles).set_TabIndex(38);
			((ButtonBase)cmdGetDoubles).set_TextAlign(ContentAlignment.TopCenter);
			toolTip.SetToolTip((Control)(object)cmdGetDoubles, "Retrieves WDS and IF double star details");
			((ButtonBase)cmdGetDoubles).set_UseVisualStyleBackColor(true);
			((Control)cmdGetDoubles).add_Click((EventHandler)cmdGetDoubles_Click);
			cmbTemp.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbTemp).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbTemp).set_FormattingEnabled(true);
			cmbTemp.get_Items().AddRange(new object[66]
			{
				" ", " 44C / 111F", " 43C / 109F", " 42C / 108F", " 41C / 106F", " 40C / 104F", " 39C / 102F", " 38C / 100F", " 37C / 99F", " 36C / 97F",
				" 35C / 95F", " 34C / 93F", " 33C / 91F", " 32C / 90F", " 31C / 88F", " 30C / 86F", " 29C / 84F", " 28C / 82F", " 27C / 81F", " 26C / 79F",
				" 25C / 77F", " 24C / 75F", " 23C / 73F", " 22C / 72F", " 21C / 70F", " 20C / 68F", " 19C / 66F", " 18C / 64F", " 17C / 63F", " 16C / 61F",
				" 15C / 59F", " 14C / 57F", " 13C / 55F", " 12C / 54F", " 11C / 52F", " 10C / 50F", "  9C / 48F", "  8C / 46F", "  7C / 45F", "  6C / 43F",
				"  5C / 41F", "  4C / 39F", "  3C / 37F", "  2C / 36F", "  1C / 34F", "  0C / 32F", " -1C / 30F", " -2C / 28F", " -3C / 27F", " -4C / 25F",
				" -5C / 23F", " -6C / 21F", " -7C / 19F", " -8C / 18F", " -9C / 16F", "-10C / 14F", "-11C / 12F", "-12C / 10F", "-13C / 9F", "-14C / 7F",
				"-15C / 5F", "-16C / 3F", "-17C / 1F", "-18C / 0F", "-19C / -2F", "-20C / -4F"
			});
			((Control)cmbTemp).set_Location(new Point(258, 268));
			cmbTemp.set_MaxDropDownItems(20);
			((Control)cmbTemp).set_Name("cmbTemp");
			((Control)cmbTemp).set_Size(new Size(76, 21));
			((Control)cmbTemp).set_TabIndex(37);
			((Control)lblSep).set_AutoSize(true);
			((Control)lblSep).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblSep).set_Location(new Point(280, 227));
			((Control)lblSep).set_Name("lblSep");
			((Control)lblSep).set_Size(new Size(28, 12));
			((Control)lblSep).set_TabIndex(36);
			((Control)lblSep).set_Text("Sep =");
			((Control)lblPA).set_AutoSize(true);
			((Control)lblPA).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPA).set_Location(new Point(280, 238));
			((Control)lblPA).set_Name("lblPA");
			((Control)lblPA).set_Size(new Size(25, 12));
			((Control)lblPA).set_TabIndex(35);
			((Control)lblPA).set_Text("PA =");
			cmbWDS.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbWDS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbWDS).set_FormattingEnabled(true);
			cmbWDS.get_Items().AddRange(new object[53]
			{
				" ", "A", "B", "C", "D", "E", "F", "G", "H", "I",
				"J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
				"T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c",
				"d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
				"n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
				"x", "y", "z"
			});
			((Control)cmbWDS).set_Location(new Point(241, 229));
			cmbWDS.set_MaxDropDownItems(20);
			((Control)cmbWDS).set_Name("cmbWDS");
			((Control)cmbWDS).set_Size(new Size(35, 21));
			((Control)cmbWDS).set_TabIndex(34);
			cmbWDS.add_SelectedIndexChanged((EventHandler)cmbWDS_SelectedIndexChanged);
			((Control)label67).set_AutoSize(true);
			((Control)label67).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label67).set_Location(new Point(92, 256));
			((Control)label67).set_Name("label67");
			((Control)label67).set_Size(new Size(47, 12));
			((Control)label67).set_TabIndex(26);
			((Control)label67).set_Text("Light level");
			cmbLightLevel.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbLightLevel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbLightLevel).set_FormattingEnabled(true);
			cmbLightLevel.get_Items().AddRange(new object[3] { "", "25% - Fresnel", "50% - Diameter" });
			((Control)cmbLightLevel).set_Location(new Point(72, 268));
			((Control)cmbLightLevel).set_Name("cmbLightLevel");
			((Control)cmbLightLevel).set_Size(new Size(96, 21));
			((Control)cmbLightLevel).set_TabIndex(27);
			((Control)label66).set_AutoSize(true);
			((Control)label66).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label66).set_Location(new Point(189, 256));
			((Control)label66).set_Name("label66");
			((Control)label66).set_Size(new Size(48, 12));
			((Control)label66).set_TabIndex(28);
			((Control)label66).set_Text("Durn (sec)");
			((Control)txtDuration).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDuration).set_Location(new Point(191, 268));
			((Control)txtDuration).set_Name("txtDuration");
			((Control)txtDuration).set_Size(new Size(44, 20));
			((Control)txtDuration).set_TabIndex(29);
			((Control)txtDuration).add_Leave((EventHandler)txtDuration_Leave);
			((Control)txtDuration).add_MouseUp(new MouseEventHandler(txtDuration_MouseUp));
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label48).set_Location(new Point(46, 21));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(227, 12));
			((Control)label48).set_TabIndex(0);
			((Control)label48).set_Text("Method of Timing && recording (use the left box, or both)");
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(8, 293));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(120, 12));
			((Control)label47).set_TabIndex(32);
			((Control)label47).set_Text("Comments    [ not archived ]");
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label46).set_Location(new Point(263, 256));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(57, 12));
			((Control)label46).set_TabIndex(30);
			((Control)label46).set_Text("Temperature");
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(8, 177));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(38, 12));
			((Control)label45).set_TabIndex(13);
			((Control)label45).set_Text("Stability");
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label44).set_Location(new Point(71, 177));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(61, 12));
			((Control)label44).set_TabIndex(15);
			((Control)label44).set_Text("Transparency");
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label43).set_Location(new Point(66, 138));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(118, 12));
			((Control)label43).set_TabIndex(11);
			((Control)label43).set_Text("Remarkable circumstances");
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(6, 216));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(57, 12));
			((Control)label42).set_TabIndex(19);
			((Control)label42).set_Text("Double stars");
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(8, 256));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(41, 12));
			((Control)label41).set_TabIndex(24);
			((Control)label41).set_Text("S/N ratio");
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label40).set_Location(new Point(8, 138));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(45, 12));
			((Control)label40).set_TabIndex(9);
			((Control)label40).set_Text("Accuracy");
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(8, 100));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(17, 12));
			((Control)label39).set_TabIndex(5);
			((Control)label39).set_Text("PE");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label38).set_Location(new Point(68, 99));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(63, 12));
			((Control)label38).set_TabIndex(7);
			((Control)label38).set_Text("PE application");
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(137, 177));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(43, 12));
			((Control)label37).set_TabIndex(17);
			((Control)label37).set_Text("Certainty");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(8, 59));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(55, 12));
			((Control)label36).set_TabIndex(3);
			((Control)label36).set_Text("Time source");
			((Control)txtPE).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtPE).set_Location(new Point(8, 112));
			((Control)txtPE).set_Name("txtPE");
			((Control)txtPE).set_Size(new Size(41, 20));
			((Control)txtPE).set_TabIndex(6);
			((Control)txtPE).add_TextChanged((EventHandler)txtPE_TextChanged);
			((Control)txtPE).add_MouseUp(new MouseEventHandler(txtPE_MouseUp));
			((Control)txtSN).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSN).set_Location(new Point(8, 268));
			((Control)txtSN).set_Name("txtSN");
			((Control)txtSN).set_Size(new Size(41, 20));
			((Control)txtSN).set_TabIndex(25);
			((Control)txtSN).add_MouseUp(new MouseEventHandler(txtSN_MouseUp));
			((Control)txtAccuracy).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAccuracy).set_Location(new Point(8, 151));
			((Control)txtAccuracy).set_Name("txtAccuracy");
			((Control)txtAccuracy).set_Size(new Size(41, 20));
			((Control)txtAccuracy).set_TabIndex(10);
			((Control)txtAccuracy).add_Leave((EventHandler)txtAccuracy_Leave);
			((Control)txtAccuracy).add_MouseUp(new MouseEventHandler(txtAccuracy_MouseUp));
			((Control)txtComments).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtComments).set_Location(new Point(8, 306));
			((Control)txtComments).set_Name("txtComments");
			((Control)txtComments).set_Size(new Size(313, 20));
			((Control)txtComments).set_TabIndex(33);
			((Control)txtComments).add_Leave((EventHandler)txtComments_Leave);
			cmbRemarkable.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbRemarkable).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbRemarkable).set_FormattingEnabled(true);
			cmbRemarkable.get_Items().AddRange(new object[10] { "No remarkable circumstances", "Gradual (not instantaneous) event", "Dark limb visible", "By averted vision", "Star faint", "Through thin cloud", "Many clouds", "Strong wind", "In strong twilight", "In daylight (sun alt. > -6 deg)" });
			((Control)cmbRemarkable).set_Location(new Point(66, 151));
			cmbRemarkable.set_MaxDropDownItems(10);
			((Control)cmbRemarkable).set_Name("cmbRemarkable");
			((Control)cmbRemarkable).set_Size(new Size(230, 21));
			((Control)cmbRemarkable).set_TabIndex(12);
			cmbDoubles.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbDoubles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbDoubles).set_FormattingEnabled(true);
			cmbDoubles.get_Items().AddRange(new object[8] { "No double star effects seen or noted", "Preceding (west) component", "Following (east) component", "North component", "South component", "Brighter component", "Fainter component", "Unidentified star" });
			((Control)cmbDoubles).set_Location(new Point(6, 229));
			((Control)cmbDoubles).set_Name("cmbDoubles");
			((Control)cmbDoubles).set_Size(new Size(214, 21));
			((Control)cmbDoubles).set_TabIndex(20);
			cmbStability.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbStability).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbStability).set_FormattingEnabled(true);
			cmbStability.get_Items().AddRange(new object[3] { "Good", "Fair", "Poor" });
			((Control)cmbStability).set_Location(new Point(8, 190));
			((Control)cmbStability).set_Name("cmbStability");
			((Control)cmbStability).set_Size(new Size(57, 21));
			((Control)cmbStability).set_TabIndex(14);
			cmbTransparency.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbTransparency).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbTransparency).set_FormattingEnabled(true);
			cmbTransparency.get_Items().AddRange(new object[3] { "Good", "Fair", "Poor" });
			((Control)cmbTransparency).set_Location(new Point(72, 190));
			((Control)cmbTransparency).set_Name("cmbTransparency");
			((Control)cmbTransparency).set_Size(new Size(57, 21));
			((Control)cmbTransparency).set_TabIndex(16);
			cmbCertainty.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbCertainty).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbCertainty).set_FormattingEnabled(true);
			cmbCertainty.get_Items().AddRange(new object[3] { "Sure that the event occurred", "The event might be spurious", "The event is almost certainly spurious" });
			((Control)cmbCertainty).set_Location(new Point(136, 190));
			((Control)cmbCertainty).set_Name("cmbCertainty");
			((Control)cmbCertainty).set_Size(new Size(204, 21));
			((Control)cmbCertainty).set_TabIndex(18);
			cmbPE.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbPE).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbPE).set_FormattingEnabled(true);
			cmbPE.get_Items().AddRange(new object[6] { "PE has been subtracted from the reported time.", "PE has NOT been subtracted from the reported time.", "PE not relevant to the method of timing [eg Video]", "Not known whether any PE has been applied", "- has been subtracted. PE is assumed [ILOC data]", "- has been subtracted. PE is unknown [RGO data]" });
			((Control)cmbPE).set_Location(new Point(66, 112));
			((Control)cmbPE).set_Name("cmbPE");
			((Control)cmbPE).set_Size(new Size(269, 21));
			((Control)cmbPE).set_TabIndex(8);
			cmbPE.add_SelectedIndexChanged((EventHandler)cmbPE_SelectedIndexChanged);
			cmbTimeKeeping.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbTimeKeeping).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbTimeKeeping).set_FormattingEnabled(true);
			cmbTimeKeeping.get_Items().AddRange(new object[8] { "", "GPS   (using 1PPS output, NOT screen display)", "Radio signal (standard time signal)", "Network Time Protocol  (using NTP software)", "Clock (adjusted by standard time signal)", "Telephone", "Some Medium related with standard time signal", "GPS screen display, or Computer clock.  [poor accuracy]" });
			((Control)cmbTimeKeeping).set_Location(new Point(8, 73));
			((Control)cmbTimeKeeping).set_Name("cmbTimeKeeping");
			((Control)cmbTimeKeeping).set_Size(new Size(313, 21));
			((Control)cmbTimeKeeping).set_TabIndex(4);
			cmbMethod2.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbMethod2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbMethod2).set_FormattingEnabled(true);
			cmbMethod2.get_Items().AddRange(new object[12]
			{
				"", "Video (time insert) + frame anal.", "Video (other) + frame analysis", "Video (other) + replay", "Stopwatch (visual)", "Tape Recorder (visual)", "Eye and ear", "Photoelectric", "Keytapping", "Chronograph",
				"Camera and clock", "Time base adjusted (graze)"
			});
			((Control)cmbMethod2).set_Location(new Point(177, 34));
			cmbMethod2.set_MaxDropDownItems(11);
			((Control)cmbMethod2).set_Name("cmbMethod2");
			((Control)cmbMethod2).set_Size(new Size(161, 21));
			((Control)cmbMethod2).set_TabIndex(2);
			cmbMethod2.add_SelectedIndexChanged((EventHandler)cmbMethod2_SelectedIndexChanged);
			cmbMethod1.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbMethod1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbMethod1).set_FormattingEnabled(true);
			cmbMethod1.get_Items().AddRange(new object[11]
			{
				"", "Video (time insert) + frame anal.", "Video (other) + frame analysis", "Video (other) + replay", "Stopwatch (visual)", "Tape Recorder (visual)", "Eye and ear", "Photoelectric", "Keytapping", "Chronograph",
				"Camera and clock"
			});
			((Control)cmbMethod1).set_Location(new Point(9, 34));
			cmbMethod1.set_MaxDropDownItems(10);
			((Control)cmbMethod1).set_Name("cmbMethod1");
			((Control)cmbMethod1).set_Size(new Size(161, 21));
			((Control)cmbMethod1).set_TabIndex(1);
			cmbMethod1.add_SelectedIndexChanged((EventHandler)cmbMethod1_SelectedIndexChanged);
			((Control)grpObserver).get_Controls().Add((Control)(object)label63);
			((Control)grpObserver).get_Controls().Add((Control)(object)txtRecorder);
			((Control)grpObserver).get_Controls().Add((Control)(object)label61);
			((Control)grpObserver).get_Controls().Add((Control)(object)grpILOCevents);
			((Control)grpObserver).get_Controls().Add((Control)(object)cmbObservers);
			((Control)grpObserver).get_Controls().Add((Control)(object)cmbRecorders);
			((Control)grpObserver).get_Controls().Add((Control)(object)cmbSites);
			((Control)grpObserver).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpObserver).set_Location(new Point(763, 19));
			((Control)grpObserver).set_Name("grpObserver");
			((Control)grpObserver).set_Size(new Size(228, 202));
			((Control)grpObserver).set_TabIndex(3);
			grpObserver.set_TabStop(false);
			((Control)grpObserver).set_Text("4.  Observer");
			((Control)label63).set_AutoSize(true);
			((Control)label63).set_Location(new Point(9, 71));
			((Control)label63).set_Name("label63");
			((Control)label63).set_Size(new Size(58, 13));
			((Control)label63).set_TabIndex(2);
			((Control)label63).set_Text("Observer");
			((Control)txtRecorder).set_AutoSize(true);
			((Control)txtRecorder).set_Location(new Point(9, 103));
			((Control)txtRecorder).set_Name("txtRecorder");
			((Control)txtRecorder).set_Size(new Size(59, 13));
			((Control)txtRecorder).set_TabIndex(4);
			((Control)txtRecorder).set_Text("Recorder");
			((Control)label61).set_AutoSize(true);
			((Control)label61).set_Location(new Point(9, 19));
			((Control)label61).set_Name("label61");
			((Control)label61).set_Size(new Size(79, 13));
			((Control)label61).set_TabIndex(0);
			((Control)label61).set_Text("Station (site)");
			((Control)grpILOCevents).get_Controls().Add((Control)(object)txtILOCEventObserver);
			((Control)grpILOCevents).get_Controls().Add((Control)(object)txtILOCEventRecorder);
			((Control)grpILOCevents).get_Controls().Add((Control)(object)txtILOCEventTelescope);
			((Control)grpILOCevents).get_Controls().Add((Control)(object)txtILOCEventStation);
			((Control)grpILOCevents).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)grpILOCevents).set_Location(new Point(9, 123));
			((Control)grpILOCevents).set_Name("grpILOCevents");
			((Control)grpILOCevents).set_Size(new Size(190, 73));
			((Control)grpILOCevents).set_TabIndex(6);
			grpILOCevents.set_TabStop(false);
			((Control)grpILOCevents).set_Text("ILOC codes (if known)");
			((Control)txtILOCEventObserver).set_Location(new Point(53, 47));
			((Control)txtILOCEventObserver).set_Name("txtILOCEventObserver");
			((Control)txtILOCEventObserver).set_Size(new Size(24, 20));
			((Control)txtILOCEventObserver).set_TabIndex(2);
			((Control)txtILOCEventRecorder).set_Location(new Point(122, 49));
			((Control)txtILOCEventRecorder).set_Name("txtILOCEventRecorder");
			((Control)txtILOCEventRecorder).set_Size(new Size(26, 20));
			((Control)txtILOCEventRecorder).set_TabIndex(3);
			((Control)txtILOCEventTelescope).set_Location(new Point(122, 19));
			((Control)txtILOCEventTelescope).set_Name("txtILOCEventTelescope");
			((Control)txtILOCEventTelescope).set_Size(new Size(29, 20));
			((Control)txtILOCEventTelescope).set_TabIndex(1);
			((Control)txtILOCEventStation).set_Location(new Point(48, 19));
			((Control)txtILOCEventStation).set_Name("txtILOCEventStation");
			((Control)txtILOCEventStation).set_Size(new Size(40, 20));
			((Control)txtILOCEventStation).set_TabIndex(0);
			cmbObservers.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbObservers).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbObservers).set_FormattingEnabled(true);
			((Control)cmbObservers).set_Location(new Point(70, 68));
			cmbObservers.set_MaxDropDownItems(20);
			((Control)cmbObservers).set_Name("cmbObservers");
			((Control)cmbObservers).set_Size(new Size(152, 21));
			((Control)cmbObservers).set_TabIndex(3);
			cmbRecorders.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbRecorders).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbRecorders).set_FormattingEnabled(true);
			((Control)cmbRecorders).set_Location(new Point(70, 99));
			cmbRecorders.set_MaxDropDownItems(20);
			((Control)cmbRecorders).set_Name("cmbRecorders");
			((Control)cmbRecorders).set_Size(new Size(152, 21));
			((Control)cmbRecorders).set_TabIndex(5);
			cmbSites.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbSites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbSites).set_FormattingEnabled(true);
			((Control)cmbSites).set_Location(new Point(6, 34));
			cmbSites.set_MaxDropDownItems(20);
			((Control)cmbSites).set_Name("cmbSites");
			((Control)cmbSites).set_Size(new Size(216, 21));
			((Control)cmbSites).set_TabIndex(1);
			cmbSites.add_SelectedIndexChanged((EventHandler)cmbSites_SelectedIndexChanged);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmdCamera);
			((Control)groupBox6).get_Controls().Add((Control)(object)panel4);
			((Control)groupBox6).get_Controls().Add((Control)(object)label35);
			((Control)groupBox6).get_Controls().Add((Control)(object)label34);
			((Control)groupBox6).get_Controls().Add((Control)(object)label33);
			((Control)groupBox6).get_Controls().Add((Control)(object)label32);
			((Control)groupBox6).get_Controls().Add((Control)(object)label31);
			((Control)groupBox6).get_Controls().Add((Control)(object)label30);
			((Control)groupBox6).get_Controls().Add((Control)(object)label29);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmdToday);
			((Control)groupBox6).get_Controls().Add((Control)(object)chkGraze);
			((Control)groupBox6).get_Controls().Add((Control)(object)optReappear);
			((Control)groupBox6).get_Controls().Add((Control)(object)optFailed);
			((Control)groupBox6).get_Controls().Add((Control)(object)optStarted);
			((Control)groupBox6).get_Controls().Add((Control)(object)optMiss);
			((Control)groupBox6).get_Controls().Add((Control)(object)optStopped);
			((Control)groupBox6).get_Controls().Add((Control)(object)optFlash);
			((Control)groupBox6).get_Controls().Add((Control)(object)optBlink);
			((Control)groupBox6).get_Controls().Add((Control)(object)optDisappear);
			((Control)groupBox6).get_Controls().Add((Control)(object)label27);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtSecond);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnMonth);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnDay);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnHour);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnMin);
			((Control)groupBox6).get_Controls().Add((Control)(object)updnYear);
			((Control)groupBox6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox6).set_Location(new Point(8, 19));
			((Control)groupBox6).set_Name("groupBox6");
			((Control)groupBox6).set_Size(new Size(209, 276));
			((Control)groupBox6).set_TabIndex(0);
			groupBox6.set_TabStop(false);
			((Control)groupBox6).set_Text("1.  Event time && type");
			((Control)cmdCamera).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCamera).set_Location(new Point(155, 75));
			((Control)cmdCamera).set_Name("cmdCamera");
			((Control)cmdCamera).set_Size(new Size(51, 22));
			((Control)cmdCamera).set_TabIndex(25);
			((Control)cmdCamera).set_Text("Camera");
			((ButtonBase)cmdCamera).set_UseVisualStyleBackColor(true);
			((Control)cmdCamera).add_Click((EventHandler)cmdCamera_Click);
			((Control)panel4).get_Controls().Add((Control)(object)optUmbra);
			((Control)panel4).get_Controls().Add((Control)(object)optBrightLimb);
			((Control)panel4).get_Controls().Add((Control)(object)optDarkLimb);
			((Control)panel4).get_Controls().Add((Control)(object)label28);
			((Control)panel4).set_Location(new Point(3, 222));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(206, 46));
			((Control)panel4).set_TabIndex(24);
			((Control)optUmbra).set_AutoSize(true);
			((Control)optUmbra).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optUmbra).set_Location(new Point(144, 20));
			((Control)optUmbra).set_Name("optUmbra");
			((Control)optUmbra).set_Size(new Size(56, 17));
			((Control)optUmbra).set_TabIndex(3);
			optUmbra.set_TabStop(true);
			((Control)optUmbra).set_Text("Umbra");
			((ButtonBase)optUmbra).set_UseVisualStyleBackColor(true);
			((Control)optBrightLimb).set_AutoSize(true);
			((Control)optBrightLimb).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optBrightLimb).set_Location(new Point(71, 20));
			((Control)optBrightLimb).set_Name("optBrightLimb");
			((Control)optBrightLimb).set_Size(new Size(73, 17));
			((Control)optBrightLimb).set_TabIndex(2);
			optBrightLimb.set_TabStop(true);
			((Control)optBrightLimb).set_Text("Bright limb");
			((ButtonBase)optBrightLimb).set_UseVisualStyleBackColor(true);
			((Control)optDarkLimb).set_AutoSize(true);
			((Control)optDarkLimb).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDarkLimb).set_Location(new Point(2, 20));
			((Control)optDarkLimb).set_Name("optDarkLimb");
			((Control)optDarkLimb).set_Size(new Size(69, 17));
			((Control)optDarkLimb).set_TabIndex(1);
			optDarkLimb.set_TabStop(true);
			((Control)optDarkLimb).set_Text("Dark limb");
			((ButtonBase)optDarkLimb).set_UseVisualStyleBackColor(true);
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(4, 4));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(33, 13));
			((Control)label28).set_TabIndex(0);
			((Control)label28).set_Text("Limb");
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label35).set_Location(new Point(150, 23));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(37, 13));
			((Control)label35).set_TabIndex(6);
			((Control)label35).set_Text("Today");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(57, 23));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(37, 13));
			((Control)label34).set_TabIndex(2);
			((Control)label34).set_Text("Month");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label33).set_Location(new Point(99, 23));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(26, 13));
			((Control)label33).set_TabIndex(4);
			((Control)label33).set_Text("Day");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label32).set_Location(new Point(13, 63));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(30, 13));
			((Control)label32).set_TabIndex(8);
			((Control)label32).set_Text("Hour");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label31).set_Location(new Point(58, 63));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(27, 13));
			((Control)label31).set_TabIndex(10);
			((Control)label31).set_Text("Min.");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(104, 63));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(44, 13));
			((Control)label30).set_TabIndex(12);
			((Control)label30).set_Text("Second");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(8, 23));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(29, 13));
			((Control)label29).set_TabIndex(0);
			((Control)label29).set_Text("Year");
			((Control)cmdToday).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdToday).set_Location(new Point(159, 37));
			((Control)cmdToday).set_Name("cmdToday");
			((Control)cmdToday).set_Size(new Size(19, 18));
			((Control)cmdToday).set_TabIndex(7);
			((ButtonBase)cmdToday).set_UseVisualStyleBackColor(true);
			((Control)cmdToday).add_Click((EventHandler)cmdToday_Click);
			((Control)chkGraze).set_AutoSize(true);
			chkGraze.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkGraze).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkGraze).set_Location(new Point(6, 197));
			((Control)chkGraze).set_Name("chkGraze");
			((Control)chkGraze).set_Size(new Size(99, 17));
			((Control)chkGraze).set_TabIndex(23);
			((Control)chkGraze).set_Text("Graze event ");
			((ButtonBase)chkGraze).set_UseVisualStyleBackColor(true);
			((Control)optReappear).set_AutoSize(true);
			((Control)optReappear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optReappear).set_Location(new Point(74, 126));
			((Control)optReappear).set_Name("optReappear");
			((Control)optReappear).set_Size(new Size(72, 17));
			((Control)optReappear).set_TabIndex(16);
			((Control)optReappear).set_Text("Reappear");
			((ButtonBase)optReappear).set_UseVisualStyleBackColor(true);
			((Control)optFailed).set_AutoSize(true);
			((Control)optFailed).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optFailed).set_Location(new Point(147, 162));
			((Control)optFailed).set_Name("optFailed");
			((Control)optFailed).set_Size(new Size(51, 17));
			((Control)optFailed).set_TabIndex(22);
			((Control)optFailed).set_Text("Other");
			((ButtonBase)optFailed).set_UseVisualStyleBackColor(true);
			((Control)optFailed).add_Click((EventHandler)optFailed_Click);
			((Control)optStarted).set_AutoSize(true);
			((Control)optStarted).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optStarted).set_Location(new Point(5, 162));
			((Control)optStarted).set_Name("optStarted");
			((Control)optStarted).set_Size(new Size(59, 17));
			((Control)optStarted).set_TabIndex(20);
			((Control)optStarted).set_Text("Started");
			((ButtonBase)optStarted).set_UseVisualStyleBackColor(true);
			((Control)optStarted).add_Click((EventHandler)optStarted_Click);
			((Control)optMiss).set_AutoSize(true);
			((Control)optMiss).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optMiss).set_Location(new Point(147, 143));
			((Control)optMiss).set_Name("optMiss");
			((Control)optMiss).set_Size(new Size(46, 17));
			((Control)optMiss).set_TabIndex(19);
			((Control)optMiss).set_Text("Miss");
			((ButtonBase)optMiss).set_UseVisualStyleBackColor(true);
			((Control)optMiss).add_Click((EventHandler)optMiss_Click);
			((Control)optStopped).set_AutoSize(true);
			((Control)optStopped).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optStopped).set_Location(new Point(74, 162));
			((Control)optStopped).set_Name("optStopped");
			((Control)optStopped).set_Size(new Size(65, 17));
			((Control)optStopped).set_TabIndex(21);
			((Control)optStopped).set_Text("Stopped");
			((ButtonBase)optStopped).set_UseVisualStyleBackColor(true);
			((Control)optStopped).add_Click((EventHandler)optStopped_Click);
			((Control)optFlash).set_AutoSize(true);
			((Control)optFlash).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optFlash).set_Location(new Point(74, 144));
			((Control)optFlash).set_Name("optFlash");
			((Control)optFlash).set_Size(new Size(50, 17));
			((Control)optFlash).set_TabIndex(18);
			((Control)optFlash).set_Text("Flash");
			((ButtonBase)optFlash).set_UseVisualStyleBackColor(true);
			((Control)optFlash).add_Click((EventHandler)optFlash_Click);
			((Control)optBlink).set_AutoSize(true);
			((Control)optBlink).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optBlink).set_Location(new Point(5, 144));
			((Control)optBlink).set_Name("optBlink");
			((Control)optBlink).set_Size(new Size(48, 17));
			((Control)optBlink).set_TabIndex(17);
			((Control)optBlink).set_Text("Blink");
			((ButtonBase)optBlink).set_UseVisualStyleBackColor(true);
			((Control)optBlink).add_Click((EventHandler)optBlink_Click);
			((Control)optDisappear).set_AutoSize(true);
			optDisappear.set_Checked(true);
			((Control)optDisappear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDisappear).set_Location(new Point(5, 126));
			((Control)optDisappear).set_Name("optDisappear");
			((Control)optDisappear).set_Size(new Size(73, 17));
			((Control)optDisappear).set_TabIndex(15);
			optDisappear.set_TabStop(true);
			((Control)optDisappear).set_Text("Disappear");
			((ButtonBase)optDisappear).set_UseVisualStyleBackColor(true);
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(6, 109));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(68, 13));
			((Control)label27).set_TabIndex(14);
			((Control)label27).set_Text("Event type");
			((Control)txtSecond).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSecond).set_Location(new Point(102, 76));
			((Control)txtSecond).set_Name("txtSecond");
			((Control)txtSecond).set_Size(new Size(49, 20));
			((Control)txtSecond).set_TabIndex(13);
			((Control)txtSecond).set_Text("0.0");
			((Control)txtSecond).add_TextChanged((EventHandler)txtSecond_TextChanged);
			((Control)txtSecond).add_MouseUp(new MouseEventHandler(txtSecond_MouseUp));
			((Control)updnMonth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnMonth).set_Location(new Point(58, 36));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(36, 20));
			((Control)updnMonth).set_TabIndex(3);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnMonth.add_ValueChanged((EventHandler)updnMonth_ValueChanged);
			((Control)updnMonth).add_Enter((EventHandler)updnMonth_Enter);
			((Control)updnMonth).add_MouseClick(new MouseEventHandler(updnMonth_MouseClick));
			((Control)updnDay).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnDay).set_Location(new Point(100, 36));
			updnDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			updnDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(36, 20));
			((Control)updnDay).set_TabIndex(5);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnDay.add_ValueChanged((EventHandler)updnDay_ValueChanged);
			((Control)updnDay).add_Enter((EventHandler)updnDay_Enter);
			((Control)updnDay).add_MouseClick(new MouseEventHandler(updnDay_MouseClick));
			((Control)updnHour).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnHour).set_Location(new Point(16, 76));
			updnHour.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(36, 20));
			((Control)updnHour).set_TabIndex(9);
			updnHour.add_ValueChanged((EventHandler)updnHour_ValueChanged);
			((Control)updnHour).add_Enter((EventHandler)updnHour_Enter);
			((Control)updnHour).add_MouseClick(new MouseEventHandler(updnHour_MouseClick));
			((Control)updnMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnMin).set_Location(new Point(58, 76));
			updnMin.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)updnMin).set_Name("updnMin");
			((Control)updnMin).set_Size(new Size(36, 20));
			((Control)updnMin).set_TabIndex(11);
			updnMin.add_ValueChanged((EventHandler)updnMin_ValueChanged);
			((Control)updnMin).add_Enter((EventHandler)updnMin_Enter);
			((Control)updnMin).add_MouseClick(new MouseEventHandler(updnMin_MouseClick));
			((Control)updnYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnYear).set_Location(new Point(6, 36));
			updnYear.set_Maximum(new decimal(new int[4] { 9000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 9000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(49, 20));
			((Control)updnYear).set_TabIndex(1);
			updnYear.set_Value(new decimal(new int[4] { 2008, 0, 0, 0 }));
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((Control)updnYear).add_MouseClick(new MouseEventHandler(updnYear_MouseClick));
			((Control)txtResidual).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtResidual).set_Location(new Point(806, 271));
			((Control)txtResidual).set_Name("txtResidual");
			((Control)txtResidual).set_Size(new Size(54, 20));
			((Control)txtResidual).set_TabIndex(16);
			((Control)txtResidual).set_TabStop(false);
			txtResidual.set_TextAlign((HorizontalAlignment)1);
			((Control)txtPA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtPA).set_Location(new Point(867, 271));
			((Control)txtPA).set_Name("txtPA");
			((Control)txtPA).set_Size(new Size(44, 20));
			((Control)txtPA).set_TabIndex(18);
			((Control)txtPA).set_TabStop(false);
			txtPA.set_TextAlign((HorizontalAlignment)2);
			((Control)txtMag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMag).set_Location(new Point(918, 271));
			((Control)txtMag).set_Name("txtMag");
			((Control)txtMag).set_Size(new Size(33, 20));
			((Control)txtMag).set_TabIndex(20);
			((Control)txtMag).set_TabStop(false);
			txtMag.set_TextAlign((HorizontalAlignment)2);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)withReportToolStripMenuItem,
				(ToolStripItem)doubleStarReportToolStripMenuItem,
				(ToolStripItem)lightCurveReportToolStripMenuItem,
				(ToolStripItem)displayOnGoogleEarthToolStripMenuItem,
				(ToolStripItem)liMovieToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1016, 24));
			((Control)menuStrip1).set_TabIndex(6);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[25]
			{
				(ToolStripItem)newToolStripMenuItem,
				(ToolStripItem)toolStripMenuItem2,
				(ToolStripItem)openToolStripMenuItem,
				(ToolStripItem)importToolStripMenuItem,
				(ToolStripItem)toolStripMenuItem1,
				(ToolStripItem)pasteToolStripMenuItem,
				(ToolStripItem)pasteMergeToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyReportToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveAsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)cameraCorrectionsToolStripMenuItem,
				(ToolStripItem)doAllToolStripMenuItem,
				(ToolStripItem)identifyUnidentifiedStarsInReportToolStripMenuItem,
				(ToolStripItem)listPossibleDoubleStarsToolStripMenuItem,
				(ToolStripItem)verifyAccuracyValuesToolStripMenuItem,
				(ToolStripItem)checkTemperatureValuesToolStripMenuItem,
				(ToolStripItem)verifyDataEntryToolStripMenuItem,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)saveoldFormatToolStripMenuItem,
				(ToolStripItem)saveAsoldFormatToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)addGrazeToRecentGrazesFileToolStripMenuItem,
				(ToolStripItem)addGrazeToMainFileOfGrazesToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(46, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...");
			((ToolStripItem)newToolStripMenuItem).set_Image((Image)Resources.NewDocumentHS);
			((ToolStripItem)newToolStripMenuItem).set_Name("newToolStripMenuItem");
			newToolStripMenuItem.set_ShortcutKeys((Keys)131150);
			((ToolStripItem)newToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)newToolStripMenuItem).set_Text("New");
			((ToolStripItem)newToolStripMenuItem).add_Click((EventHandler)newToolStripMenuItem_Click);
			((ToolStripItem)toolStripMenuItem2).set_Name("toolStripMenuItem2");
			((ToolStripItem)toolStripMenuItem2).set_Size(new Size(256, 6));
			((ToolStripItem)openToolStripMenuItem).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)openToolStripMenuItem).set_Name("openToolStripMenuItem");
			openToolStripMenuItem.set_ShortcutKeys((Keys)131151);
			((ToolStripItem)openToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)openToolStripMenuItem).set_Text("Open");
			((ToolStripItem)openToolStripMenuItem).add_Click((EventHandler)openToolStripMenuItem_Click);
			((ToolStripItem)importToolStripMenuItem).set_Image((Image)Resources.ImportXMLHS);
			((ToolStripItem)importToolStripMenuItem).set_Name("importToolStripMenuItem");
			importToolStripMenuItem.set_ShortcutKeys((Keys)131145);
			((ToolStripItem)importToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)importToolStripMenuItem).set_Text("Import && merge");
			((ToolStripItem)importToolStripMenuItem).add_Click((EventHandler)importToolStripMenuItem_Click);
			((ToolStripItem)toolStripMenuItem1).set_Name("toolStripMenuItem1");
			((ToolStripItem)toolStripMenuItem1).set_Size(new Size(256, 6));
			((ToolStripItem)pasteToolStripMenuItem).set_Image((Image)Resources.PasteHS);
			((ToolStripItem)pasteToolStripMenuItem).set_Name("pasteToolStripMenuItem");
			((ToolStripItem)pasteToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)pasteToolStripMenuItem).set_Text("Paste");
			((ToolStripItem)pasteToolStripMenuItem).add_Click((EventHandler)pasteToolStripMenuItem_Click);
			((ToolStripItem)pasteMergeToolStripMenuItem).set_Name("pasteMergeToolStripMenuItem");
			pasteMergeToolStripMenuItem.set_ShortcutKeys((Keys)131159);
			((ToolStripItem)pasteMergeToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)pasteMergeToolStripMenuItem).set_Text("Paste && merge");
			((ToolStripItem)pasteMergeToolStripMenuItem).add_Click((EventHandler)pasteMergeToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(256, 6));
			((ToolStripItem)copyReportToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyReportToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyReportToolStripMenuItem).set_Name("copyReportToolStripMenuItem");
			copyReportToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyReportToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)copyReportToolStripMenuItem).set_Text("Copy entire report");
			((ToolStripItem)copyReportToolStripMenuItem).add_Click((EventHandler)copyReportToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)saveAsToolStripMenuItem).set_Image((Image)Resources.SaveAsWebPageHS);
			((ToolStripItem)saveAsToolStripMenuItem).set_Name("saveAsToolStripMenuItem");
			((ToolStripItem)saveAsToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)saveAsToolStripMenuItem).set_Text("Save as...");
			((ToolStripItem)saveAsToolStripMenuItem).add_Click((EventHandler)saveAsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(256, 6));
			((ToolStripItem)cameraCorrectionsToolStripMenuItem).set_Name("cameraCorrectionsToolStripMenuItem");
			((ToolStripItem)cameraCorrectionsToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)cameraCorrectionsToolStripMenuItem).set_Text("Camera corrections");
			((ToolStripItem)cameraCorrectionsToolStripMenuItem).add_Click((EventHandler)cameraCorrectionsToolStripMenuItem_Click);
			((ToolStripItem)doAllToolStripMenuItem).set_Name("doAllToolStripMenuItem");
			((ToolStripItem)doAllToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)doAllToolStripMenuItem).set_Text("Conduct 5 data validity checks");
			((ToolStripItem)doAllToolStripMenuItem).add_Click((EventHandler)doAllToolStripMenuItem_Click);
			((ToolStripItem)identifyUnidentifiedStarsInReportToolStripMenuItem).set_Name("identifyUnidentifiedStarsInReportToolStripMenuItem");
			identifyUnidentifiedStarsInReportToolStripMenuItem.set_ShortcutKeys((Keys)131157);
			((ToolStripItem)identifyUnidentifiedStarsInReportToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)identifyUnidentifiedStarsInReportToolStripMenuItem).set_Text("Identify unidentified stars");
			((ToolStripItem)identifyUnidentifiedStarsInReportToolStripMenuItem).set_Visible(false);
			((ToolStripItem)identifyUnidentifiedStarsInReportToolStripMenuItem).add_Click((EventHandler)identifyUnidentifiedStarsInReportToolStripMenuItem_Click);
			((ToolStripItem)listPossibleDoubleStarsToolStripMenuItem).set_Name("listPossibleDoubleStarsToolStripMenuItem");
			listPossibleDoubleStarsToolStripMenuItem.set_ShortcutKeys((Keys)131140);
			((ToolStripItem)listPossibleDoubleStarsToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)listPossibleDoubleStarsToolStripMenuItem).set_Text("list possible Double stars");
			((ToolStripItem)listPossibleDoubleStarsToolStripMenuItem).set_Visible(false);
			((ToolStripItem)listPossibleDoubleStarsToolStripMenuItem).add_Click((EventHandler)listPossibleDoubleStarsToolStripMenuItem_Click);
			((ToolStripItem)verifyAccuracyValuesToolStripMenuItem).set_Name("verifyAccuracyValuesToolStripMenuItem");
			((ToolStripItem)verifyAccuracyValuesToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)verifyAccuracyValuesToolStripMenuItem).set_Text("check Accuracy values > 1 sec");
			((ToolStripItem)verifyAccuracyValuesToolStripMenuItem).set_Visible(false);
			((ToolStripItem)verifyAccuracyValuesToolStripMenuItem).add_Click((EventHandler)verifyAccuracyValuesToolStripMenuItem_Click);
			((ToolStripItem)checkTemperatureValuesToolStripMenuItem).set_Name("checkTemperatureValuesToolStripMenuItem");
			((ToolStripItem)checkTemperatureValuesToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)checkTemperatureValuesToolStripMenuItem).set_Text("check Temperature values > 60 deg");
			((ToolStripItem)checkTemperatureValuesToolStripMenuItem).set_Visible(false);
			((ToolStripItem)checkTemperatureValuesToolStripMenuItem).add_Click((EventHandler)checkTemperatureValuesToolStripMenuItem_Click);
			((ToolStripItem)verifyDataEntryToolStripMenuItem).set_Name("verifyDataEntryToolStripMenuItem");
			((ToolStripItem)verifyDataEntryToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)verifyDataEntryToolStripMenuItem).set_Text("Verify data entry");
			((ToolStripItem)verifyDataEntryToolStripMenuItem).set_Visible(false);
			((ToolStripItem)verifyDataEntryToolStripMenuItem).add_Click((EventHandler)verifyDataEntryToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(256, 6));
			((ToolStripItem)saveoldFormatToolStripMenuItem).set_Font(new Font("Segoe UI", 6.5f, FontStyle.Italic));
			((ToolStripItem)saveoldFormatToolStripMenuItem).set_Name("saveoldFormatToolStripMenuItem");
			((ToolStripItem)saveoldFormatToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)saveoldFormatToolStripMenuItem).set_Text("Save                    [old format]");
			((ToolStripItem)saveoldFormatToolStripMenuItem).add_Click((EventHandler)saveoldFormatToolStripMenuItem_Click);
			((ToolStripItem)saveAsoldFormatToolStripMenuItem).set_Font(new Font("Segoe UI", 6.5f, FontStyle.Italic));
			((ToolStripItem)saveAsoldFormatToolStripMenuItem).set_Name("saveAsoldFormatToolStripMenuItem");
			((ToolStripItem)saveAsoldFormatToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)saveAsoldFormatToolStripMenuItem).set_Text("Save as...           [old format]");
			((ToolStripItem)saveAsoldFormatToolStripMenuItem).add_Click((EventHandler)saveAsoldFormatToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(256, 6));
			((ToolStripItem)toolStripSeparator1).set_Visible(false);
			((ToolStripItem)addGrazeToRecentGrazesFileToolStripMenuItem).set_Font(new Font("Segoe UI", 8f, FontStyle.Italic));
			((ToolStripItem)addGrazeToRecentGrazesFileToolStripMenuItem).set_Name("addGrazeToRecentGrazesFileToolStripMenuItem");
			((ToolStripItem)addGrazeToRecentGrazesFileToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)addGrazeToRecentGrazesFileToolStripMenuItem).set_Text("Add graze to file of recent Grazes");
			((ToolStripItem)addGrazeToRecentGrazesFileToolStripMenuItem).set_Visible(false);
			((ToolStripItem)addGrazeToRecentGrazesFileToolStripMenuItem).add_Click((EventHandler)addGrazeToRecentGrazesFileToolStripMenuItem_Click);
			((ToolStripItem)addGrazeToMainFileOfGrazesToolStripMenuItem).set_Font(new Font("Segoe UI", 8f, FontStyle.Italic));
			((ToolStripItem)addGrazeToMainFileOfGrazesToolStripMenuItem).set_Name("addGrazeToMainFileOfGrazesToolStripMenuItem");
			((ToolStripItem)addGrazeToMainFileOfGrazesToolStripMenuItem).set_Size(new Size(259, 22));
			((ToolStripItem)addGrazeToMainFileOfGrazesToolStripMenuItem).set_Text("Add graze to Main file of Grazes");
			((ToolStripItem)addGrazeToMainFileOfGrazesToolStripMenuItem).set_Visible(false);
			((ToolStripItem)addGrazeToMainFileOfGrazesToolStripMenuItem).add_Click((EventHandler)addGrazeToMainFileOfGrazesToolStripMenuItem_Click);
			((ToolStripDropDownItem)withReportToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)submitToolStripMenuItem,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)downloadCurrentEmailAddressesToolStripMenuItem
			});
			((ToolStripItem)withReportToolStripMenuItem).set_Image((Image)Resources.mail);
			((ToolStripItem)withReportToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)withReportToolStripMenuItem).set_Name("withReportToolStripMenuItem");
			((ToolStripItem)withReportToolStripMenuItem).set_Size(new Size(150, 20));
			((ToolStripItem)withReportToolStripMenuItem).set_Text("Submit report...           ");
			((ToolStripItem)submitToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold));
			((ToolStripItem)submitToolStripMenuItem).set_Name("submitToolStripMenuItem");
			((ToolStripItem)submitToolStripMenuItem).set_Size(new Size(307, 24));
			((ToolStripItem)submitToolStripMenuItem).set_Text("Submit report");
			((ToolStripItem)submitToolStripMenuItem).add_Click((EventHandler)submitToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(304, 6));
			((ToolStripItem)downloadCurrentEmailAddressesToolStripMenuItem).set_Name("downloadCurrentEmailAddressesToolStripMenuItem");
			((ToolStripItem)downloadCurrentEmailAddressesToolStripMenuItem).set_Size(new Size(307, 24));
			((ToolStripItem)downloadCurrentEmailAddressesToolStripMenuItem).set_Text("Download current Email reporting addresses");
			((ToolStripItem)downloadCurrentEmailAddressesToolStripMenuItem).add_Click((EventHandler)downloadCurrentEmailAddressesToolStripMenuItem_Click);
			((ToolStripDropDownItem)doubleStarReportToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)createReportForSelectedStarToolStripMenuItem,
				(ToolStripItem)selectTheBrighterComponentToolStripMenuItem
			});
			((ToolStripItem)doubleStarReportToolStripMenuItem).set_Name("doubleStarReportToolStripMenuItem");
			((ToolStripItem)doubleStarReportToolStripMenuItem).set_Size(new Size(144, 20));
			((ToolStripItem)doubleStarReportToolStripMenuItem).set_Text("Double star report...       ");
			((ToolStripItem)createReportForSelectedStarToolStripMenuItem).set_Name("createReportForSelectedStarToolStripMenuItem");
			((ToolStripItem)createReportForSelectedStarToolStripMenuItem).set_Size(new Size(342, 22));
			((ToolStripItem)createReportForSelectedStarToolStripMenuItem).set_Text("Create report for current star (brighter component)");
			((ToolStripItem)createReportForSelectedStarToolStripMenuItem).add_Click((EventHandler)createReportForSelectedStarToolStripMenuItem_Click);
			((ToolStripItem)selectTheBrighterComponentToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)selectTheBrighterComponentToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold | FontStyle.Italic));
			((ToolStripItem)selectTheBrighterComponentToolStripMenuItem).set_Name("selectTheBrighterComponentToolStripMenuItem");
			((ToolStripItem)selectTheBrighterComponentToolStripMenuItem).set_Size(new Size(342, 22));
			((ToolStripItem)selectTheBrighterComponentToolStripMenuItem).set_Text("(You must select the brighter component)");
			((ToolStripDropDownItem)lightCurveReportToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)createLightCurveReportToolStripMenuItem,
				(ToolStripItem)emailLightCurveReportsToolStripMenuItem
			});
			((ToolStripItem)lightCurveReportToolStripMenuItem).set_Name("lightCurveReportToolStripMenuItem");
			((ToolStripItem)lightCurveReportToolStripMenuItem).set_Size(new Size(122, 20));
			((ToolStripItem)lightCurveReportToolStripMenuItem).set_Text("Light curve report...");
			((ToolStripItem)createLightCurveReportToolStripMenuItem).set_Name("createLightCurveReportToolStripMenuItem");
			((ToolStripItem)createLightCurveReportToolStripMenuItem).set_Size(new Size(202, 22));
			((ToolStripItem)createLightCurveReportToolStripMenuItem).set_Text("Create light curve report");
			((ToolStripItem)createLightCurveReportToolStripMenuItem).add_Click((EventHandler)createLightCurveReportToolStripMenuItem_Click);
			((ToolStripItem)emailLightCurveReportsToolStripMenuItem).set_Name("emailLightCurveReportsToolStripMenuItem");
			((ToolStripItem)emailLightCurveReportsToolStripMenuItem).set_Size(new Size(202, 22));
			((ToolStripItem)emailLightCurveReportsToolStripMenuItem).set_Text("Email light curve reports");
			((ToolStripItem)emailLightCurveReportsToolStripMenuItem).add_Click((EventHandler)emailLightCurveReportsToolStripMenuItem_Click);
			((ToolStripDropDownItem)displayOnGoogleEarthToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[11]
			{
				(ToolStripItem)displayCurrentSitesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator6,
				(ToolStripItem)plotMoonFirst10EventsToolStripMenuItem,
				(ToolStripItem)plotMoonFirst20EventsToolStripMenuItem,
				(ToolStripItem)plotMoonFirst50EventsToolStripMenuItem,
				(ToolStripItem)plotMoonAllEventsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator7,
				(ToolStripItem)includeLimbProfile,
				(ToolStripItem)altitudemToUseInLimbPlotsToolStripMenuItem,
				(ToolStripItem)mnuGoogleHeight,
				(ToolStripItem)toolStripSeparator8
			});
			((ToolStripItem)displayOnGoogleEarthToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)displayOnGoogleEarthToolStripMenuItem).set_Name("displayOnGoogleEarthToolStripMenuItem");
			displayOnGoogleEarthToolStripMenuItem.set_ShortcutKeys((Keys)131143);
			((ToolStripItem)displayOnGoogleEarthToolStripMenuItem).set_Size(new Size(127, 20));
			((ToolStripItem)displayOnGoogleEarthToolStripMenuItem).set_Text("&GoogleEarth         ");
			((ToolStripItem)displayCurrentSitesToolStripMenuItem).set_Name("displayCurrentSitesToolStripMenuItem");
			((ToolStripItem)displayCurrentSitesToolStripMenuItem).set_Size(new Size(242, 22));
			((ToolStripItem)displayCurrentSitesToolStripMenuItem).set_Text("plot current Sites");
			((ToolStripItem)displayCurrentSitesToolStripMenuItem).add_Click((EventHandler)displayCurrentSitesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator6).set_Name("toolStripSeparator6");
			((ToolStripItem)toolStripSeparator6).set_Size(new Size(239, 6));
			((ToolStripItem)plotMoonFirst10EventsToolStripMenuItem).set_Name("plotMoonFirst10EventsToolStripMenuItem");
			((ToolStripItem)plotMoonFirst10EventsToolStripMenuItem).set_Size(new Size(242, 22));
			((ToolStripItem)plotMoonFirst10EventsToolStripMenuItem).set_Text("plot Moon limb - first 10 events");
			((ToolStripItem)plotMoonFirst10EventsToolStripMenuItem).add_Click((EventHandler)plotMoonFirst10EventsToolStripMenuItem_Click);
			((ToolStripItem)plotMoonFirst20EventsToolStripMenuItem).set_Name("plotMoonFirst20EventsToolStripMenuItem");
			((ToolStripItem)plotMoonFirst20EventsToolStripMenuItem).set_Size(new Size(242, 22));
			((ToolStripItem)plotMoonFirst20EventsToolStripMenuItem).set_Text("plot Moon limb - first 20 events");
			((ToolStripItem)plotMoonFirst20EventsToolStripMenuItem).add_Click((EventHandler)plotMoonFirst20EventsToolStripMenuItem_Click);
			((ToolStripItem)plotMoonFirst50EventsToolStripMenuItem).set_Name("plotMoonFirst50EventsToolStripMenuItem");
			((ToolStripItem)plotMoonFirst50EventsToolStripMenuItem).set_Size(new Size(242, 22));
			((ToolStripItem)plotMoonFirst50EventsToolStripMenuItem).set_Text("plot Moon limb - first 50 events");
			((ToolStripItem)plotMoonFirst50EventsToolStripMenuItem).add_Click((EventHandler)plotMoonFirst50EventsToolStripMenuItem_Click);
			((ToolStripItem)plotMoonAllEventsToolStripMenuItem).set_Name("plotMoonAllEventsToolStripMenuItem");
			((ToolStripItem)plotMoonAllEventsToolStripMenuItem).set_Size(new Size(242, 22));
			((ToolStripItem)plotMoonAllEventsToolStripMenuItem).set_Text("plot Moon limb - all events");
			((ToolStripItem)plotMoonAllEventsToolStripMenuItem).add_Click((EventHandler)plotMoonAllEventsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator7).set_Name("toolStripSeparator7");
			((ToolStripItem)toolStripSeparator7).set_Size(new Size(239, 6));
			includeLimbProfile.set_Checked(true);
			includeLimbProfile.set_CheckOnClick(true);
			includeLimbProfile.set_CheckState((CheckState)1);
			((ToolStripItem)includeLimbProfile).set_Name("includeLimbProfile");
			((ToolStripItem)includeLimbProfile).set_Size(new Size(242, 22));
			((ToolStripItem)includeLimbProfile).set_Text("Include LOLA limb profile");
			((ToolStripItem)altitudemToUseInLimbPlotsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)altitudemToUseInLimbPlotsToolStripMenuItem).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)altitudemToUseInLimbPlotsToolStripMenuItem).set_Name("altitudemToUseInLimbPlotsToolStripMenuItem");
			((ToolStripItem)altitudemToUseInLimbPlotsToolStripMenuItem).set_Size(new Size(242, 22));
			((ToolStripItem)altitudemToUseInLimbPlotsToolStripMenuItem).set_Text("Altitude (m) to use in limb plots");
			((ToolStripItem)mnuGoogleHeight).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)mnuGoogleHeight).set_Name("mnuGoogleHeight");
			((ToolStripItem)mnuGoogleHeight).set_Size(new Size(100, 23));
			((ToolStripItem)mnuGoogleHeight).set_Text(Settings.Default.GoogleEarthHeightForLunarLimb);
			((ToolStripItem)toolStripSeparator8).set_Name("toolStripSeparator8");
			((ToolStripItem)toolStripSeparator8).set_Size(new Size(239, 6));
			((ToolStripDropDownItem)liMovieToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)saveGraphicFromClipboardToolStripMenuItem });
			((ToolStripItem)liMovieToolStripMenuItem).set_Name("liMovieToolStripMenuItem");
			((ToolStripItem)liMovieToolStripMenuItem).set_Size(new Size(79, 20));
			((ToolStripItem)liMovieToolStripMenuItem).set_Text("LiMovie...   ");
			((ToolStripItem)saveGraphicFromClipboardToolStripMenuItem).set_Name("saveGraphicFromClipboardToolStripMenuItem");
			((ToolStripItem)saveGraphicFromClipboardToolStripMenuItem).set_Size(new Size(246, 22));
			((ToolStripItem)saveGraphicFromClipboardToolStripMenuItem).set_Text("Save graphic pasted to clipboard");
			((ToolStripItem)saveGraphicFromClipboardToolStripMenuItem).add_Click((EventHandler)saveGraphicFromClipboardToolStripMenuItem_Click);
			((ToolStripDropDownItem)helpToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)generalHelpToolStripMenuItem,
				(ToolStripItem)fresnelDiffractionToolStripMenuItem
			});
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(135, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help                         ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)generalHelpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)generalHelpToolStripMenuItem).set_Name("generalHelpToolStripMenuItem");
			((ToolStripItem)generalHelpToolStripMenuItem).set_Size(new Size(169, 22));
			((ToolStripItem)generalHelpToolStripMenuItem).set_Text("on Editor");
			((ToolStripItem)generalHelpToolStripMenuItem).add_Click((EventHandler)generalHelpToolStripMenuItem_Click);
			((ToolStripItem)fresnelDiffractionToolStripMenuItem).set_Image((Image)Resources.Fresnel);
			((ToolStripItem)fresnelDiffractionToolStripMenuItem).set_Name("fresnelDiffractionToolStripMenuItem");
			((ToolStripItem)fresnelDiffractionToolStripMenuItem).set_Size(new Size(169, 22));
			((ToolStripItem)fresnelDiffractionToolStripMenuItem).set_Text("Fresnel diffraction");
			((ToolStripItem)fresnelDiffractionToolStripMenuItem).add_Click((EventHandler)fresnelDiffractionToolStripMenuItem_Click);
			((Control)panel3).set_Anchor((AnchorStyles)1);
			((Control)panel3).get_Controls().Add((Control)(object)lblLimbBasis);
			((Control)panel3).get_Controls().Add((Control)(object)chkFormat);
			((Control)panel3).get_Controls().Add((Control)(object)cmdReduce);
			((Control)panel3).get_Controls().Add((Control)(object)label10);
			((Control)panel3).get_Controls().Add((Control)(object)optView);
			((Control)panel3).get_Controls().Add((Control)(object)optHeader);
			((Control)panel3).get_Controls().Add((Control)(object)label8);
			((Control)panel3).get_Controls().Add((Control)(object)optEvents);
			((Control)panel3).get_Controls().Add((Control)(object)optSites);
			((Control)panel3).set_Location(new Point(6, 29));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(1001, 34));
			((Control)panel3).set_TabIndex(0);
			((Control)lblLimbBasis).set_AutoSize(true);
			((Control)lblLimbBasis).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblLimbBasis).set_Location(new Point(881, 9));
			((Control)lblLimbBasis).set_Name("lblLimbBasis");
			((Control)lblLimbBasis).set_Size(new Size(78, 17));
			((Control)lblLimbBasis).set_TabIndex(9);
			((Control)lblLimbBasis).set_Text("LRO-LOLA");
			((Control)chkFormat).set_AutoSize(true);
			chkFormat.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkFormat).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkFormat).set_Location(new Point(8, 3));
			((Control)chkFormat).set_Name("chkFormat");
			((Control)chkFormat).set_Size(new Size(65, 28));
			((Control)chkFormat).set_TabIndex(8);
			((Control)chkFormat).set_Text("Display in\r\nold format");
			((ButtonBase)chkFormat).set_UseVisualStyleBackColor(true);
			chkFormat.add_CheckedChanged((EventHandler)chkFormat_CheckedChanged);
			((Control)cmdReduce).set_Font(new Font("Microsoft Sans Serif", 11f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReduce).set_Location(new Point(717, 3));
			((Control)cmdReduce).set_Name("cmdReduce");
			((Control)cmdReduce).set_Size(new Size(130, 28));
			((Control)cmdReduce).set_TabIndex(6);
			((Control)cmdReduce).set_Text("Reduce && Plot");
			((ButtonBase)cmdReduce).set_UseVisualStyleBackColor(true);
			((Control)cmdReduce).add_Click((EventHandler)cmdReduce_Click);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 14.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(108, 5));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(67, 24));
			((Control)label10).set_TabIndex(0);
			((Control)label10).set_Text("View : ");
			((Control)optView).set_AutoSize(true);
			optView.set_Checked(true);
			((Control)optView).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optView).set_Location(new Point(181, 7));
			((Control)optView).set_Name("optView");
			((Control)optView).set_Size(new Size(76, 24));
			((Control)optView).set_TabIndex(1);
			optView.set_TabStop(true);
			((Control)optView).set_Text("Report");
			((ButtonBase)optView).set_UseVisualStyleBackColor(true);
			((Control)optHeader).set_AutoSize(true);
			((Control)optHeader).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optHeader).set_Location(new Point(352, 7));
			((Control)optHeader).set_Name("optHeader");
			((Control)optHeader).set_Size(new Size(80, 24));
			((Control)optHeader).set_TabIndex(3);
			((Control)optHeader).set_Text("Header");
			((ButtonBase)optHeader).set_UseVisualStyleBackColor(true);
			optHeader.add_CheckedChanged((EventHandler)optHeader_CheckedChanged);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 14.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(291, 5));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(57, 24));
			((Control)label8).set_TabIndex(2);
			((Control)label8).set_Text("Edit : ");
			((Control)optEvents).set_AutoSize(true);
			((Control)optEvents).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optEvents).set_Location(new Point(596, 7));
			((Control)optEvents).set_Name("optEvents");
			((Control)optEvents).set_Size(new Size(76, 24));
			((Control)optEvents).set_TabIndex(5);
			((Control)optEvents).set_Text("Events");
			((ButtonBase)optEvents).set_UseVisualStyleBackColor(true);
			optEvents.add_CheckedChanged((EventHandler)optEvents_CheckedChanged);
			((Control)optSites).set_AutoSize(true);
			((Control)optSites).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSites).set_Location(new Point(448, 7));
			((Control)optSites).set_Name("optSites");
			((Control)optSites).set_Size(new Size(132, 24));
			((Control)optSites).set_TabIndex(4);
			((Control)optSites).set_Text("Sites && Names");
			((ButtonBase)optSites).set_UseVisualStyleBackColor(true);
			optSites.add_CheckedChanged((EventHandler)optSites_CheckedChanged);
			((Control)grpHeader).set_Anchor((AnchorStyles)1);
			((Control)grpHeader).get_Controls().Add((Control)(object)label79);
			((Control)grpHeader).get_Controls().Add((Control)(object)label78);
			((Control)grpHeader).get_Controls().Add((Control)(object)label76);
			((Control)grpHeader).get_Controls().Add((Control)(object)label73);
			((Control)grpHeader).get_Controls().Add((Control)(object)txtMessage);
			((Control)grpHeader).get_Controls().Add((Control)(object)label75);
			((Control)grpHeader).get_Controls().Add((Control)(object)label74);
			((Control)grpHeader).get_Controls().Add((Control)(object)label62);
			((Control)grpHeader).get_Controls().Add((Control)(object)lblCharsLeft);
			((Control)grpHeader).get_Controls().Add((Control)(object)label68);
			((Control)grpHeader).get_Controls().Add((Control)(object)panelRepresentative);
			((Control)grpHeader).get_Controls().Add((Control)(object)panelReportedTo);
			((Control)grpHeader).get_Controls().Add((Control)(object)panelAddress);
			((Control)grpHeader).get_Controls().Add((Control)(object)label22);
			((Control)grpHeader).get_Controls().Add((Control)(object)label21);
			((Control)grpHeader).get_Controls().Add((Control)(object)cmdSetPlace);
			((Control)grpHeader).get_Controls().Add((Control)(object)cmdGetEMail);
			((Control)grpHeader).get_Controls().Add((Control)(object)cmdSetEmail);
			((Control)grpHeader).get_Controls().Add((Control)(object)cmdGetPlace);
			((Control)grpHeader).get_Controls().Add((Control)(object)txtEmail);
			((Control)grpHeader).get_Controls().Add((Control)(object)txtPlace);
			((Control)grpHeader).get_Controls().Add((Control)(object)label13);
			((Control)grpHeader).get_Controls().Add((Control)(object)label9);
			((Control)grpHeader).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpHeader).set_Location(new Point(5, 402));
			((Control)grpHeader).set_Name("grpHeader");
			((Control)grpHeader).set_Size(new Size(738, 447));
			((Control)grpHeader).set_TabIndex(1);
			grpHeader.set_TabStop(false);
			((Control)grpHeader).set_Text("Header");
			((Control)grpHeader).add_Leave((EventHandler)grpHeader_Leave);
			((Control)label76).set_AutoSize(true);
			((Control)label76).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label76).set_Location(new Point(70, 409));
			((Control)label76).set_Name("label76");
			((Control)label76).set_Size(new Size(163, 13));
			((Control)label76).set_TabIndex(32);
			((Control)label76).set_Text("Note:  empty lines will be deleted.");
			((Control)label73).set_AutoSize(true);
			((Control)label73).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label73).set_Location(new Point(70, 267));
			((Control)label73).set_Name("label73");
			((Control)label73).set_Size(new Size(528, 26));
			((Control)label73).set_TabIndex(31);
			((Control)label73).set_Text(componentResourceManager.GetString("label73.Text"));
			((Control)txtMessage).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMessage).set_Location(new Point(70, 294));
			((TextBoxBase)txtMessage).set_Multiline(true);
			((Control)txtMessage).set_Name("txtMessage");
			txtMessage.set_ScrollBars((ScrollBars)2);
			((Control)txtMessage).set_Size(new Size(599, 113));
			((Control)txtMessage).set_TabIndex(30);
			((Control)txtMessage).add_TextChanged((EventHandler)txtMessage_TextChanged);
			((Control)txtMessage).add_Leave((EventHandler)txtMessage_Leave);
			((Control)label75).set_AutoSize(true);
			((Control)label75).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label75).set_Location(new Point(129, 52));
			((Control)label75).set_Name("label75");
			((Control)label75).set_Size(new Size(247, 13));
			((Control)label75).set_TabIndex(29);
			((Control)label75).set_Text("Name of nearby city, town or landmark, plus country");
			((Control)label74).set_AutoSize(true);
			((Control)label74).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label74).set_Location(new Point(553, 52));
			((Control)label74).set_Name("label74");
			((Control)label74).set_Size(new Size(52, 13));
			((Control)label74).set_TabIndex(28);
			((Control)label74).set_Text("remaining");
			((Control)label62).set_AutoSize(true);
			((Control)label62).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label62).set_Location(new Point(630, 48));
			((Control)label62).set_Name("label62");
			((Control)label62).set_Size(new Size(75, 13));
			((Control)label62).set_TabIndex(27);
			((Control)label62).set_Text("view in ToolTip");
			((Control)lblCharsLeft).set_AutoSize(true);
			((Control)lblCharsLeft).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCharsLeft).set_Location(new Point(534, 52));
			((Control)lblCharsLeft).set_Name("lblCharsLeft");
			((Control)lblCharsLeft).set_Size(new Size(19, 13));
			((Control)lblCharsLeft).set_TabIndex(26);
			((Control)lblCharsLeft).set_Text("50");
			((Control)label68).set_AutoSize(true);
			((Control)label68).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label68).set_Location(new Point(420, 52));
			((Control)label68).set_Name("label68");
			((Control)label68).set_Size(new Size(115, 13));
			((Control)label68).set_TabIndex(25);
			((Control)label68).set_Text("Limit of 50 characters : ");
			((Control)panelRepresentative).get_Controls().Add((Control)(object)cmdGetRep);
			((Control)panelRepresentative).get_Controls().Add((Control)(object)cmdSetRep);
			((Control)panelRepresentative).get_Controls().Add((Control)(object)txtRepresentative);
			((Control)panelRepresentative).get_Controls().Add((Control)(object)label12);
			((Control)panelRepresentative).set_Location(new Point(40, 166));
			((Control)panelRepresentative).set_Name("panelRepresentative");
			((Control)panelRepresentative).set_Size(new Size(676, 31));
			((Control)panelRepresentative).set_TabIndex(24);
			((Control)cmdGetRep).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetRep).set_Location(new Point(587, 8));
			((Control)cmdGetRep).set_Name("cmdGetRep");
			((Control)cmdGetRep).set_Size(new Size(20, 20));
			((Control)cmdGetRep).set_TabIndex(18);
			((ButtonBase)cmdGetRep).set_UseVisualStyleBackColor(true);
			((Control)cmdGetRep).add_Click((EventHandler)cmdGetRep_Click);
			((Control)cmdSetRep).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetRep).set_Location(new Point(647, 8));
			((Control)cmdSetRep).set_Name("cmdSetRep");
			((Control)cmdSetRep).set_Size(new Size(20, 20));
			((Control)cmdSetRep).set_TabIndex(19);
			((ButtonBase)cmdSetRep).set_UseVisualStyleBackColor(true);
			((Control)cmdSetRep).add_Click((EventHandler)cmdSetRep_Click);
			((Control)txtRepresentative).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRepresentative).set_Location(new Point(92, 8));
			((Control)txtRepresentative).set_Name("txtRepresentative");
			((Control)txtRepresentative).set_Size(new Size(468, 20));
			((Control)txtRepresentative).set_TabIndex(7);
			((Control)txtRepresentative).set_Text("My Name");
			toolTip.SetToolTip((Control)(object)txtRepresentative, "Insert the name of the person submitting the report");
			((Control)txtRepresentative).add_TextChanged((EventHandler)txtRepresentative_TextChanged);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(11, 12));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(79, 13));
			((Control)label12).set_TabIndex(6);
			((Control)label12).set_Text("Representative");
			((Control)panelReportedTo).get_Controls().Add((Control)(object)cmdGetReported);
			((Control)panelReportedTo).get_Controls().Add((Control)(object)cmdSetReported);
			((Control)panelReportedTo).get_Controls().Add((Control)(object)txtReported);
			((Control)panelReportedTo).get_Controls().Add((Control)(object)label11);
			((Control)panelReportedTo).set_Location(new Point(39, 200));
			((Control)panelReportedTo).set_Name("panelReportedTo");
			((Control)panelReportedTo).set_Size(new Size(677, 42));
			((Control)panelReportedTo).set_TabIndex(23);
			((Control)cmdGetReported).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetReported).set_Location(new Point(588, 10));
			((Control)cmdGetReported).set_Name("cmdGetReported");
			((Control)cmdGetReported).set_Size(new Size(20, 20));
			((Control)cmdGetReported).set_TabIndex(20);
			((ButtonBase)cmdGetReported).set_UseVisualStyleBackColor(true);
			((Control)cmdGetReported).add_Click((EventHandler)cmdGetReported_Click);
			((Control)cmdSetReported).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetReported).set_Location(new Point(648, 10));
			((Control)cmdSetReported).set_Name("cmdSetReported");
			((Control)cmdSetReported).set_Size(new Size(20, 20));
			((Control)cmdSetReported).set_TabIndex(21);
			((ButtonBase)cmdSetReported).set_UseVisualStyleBackColor(true);
			((Control)cmdSetReported).add_Click((EventHandler)cmdSetReported_Click);
			((Control)txtReported).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtReported).set_Location(new Point(93, 10));
			((Control)txtReported).set_Name("txtReported");
			((Control)txtReported).set_Size(new Size(468, 20));
			((Control)txtReported).set_TabIndex(9);
			toolTip.SetToolTip((Control)(object)txtReported, "This field is no longer used");
			((Control)txtReported).add_TextChanged((EventHandler)txtReported_TextChanged);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(12, 14));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(63, 13));
			((Control)label11).set_TabIndex(8);
			((Control)label11).set_Text("Reported to");
			((Control)panelAddress).get_Controls().Add((Control)(object)cmdGetAddress);
			((Control)panelAddress).get_Controls().Add((Control)(object)cmdSetAddress);
			((Control)panelAddress).get_Controls().Add((Control)(object)txtAddress);
			((Control)panelAddress).get_Controls().Add((Control)(object)label14);
			((Control)panelAddress).set_Location(new Point(40, 94));
			((Control)panelAddress).set_Name("panelAddress");
			((Control)panelAddress).set_Size(new Size(676, 36));
			((Control)panelAddress).set_TabIndex(22);
			((Control)cmdGetAddress).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetAddress).set_Location(new Point(587, 8));
			((Control)cmdGetAddress).set_Name("cmdGetAddress");
			((Control)cmdGetAddress).set_Size(new Size(20, 20));
			((Control)cmdGetAddress).set_TabIndex(14);
			((ButtonBase)cmdGetAddress).set_UseVisualStyleBackColor(true);
			((Control)cmdGetAddress).add_Click((EventHandler)cmdGetAddress_Click);
			((Control)cmdSetAddress).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetAddress).set_Location(new Point(647, 8));
			((Control)cmdSetAddress).set_Name("cmdSetAddress");
			((Control)cmdSetAddress).set_Size(new Size(20, 20));
			((Control)cmdSetAddress).set_TabIndex(15);
			((ButtonBase)cmdSetAddress).set_UseVisualStyleBackColor(true);
			((Control)cmdSetAddress).add_Click((EventHandler)cmdSetAddress_Click);
			((Control)txtAddress).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAddress).set_Location(new Point(92, 8));
			((Control)txtAddress).set_Name("txtAddress");
			((Control)txtAddress).set_Size(new Size(468, 20));
			((Control)txtAddress).set_TabIndex(3);
			toolTip.SetToolTip((Control)(object)txtAddress, "This 'snail-mail' address is no longer used. All correspondance is by email");
			((Control)txtAddress).add_TextChanged((EventHandler)txtAddress_TextChanged);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(11, 12));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(45, 13));
			((Control)label14).set_TabIndex(2);
			((Control)label14).set_Text("Address");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(618, 20));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(39, 26));
			((Control)label22).set_TabIndex(10);
			((Control)label22).set_Text("Insert\r\ndefault");
			label22.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(678, 20));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(39, 26));
			((Control)label21).set_TabIndex(11);
			((Control)label21).set_Text("Set as\r\ndefault");
			label21.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdSetPlace).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetPlace).set_Location(new Point(687, 66));
			((Control)cmdSetPlace).set_Name("cmdSetPlace");
			((Control)cmdSetPlace).set_Size(new Size(20, 20));
			((Control)cmdSetPlace).set_TabIndex(13);
			toolTip.SetToolTip((Control)(object)cmdSetPlace, "x");
			((ButtonBase)cmdSetPlace).set_UseVisualStyleBackColor(true);
			((Control)cmdSetPlace).add_Click((EventHandler)cmdSetPlace_Click);
			((Control)cmdGetEMail).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetEMail).set_Location(new Point(627, 138));
			((Control)cmdGetEMail).set_Name("cmdGetEMail");
			((Control)cmdGetEMail).set_Size(new Size(20, 20));
			((Control)cmdGetEMail).set_TabIndex(16);
			((ButtonBase)cmdGetEMail).set_UseVisualStyleBackColor(true);
			((Control)cmdGetEMail).add_Click((EventHandler)cmdGetEMail_Click);
			((Control)cmdSetEmail).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetEmail).set_Location(new Point(687, 138));
			((Control)cmdSetEmail).set_Name("cmdSetEmail");
			((Control)cmdSetEmail).set_Size(new Size(20, 20));
			((Control)cmdSetEmail).set_TabIndex(17);
			((ButtonBase)cmdSetEmail).set_UseVisualStyleBackColor(true);
			((Control)cmdSetEmail).add_Click((EventHandler)cmdSetEmail_Click);
			((Control)cmdGetPlace).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetPlace).set_Location(new Point(627, 66));
			((Control)cmdGetPlace).set_Name("cmdGetPlace");
			((Control)cmdGetPlace).set_Size(new Size(20, 20));
			((Control)cmdGetPlace).set_TabIndex(12);
			((ButtonBase)cmdGetPlace).set_UseVisualStyleBackColor(true);
			((Control)cmdGetPlace).add_Click((EventHandler)cmdGetPlace_Click);
			((Control)txtEmail).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtEmail).set_Location(new Point(132, 138));
			((Control)txtEmail).set_Name("txtEmail");
			((Control)txtEmail).set_Size(new Size(468, 20));
			((Control)txtEmail).set_TabIndex(5);
			((Control)txtEmail).set_Text("MyAddress@MyISP.com.xx");
			toolTip.SetToolTip((Control)(object)txtEmail, "Insert your email address - so that the analysis can be sent to you!");
			((Control)txtEmail).add_TextChanged((EventHandler)txtEmail_TextChanged);
			((Control)txtPlace).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtPlace).set_Location(new Point(132, 66));
			((TextBoxBase)txtPlace).set_MaxLength(50);
			((Control)txtPlace).set_Name("txtPlace");
			((Control)txtPlace).set_Size(new Size(468, 20));
			((Control)txtPlace).set_TabIndex(1);
			((Control)txtPlace).set_Text("My home town, My state, My Country");
			toolTip.SetToolTip((Control)(object)txtPlace, "Insert the name of the closest city, town or geographic landmark. eg:\r\n     My home town, my State, my country\r\nThis is archived with the observations");
			((Control)txtPlace).add_TextChanged((EventHandler)txtPlace_TextChanged);
			((Control)txtPlace).add_Enter((EventHandler)txtPlace_Enter);
			((Control)txtPlace).add_KeyUp(new KeyEventHandler(txtPlace_KeyUp));
			((Control)txtPlace).add_Leave((EventHandler)txtPlace_Leave);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(51, 142));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(72, 13));
			((Control)label13).set_TabIndex(4);
			((Control)label13).set_Text("Email address");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(51, 70));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(63, 13));
			((Control)label9).set_TabIndex(0);
			((Control)label9).set_Text("Place name");
			((Control)lstReport).set_Anchor((AnchorStyles)1);
			((Control)lstReport).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstReport).set_FormattingEnabled(true);
			lstReport.set_ItemHeight(14);
			((Control)lstReport).set_Location(new Point(143, 83));
			((Control)lstReport).set_Name("lstReport");
			((Control)lstReport).set_Size(new Size(730, 564));
			((Control)lstReport).set_TabIndex(9);
			toolTip.set_AutomaticDelay(100);
			toolTip.set_AutoPopDelay(4000);
			toolTip.set_InitialDelay(100);
			toolTip.set_ReshowDelay(20);
			((Control)label78).set_AutoSize(true);
			((Control)label78).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label78).set_ForeColor(Color.Maroon);
			((Control)label78).set_Location(new Point(129, 41));
			((Control)label78).set_Name("label78");
			((Control)label78).set_Size(new Size(117, 13));
			((Control)label78).set_TabIndex(33);
			((Control)label78).set_Text("No accented characters");
			((Control)label79).set_AutoSize(true);
			((Control)label79).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label79).set_ForeColor(Color.Maroon);
			((Control)label79).set_Location(new Point(70, 254));
			((Control)label79).set_Name("label79");
			((Control)label79).set_Size(new Size(117, 13));
			((Control)label79).set_TabIndex(34);
			((Control)label79).set_Text("No accented characters");
			((Control)label80).set_AutoSize(true);
			((Control)label80).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label80).set_ForeColor(Color.Maroon);
			((Control)label80).set_Location(new Point(150, 292));
			((Control)label80).set_Name("label80");
			((Control)label80).set_Size(new Size(117, 13));
			((Control)label80).set_TabIndex(40);
			((Control)label80).set_Text("No accented characters");
			((Control)label81).set_AutoSize(true);
			((Control)label81).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label81).set_ForeColor(Color.Maroon);
			((Control)label81).set_Location(new Point(101, 20));
			((Control)label81).set_Name("label81");
			((Control)label81).set_Size(new Size(117, 13));
			((Control)label81).set_TabIndex(34);
			((Control)label81).set_Text("No accented characters");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1016, 666));
			((Control)this).get_Controls().Add((Control)(object)grpNames);
			((Control)this).get_Controls().Add((Control)(object)grpHeader);
			((Control)this).get_Controls().Add((Control)(object)grpEvents);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)lstReport);
			((Control)this).get_Controls().Add((Control)(object)grpSites);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationObservationsEditor", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationObservationsEditor);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(1024, 700));
			((Control)this).set_Name("ObservationsEditor");
			((Control)this).set_Text("Observations editor : ");
			((Form)this).add_FormClosing(new FormClosingEventHandler(ObservationsEditor_FormClosing));
			((Form)this).add_Load((EventHandler)ObservationsEditor_Load);
			((Control)this).add_Resize((EventHandler)ObservationsEditor_Resize);
			((Control)grpSites).ResumeLayout(false);
			((Control)grpSortSites).ResumeLayout(false);
			((Control)grpSortSites).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((Control)grpILOCstations).ResumeLayout(false);
			((Control)grpILOCstations).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)panelHeightDatum).ResumeLayout(false);
			((Control)panelHeightDatum).PerformLayout();
			((Control)panelDMS).ResumeLayout(false);
			((Control)panelDMS).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panelDMM).ResumeLayout(false);
			((Control)panelDMM).PerformLayout();
			((Control)panelDDD).ResumeLayout(false);
			((Control)panelDDD).PerformLayout();
			((Control)grpNames).ResumeLayout(false);
			((Control)grpNames).PerformLayout();
			((Control)grpILOCnames).ResumeLayout(false);
			((Control)grpILOCnames).PerformLayout();
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((Control)grpEvents).ResumeLayout(false);
			((Control)grpEvents).PerformLayout();
			((ISupportInitialize)picLiMovie).EndInit();
			((Control)grpEditControl).ResumeLayout(false);
			((Control)grpEditControl).PerformLayout();
			((Control)groupBox8).ResumeLayout(false);
			((Control)groupBox8).PerformLayout();
			((Control)grpPlanets).ResumeLayout(false);
			((Control)grpPlanets).PerformLayout();
			((Control)grpMethods).ResumeLayout(false);
			((Control)grpMethods).PerformLayout();
			((Control)grpObserver).ResumeLayout(false);
			((Control)grpObserver).PerformLayout();
			((Control)grpILOCevents).ResumeLayout(false);
			((Control)grpILOCevents).PerformLayout();
			((Control)groupBox6).ResumeLayout(false);
			((Control)groupBox6).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnHour).EndInit();
			((ISupportInitialize)updnMin).EndInit();
			((ISupportInitialize)updnYear).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)grpHeader).ResumeLayout(false);
			((Control)grpHeader).PerformLayout();
			((Control)panelRepresentative).ResumeLayout(false);
			((Control)panelRepresentative).PerformLayout();
			((Control)panelReportedTo).ResumeLayout(false);
			((Control)panelReportedTo).PerformLayout();
			((Control)panelAddress).ResumeLayout(false);
			((Control)panelAddress).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
