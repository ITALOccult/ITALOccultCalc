using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class ArchiveEditor : Form
	{
		private string LargeResidualFile;

		private int[] LargeResidualRecord = new int[25000];

		private double JD;

		private double Longitude;

		private double Latitude;

		private double Residual;

		private double HeightSite;

		private string StarCat;

		private string Reduction;

		private string EventCode;

		private string TelDia;

		private string WDScode;

		private string NewTime;

		private string NewStar;

		private string NewSite;

		private string NewAccuracy;

		private string SiteLocation;

		private int StarNumber;

		private int DatumNumber;

		private const double Radian = 180.0 / Math.PI;

		private const int RecordLength = 270;

		private char[] Out = new char[110];

		private bool PopulatingNewEvent;

		private string Observer;

		private bool EventsForDatumsLoaded;

		private bool VerticalDatumIsMSL = true;

		private string RGOtest;

		private string ILOCtest;

		private List<Archive_File_Record> Site_ArchiveFileRecord = new List<Archive_File_Record>();

		private Archive_File_Record AFR;

		private bool ChangingStar;

		private bool FormCreated;

		private List<Archive_File_Record> Star_ArchiveFileRecord = new List<Archive_File_Record>();

		internal ArrayList StarList = new ArrayList();

		private readonly string ZCNameFile;

		private string GrazeID = "";

		private string GrazeEditPath = "";

		private string GrazeArchiveFile = "";

		private bool LoadingGrazeList;

		private IContainer components;

		private Button cmdAutoCorrectArchive;

		private Button cmdReduceArchiveFile;

		private ListBox lstEdit;

		private Button cmdListLargeResiduals;

		private GroupBox pnlPre1750;

		private Label label14;

		private NumericUpDown updnSecond;

		private Label label13;

		private NumericUpDown updnPE;

		private Button cmdAdjustedReduce;

		private Label label12;

		private Label label11;

		private Label label10;

		private Label label9;

		private Label label8;

		private Label label7;

		private NumericUpDown updnDay;

		private NumericUpDown updnHour;

		private NumericUpDown updnMinute;

		private NumericUpDown updnMonth;

		private NumericUpDown updnYear;

		private Label label1;

		private Label lblResidual;

		private Button cmdIdentify;

		internal RadioButton optXZ;

		internal RadioButton optSAO;

		internal RadioButton optZC;

		internal TextBox txtStar;

		private Button cmdGoogleEarth;

		private GroupBox groupBox2;

		private NumericUpDown updnLatS;

		private NumericUpDown updnLatD;

		private NumericUpDown updnLatM;

		private GroupBox groupBox1;

		private NumericUpDown updnLongSec;

		private NumericUpDown updnLongD;

		private NumericUpDown updnLongM;

		private Label label15;

		private Label label5;

		private Label label3;

		internal RadioButton optS;

		internal RadioButton optN;

		private Label label6;

		private Label label4;

		private Label label2;

		internal RadioButton optW;

		internal RadioButton optE;

		private RadioButton optA;

		private RadioButton optF;

		private RadioButton optD;

		private RadioButton optC;

		private RadioButton optB;

		private RadioButton optEc;

		private GroupBox groupBox3;

		private RadioButton optX;

		private RadioButton optY;

		private RadioButton optZ;

		private Button cmdCorrect;

		internal RadioButton optPlanet;

		private RadioButton optWe;

		private GroupBox groupBox4;

		private GroupBox groupBox5;

		private Button cmdSortArchive;

		private Button cmdMergeArchiveFiles;

		private RadioButton optT;

		private Label label17;

		private Label label16;

		private CheckBox chkSecs;

		private RadioButton optAll;

		private RadioButton optUnresolved;

		private RadioButton optCorrected;

		private CheckBox chkDR;

		private RadioButton optUnconsidered;

		private Button cmdRetrieveOriginal;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem saveListToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem appendListToolStripMenuItem;

		private Button cmdPutEventsInEditor;

		private TextBox txtEventYear;

		private Panel panelGeneralEdit;

		private Panel panelEventTypes;

		private RadioButton optGeneral;

		private RadioButton optSiteEditor;

		private Panel panelSiteEdit;

		private Panel panelRGO;

		private RadioButton optILOC;

		private RadioButton optRGO;

		private Panel panelILOC;

		private TextBox txtILOC2;

		private TextBox txtILOC1;

		private TextBox txtRGO2;

		private TextBox txtRGO1;

		private TextBox txtILOC3;

		private Button cmdGetSiteEvents;

		private GroupBox groupBox6;

		private TextBox txtLatS1;

		private TextBox txtLatM1;

		private TextBox txtLatD1;

		private TextBox txtLatS2;

		private TextBox txtLatM2;

		private TextBox txtLatD2;

		private TextBox txtLonS1;

		private TextBox txtLonM1;

		private TextBox txtLonD1;

		private TextBox txtLonS2;

		private TextBox txtLonM2;

		private TextBox txtLonD2;

		private ComboBox cmbDatum1;

		private ComboBox cmbDatum2;

		private ComboBox cmbAltDatum2;

		private ComboBox cmbAltDatum1;

		private TextBox txtAlt1;

		private TextBox txtAlt2;

		private TextBox txtSite2;

		private TextBox txtSite1;

		private TextBox txtObs1;

		private TextBox txtObs2;

		private Label lblSiteLeft;

		private Label lblObsLeft;

		private Label label22;

		private Label label21;

		private Label label20;

		private Label label19;

		private Label label18;

		private Label label24;

		private Label label23;

		private CheckBox chkCorrectSite;

		private CheckBox chkCorrectNames;

		private CheckBox chkCoords;

		private Button cmdUpdateSites;

		private Button cmdGoogleEarthSites;

		private Label lblNewCoords;

		private Label lblOldCoords;

		private Label label25;

		private Label label27;

		private Label label26;

		private Label lblCurrentCoords;

		private Panel panelHeader;

		private CheckBox chkGraze;

		private RadioButton optDoublesEditor;

		private Panel panelDoubles;

		private Button cmdReadForDoubles;

		private CheckBox chkDoublesOnly;

		private TextBox txtXZ;

		private ComboBox cmbNames;

		private Label label28;

		private Label label29;

		private Label label30;

		private Label label31;

		private TextBox txtZC;

		private TextBox txtSAO;

		private ComboBox cmbStarList;

		private Label lblResidual2;

		private Label label34;

		private Button cmdStarsCheckAll;

		private Button cmdStarsClearAll;

		private GroupBox grpSetWDS;

		private Button cmdApplyWDS;

		private ComboBox cmbWDS;

		private GroupBox groupBox7;

		private Label label32;

		private ListBox lstDoubles;

		private Label labelDouble;

		internal RadioButton optUnknown;

		private RadioButton optG;

		private CheckBox chkbySiteID;

		private RadioButton optU;

		private RadioButton optSs;

		private Label label33;

		internal TextBox txtGrazeNumber;

		private Button cmdRetrieveRGOsite;

		private CheckBox chkSites;

		private Panel PanelILOCsiteReplace;

		private TextBox txtILOC3replace;

		private CheckBox chkCorrectILOCsites;

		private TextBox txtILOC2replace;

		private TextBox txtILOC1replace;

		private ToolStripMenuItem withCheckedListToolStripMenuItem;

		private ToolStripMenuItem checkAllToolStripMenuItem;

		private ToolStripMenuItem clearAllToolStripMenuItem;

		private Button cmdRetrieveNewILOCsite;

		private TextBox txtTelescope;

		private CheckBox chkChangeTelescope;

		private TextBox txtILOCSearchSite;

		private Label label35;

		private TextBox txtEventMonth;

		private Button cmdGeneratePrediction;

		internal RadioButton optAsteroid;

		private RadioButton optNoCorrection;

		private RadioButton optPlanets;

		private RadioButton optOther;

		private ComboBox cmbWDS_General;

		private Label label72;

		private Button cmdProcessfromCoordinators;

		private RadioButton optGrazeEditor;

		private RadioButton optCombineRegions;

		private Panel panelGraze;

		private GroupBox grpGrazeFlag;

		private Button cmdClearGrazeFlags;

		private Button cmdSetGrazeFlags;

		private Label lblGrazeID;

		private GroupBox grpNullSeconds;

		private Button cmdSetZeroSecs;

		private Button cmdFindBlankSecs;

		private ComboBox cmbObservationsSource;

		private Panel panelCombine;

		private Label label36;

		private Panel panelResiduals;

		private Button cmdGrazeEventsToEditor;

		internal CheckedListBox lstCheckedEdit;

		private NumericUpDown updnLimbCorrectedResidual;

		private Button cmdMarkLargeResiduals;

		private GroupBox grpLargeResiduals;

		private RadioButton optSmoothResidual;

		private RadioButton optLimbResidual;

		private Label label39;

		private Label label38;

		private NumericUpDown updnSmoothTop;

		private NumericUpDown updnSmoothBottom;

		private Label label37;

		private Button cmdGenerateGrazeIndex;

		private Button cmdSelectArchiveForGrazeEdit;

		private ComboBox cmbGrazeList;

		private GroupBox groupBox11;

		private RadioButton optGraze_NullSecs;

		private RadioButton optGraze_RenumberGrazes;

		private RadioButton optGraze_Duplicates;

		private RadioButton optGraze_LargeResiduals;

		private RadioButton optGraze_Grazeflag;

		private Button cmdGrazeEventsInEditor_Valid;

		private Button cmdGrazeEventsInEditor_No_StartEnd;

		private Button cmdSetFlagLargeResiduals;

		private GroupBox grpDuplicateGrazeEvents;

		private Button cmdGrazeSetDuplicateToKeep;

		private Button cmdGrazeSetDuplicateToDelete;

		private Label lblSource;

		private Button cmdRecordGrazeAsEdited;

		private Button cmdClearDuplicatteResidualFlags;

		private CheckBox chkExcludeInvalid;

		private CheckBox chkExcludeStartEnd;

		private Label label40;

		private CheckBox checkBox1;

		private CheckBox checkBox2;

		private GroupBox grpGrazeRenumber;

		private Button cmdRenumberGrazes;

		private Label lblGrazeFile;

		private Button cmdTagAllGrazeEvents;

		private RadioButton optTagGrazes;

		private RadioButton optHT;

		private RadioButton optSomaWGS84;

		private GroupBox grpDatums;

		private RadioButton optDatums;

		private Button cmdSetSomaVerticalToEllipsoid;

		private Button cmdFindSomaEllipsoid;

		private Button cmdSetSelectedToDatum;

		private Button cmdFindZeroDatums;

		private Label label41;

		private ComboBox cmbHorizontalDatum2;

		private Button cmdSetSomaVerticaltoMSL;

		private ToolStripMenuItem exitToolStripMenuItem;

		private RadioButton optMarkAs2ndLimit;

		private GroupBox grp2ndLimit;

		private Button cmdClear2ndLimit;

		private Button cmdSet2ndLimit;

		private CheckBox chkUseGrazeIDonly;

		private RadioButton optGrazeSiteDetails;

		private GroupBox grpGrazeScopes;

		private Label label42;

		private Button cmdGrazeSitesCheckeSameAsFirst;

		private Label lblnumberChecked;

		private GroupBox grpTelescopesAndNames;

		private Label label43;

		private ComboBox cmbMount;

		private Label label44;

		private ComboBox cmbDrive;

		private Label label45;

		private Label label46;

		private Label label47;

		private TextBox txtFocalLength;

		private Label label48;

		private Label label49;

		private ComboBox cmbTelescope;

		private TextBox txtAperture;

		private Label label70;

		private Label lblCharsLeftName;

		private Label lblObserverName;

		private Label label50;

		private TextBox txtName;

		private Label label75;

		private Label lblCharsLeft;

		private Label label68;

		private TextBox txtPlace;

		private Button cmdGrazeUpdateSites;

		private RadioButton optGrazeEventCode;

		private GroupBox grpGrazeDR;

		private ComboBox cmbGrazeEvents;

		private Button cmdGrazesSetEvent;

		private RadioButton optTelescopeCodes;

		private GroupBox grpTelescopeCodes;

		private Button cmdGrazesGetTelescopes;

		private Button cmdGrazeSitesAutoCorrect;

		private RadioButton optGrazeSites;

		private GroupBox grpGrazeSites;

		private Label lblnumberChecked2;

		private Label label52;

		private Button cmdGrazeSitesCheckeSameAsFirst_2;

		private Label label57;

		private Label label58;

		private Label label59;

		private Label label60;

		private Label label61;

		private ComboBox cmbAltDatum3;

		private TextBox txtAlt3;

		private ComboBox cmbDatum3;

		private TextBox txtLatS3;

		private TextBox txtLatM3;

		private TextBox txtLatD3;

		private TextBox txtLonS3;

		private TextBox txtLonM3;

		private TextBox txtLonD3;

		private Button cmdGrazeChangeSites;

		private RadioButton optGrazeSetDoubles;

		private GroupBox grpGrazeDoubles;

		private GroupBox groupBox8;

		private Button cmdGrazeWDS;

		private ComboBox cmbGrazeWDS;

		private ToolStripMenuItem invertSelectionToolStripMenuItem;

		private ComboBox cmbDoubleCode;

		private Button cmdGrazeDoublesCheck;

		private Button cmdGrazeClearChecks;

		private ComboBox cmbDoubleCodes;

		private Button cmdCheckDoubles;

		private Button cmdPasteLocation;

		private Button cmdPasteObserver;

		private Button cmdCheckFromClipboard;

		private Button cmdCheckFromClipboard2;

		private Button cmdSomaPasteSiteCoords;

		private Button cmdSetPEApplication;

		private RadioButton optGrazeEventDuration;

		private GroupBox grpEventDuration;

		private TextBox txtEventDuration;

		private Button cmdSetGrazeEventDuration;

		private Label label53;

		private Label label51;

		private Label lblCurrentEventTime;

		private Label label54;

		private Button cmdSetMidTime;

		private TextBox txtSecondsForDuration;

		private Label label55;

		private CheckBox chkAutoIncrement;

		private CheckBox chkAutoDisplay;

		private Label label56;

		private TextBox txtEmailSignatureName;

		internal ComboBox cmbGrazeDoubles;

		private Button cmdGrazeSetDoubleCode;

		private Label label62;

		private ToolStripMenuItem checkAllSourcedFromOToolStripMenuItem;

		private ToolStripMenuItem checkAllSourcedFrToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem checkAllSourcedFromSToolStripMenuItem;

		private CheckBox chkSomaGrazeClearChecks;

		private Label label63;

		private Button cmdListDuplicates;

		private Button cmdVizier;

		private GroupBox groupBox9;

		private CheckBox chkVizierLimitTo1960;

		private RadioButton optListSite;

		private RadioButton optListObserver;

		private TextBox txtListSiteName;

		private TextBox txtListObserver;

		private CheckBox chkAdjAcc;

		private Label label64;

		private NumericUpDown updnAcc;

		private CheckBox chkReReducedOldObs;

		private Panel panel1;

		private Label label65;

		private ComboBox cmbCertainty;

		public ArchiveEditor()
		{
			InitializeComponent();
			ZCNameFile = Utilities.AppPath + "\\Resource Files\\ZCNames.dat";
			FormCreated = false;
			((Control)lstCheckedEdit).set_Visible(false);
		}

		private void ArchiveEditor_Load(object sender, EventArgs e)
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
			((Control)txtEventYear).set_Text(DateTime.Now.Year.ToString());
			((Control)panelHeader).set_Top(20);
			((Control)panelHeader).set_Left(200);
			Panel obj = panelGeneralEdit;
			Panel obj2 = panelSiteEdit;
			Panel obj3 = panelDoubles;
			int num;
			((Control)panelGraze).set_Top(num = 398);
			int num2;
			((Control)obj3).set_Top(num2 = num);
			int top;
			((Control)obj2).set_Top(top = num2);
			((Control)obj).set_Top(top);
			Panel obj4 = panelGeneralEdit;
			Panel obj5 = panelSiteEdit;
			Panel obj6 = panelDoubles;
			((Control)panelGraze).set_Left(num = 7);
			((Control)obj6).set_Left(num2 = num);
			((Control)obj5).set_Left(top = num2);
			((Control)obj4).set_Left(top);
			((Control)panelResiduals).set_Top(390);
			((Control)panelResiduals).set_Left(200);
			ComboBox obj7 = cmbAltDatum1;
			ComboBox obj8 = cmbAltDatum2;
			ComboBox obj9 = cmbDatum1;
			ComboBox obj10 = cmbDatum2;
			int num3;
			((ListControl)cmbWDS).set_SelectedIndex(num3 = 0);
			((ListControl)obj10).set_SelectedIndex(num = num3);
			((ListControl)obj9).set_SelectedIndex(num2 = num);
			((ListControl)obj8).set_SelectedIndex(top = num2);
			((ListControl)obj7).set_SelectedIndex(top);
			ComboBox obj11 = cmbAltDatum3;
			ComboBox obj12 = cmbDatum3;
			ComboBox obj13 = cmbGrazeWDS;
			ComboBox obj14 = cmbDoubleCode;
			((ListControl)cmbDoubleCodes).set_SelectedIndex(num3 = 0);
			((ListControl)obj14).set_SelectedIndex(num = num3);
			((ListControl)obj13).set_SelectedIndex(num2 = num);
			((ListControl)obj12).set_SelectedIndex(top = num2);
			((ListControl)obj11).set_SelectedIndex(top);
			((Control)lstCheckedEdit).set_Width(((Control)lstEdit).get_Width());
			((Control)lstCheckedEdit).set_Height(((Control)lstEdit).get_Height());
			((Control)lstCheckedEdit).set_Top(((Control)lstEdit).get_Top());
			((Control)lstCheckedEdit).set_Left(((Control)lstEdit).get_Left());
			FormCreated = true;
			((ListControl)cmbStarList).set_SelectedIndex(3);
			((ListControl)cmbObservationsSource).set_SelectedIndex(0);
			((ListControl)cmbHorizontalDatum2).set_SelectedIndex(0);
			((ListControl)cmbGrazeEvents).set_SelectedIndex(1);
			SetPanelVisibility();
			setGrazeEditEnabled();
			GroupBox obj15 = grpEventDuration;
			GroupBox obj16 = grpGrazeDoubles;
			GroupBox obj17 = grpGrazeSites;
			GroupBox obj18 = grpTelescopeCodes;
			GroupBox obj19 = grpGrazeDR;
			GroupBox obj20 = grpGrazeScopes;
			GroupBox obj21 = grpDatums;
			GroupBox obj22 = grp2ndLimit;
			GroupBox obj23 = grpGrazeFlag;
			GroupBox obj24 = grpNullSeconds;
			GroupBox obj25 = grpLargeResiduals;
			GroupBox obj26 = grpGrazeRenumber;
			int num4;
			((Control)grpDuplicateGrazeEvents).set_Top(num4 = 45);
			int num5;
			((Control)obj26).set_Top(num5 = num4);
			int num6;
			((Control)obj25).set_Top(num6 = num5);
			int num7;
			((Control)obj24).set_Top(num7 = num6);
			int num8;
			((Control)obj23).set_Top(num8 = num7);
			int num9;
			((Control)obj22).set_Top(num9 = num8);
			int num10;
			((Control)obj21).set_Top(num10 = num9);
			int num11;
			((Control)obj20).set_Top(num11 = num10);
			((Control)obj19).set_Top(num3 = num11);
			((Control)obj18).set_Top(num = num3);
			((Control)obj17).set_Top(num2 = num);
			((Control)obj16).set_Top(top = num2);
			((Control)obj15).set_Top(top);
			GroupBox obj27 = grpEventDuration;
			GroupBox obj28 = grpGrazeDoubles;
			GroupBox obj29 = grpGrazeSites;
			GroupBox obj30 = grpTelescopeCodes;
			GroupBox obj31 = grpGrazeDR;
			GroupBox obj32 = grpGrazeScopes;
			GroupBox obj33 = grpDatums;
			GroupBox obj34 = grp2ndLimit;
			GroupBox obj35 = grpGrazeFlag;
			GroupBox obj36 = grpNullSeconds;
			GroupBox obj37 = grpLargeResiduals;
			GroupBox obj38 = grpGrazeRenumber;
			((Control)grpDuplicateGrazeEvents).set_Left(num4 = 360);
			((Control)obj38).set_Left(num5 = num4);
			((Control)obj37).set_Left(num6 = num5);
			((Control)obj36).set_Left(num7 = num6);
			((Control)obj35).set_Left(num8 = num7);
			((Control)obj34).set_Left(num9 = num8);
			((Control)obj33).set_Left(num10 = num9);
			((Control)obj32).set_Left(num11 = num10);
			((Control)obj31).set_Left(num3 = num11);
			((Control)obj30).set_Left(num = num3);
			((Control)obj29).set_Left(num2 = num);
			((Control)obj28).set_Left(top = num2);
			((Control)obj27).set_Left(top);
			((Control)cmdClearDuplicatteResidualFlags).set_Top(50);
			((Control)cmdClearDuplicatteResidualFlags).set_Left(580);
			((Control)cmdTagAllGrazeEvents).set_Top(50);
			((Control)cmdTagAllGrazeEvents).set_Left(380);
			SetSiteSelectionFields();
		}

		private void ArchiveEditor_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() > 80)
			{
				ListBox obj = lstEdit;
				int width;
				((Control)lstCheckedEdit).set_Width(width = ((Control)this).get_Width() - 40);
				((Control)obj).set_Width(width);
			}
			if (((Control)this).get_Height() > 500)
			{
				ListBox obj2 = lstEdit;
				int width;
				((Control)lstCheckedEdit).set_Height(width = ((Control)this).get_Height() - 400);
				((Control)obj2).set_Height(width);
				Panel obj3 = panelGeneralEdit;
				Panel obj4 = panelSiteEdit;
				Panel obj5 = panelDoubles;
				int num;
				((Control)panelGraze).set_Top(num = ((Control)lstEdit).get_Top() + ((Control)lstEdit).get_Height() + 4);
				int num2;
				((Control)obj5).set_Top(num2 = num);
				((Control)obj4).set_Top(width = num2);
				((Control)obj3).set_Top(width);
				((Control)panelResiduals).set_Top(((Control)lstEdit).get_Top() + ((Control)lstEdit).get_Height() - 4);
			}
		}

		private void optGeneral_CheckedChanged(object sender, EventArgs e)
		{
			SetPanelVisibility();
		}

		private void optSiteEditor_CheckedChanged(object sender, EventArgs e)
		{
			SetPanelVisibility();
		}

		private void optDoublesEditor_CheckedChanged(object sender, EventArgs e)
		{
			SetPanelVisibility();
		}

		private void optGrazeEditor_CheckedChanged(object sender, EventArgs e)
		{
			SetPanelVisibility();
		}

		private void optCombineRegions_CheckedChanged(object sender, EventArgs e)
		{
			SetPanelVisibility();
		}

		private void SetPanelVisibility()
		{
			ClearLists();
			((Control)panelCombine).set_Visible(optCombineRegions.get_Checked());
			Panel obj = panelGeneralEdit;
			ListBox obj2 = lstEdit;
			bool @checked;
			((Control)panelHeader).set_Visible(@checked = optGeneral.get_Checked());
			bool visible;
			((Control)obj2).set_Visible(visible = @checked);
			((Control)obj).set_Visible(visible);
			((Control)panelSiteEdit).set_Visible(optSiteEditor.get_Checked());
			((Control)panelDoubles).set_Visible(optDoublesEditor.get_Checked());
			ToolStripMenuItem obj3 = withCheckedListToolStripMenuItem;
			((Control)lstCheckedEdit).set_Visible(visible = optSiteEditor.get_Checked() | optDoublesEditor.get_Checked() | optGrazeEditor.get_Checked());
			((ToolStripItem)obj3).set_Visible(visible);
			((Control)panelGraze).set_Visible(optGrazeEditor.get_Checked());
			((Control)panelResiduals).set_Visible(optDoublesEditor.get_Checked() | optGrazeEditor.get_Checked());
			GrazeEditPath = "";
			GrazeArchiveFile = "";
			cmbGrazeList.get_Items().Clear();
			((Control)lblGrazeFile).set_Text("Current file");
		}

		private void ClearLists()
		{
			lstEdit.get_Items().Clear();
			((ObjectCollection)lstCheckedEdit.get_Items()).Clear();
			LargeResidualRecord.Initialize();
			Site_ArchiveFileRecord.Clear();
			Star_ArchiveFileRecord.Clear();
		}

		private void cmdProcessfromCoordinators_Click(object sender, EventArgs e)
		{
			//IL_0010: Unknown result type (might be due to invalid IL or missing references)
			//IL_0017: Expected O, but got Unknown
			//IL_004f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0055: Invalid comparison between Unknown and I4
			//IL_0385: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f4: Invalid comparison between Unknown and I4
			//IL_040b: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f3: Invalid comparison between Unknown and I4
			//IL_052c: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			Cursor.set_Current(Cursors.get_WaitCursor());
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Archive files to Process. ['MergedFiles', 'Archive Observations', 'Log File', 'Manually Added' + .zip .rar .arj .lzh .gz & .tar files will be ignored]");
			((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Lunar Archive");
			((FileDialog)val).set_FileName("*.*");
			val.set_Multiselect(true);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				int upperBound = ((FileDialog)val).get_FileNames().GetUpperBound(0);
				if (upperBound < 0)
				{
					return;
				}
				string text2 = Path.GetDirectoryName(((FileDialog)val).get_FileNames()[0]) + "\\MergedFiles.txt";
				if (File.Exists(text2))
				{
					File.Delete(text2);
				}
				text = Path.GetDirectoryName(((FileDialog)val).get_FileNames()[0]) + "\\Log File.txt";
				if (File.Exists(text))
				{
					File.Delete(text);
				}
				using (StreamWriter streamWriter = new StreamWriter(text))
				{
					streamWriter.WriteLine("Archive Observations recent.dat created\r\non " + DateTime.Now.ToUniversalTime().ToLongDateString() + " UT, using:");
					streamWriter.WriteLine("");
					for (int i = 0; i <= upperBound; i++)
					{
						if (!((FileDialog)val).get_FileNames()[i].Contains("MergedFiles.txt") && !((FileDialog)val).get_FileNames()[i].Contains("Archive Observations recent.dat") && !((FileDialog)val).get_FileNames()[i].Contains("Log File.txt") && !((FileDialog)val).get_FileNames()[i].ToLower().Contains("manually added") && !((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".zip") && !((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".rar") && !((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".arj") && !((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".lzh") && !((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".gz") && !((FileDialog)val).get_FileNames()[i].ToLower().EndsWith(".tar"))
						{
							File.AppendAllText(text2, File.ReadAllText(((FileDialog)val).get_FileNames()[i]));
							streamWriter.WriteLine(Path.GetFileName(((FileDialog)val).get_FileNames()[i]));
						}
					}
					streamWriter.WriteLine("* * * * *");
				}
				LunarObservations.AutoCorrectArchiveFile(text2, "AHIMNOPRSX".Substring(((ListControl)cmbObservationsSource).get_SelectedIndex(), 1));
				SortFile(Path.GetDirectoryName(text2) + "\\C_MergedFiles.txt");
				ListDuplicates(Path.GetDirectoryName(text2) + "\\S_C_MergedFiles.txt");
				ArrayList arrayList = new ArrayList();
				try
				{
					if (LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().get_Count() > 0)
					{
						try
						{
							for (int j = 0; j < LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().get_Count(); j++)
							{
								arrayList.Add(LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().get_Item(j).ToString());
							}
							((Form)LunarObservations.ArchiveDuplicates).Close();
						}
						catch
						{
						}
						LunarObservations.ArchiveDuplicates.SetForArchiving(ForArchiving: true);
						((Form)LunarObservations.ArchiveDuplicates).set_DialogResult((DialogResult)2);
						try
						{
							for (int k = 0; k < arrayList.Count; k++)
							{
								LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Add((object)arrayList[k]!.ToString());
							}
							LunarObservations.ArchiveDuplicates.SetForArchiving(ForArchiving: true);
							((Form)LunarObservations.ArchiveDuplicates).ShowDialog();
						}
						catch
						{
							LunarObservations.ArchiveDuplicates = new ListDuplicates();
							for (int l = 0; l < arrayList.Count; l++)
							{
								LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Add((object)arrayList[l]!.ToString());
							}
							LunarObservations.ArchiveDuplicates.SetForArchiving(ForArchiving: true);
							((Form)LunarObservations.ArchiveDuplicates).ShowDialog();
						}
						if ((int)((Form)LunarObservations.ArchiveDuplicates).get_DialogResult() == 2)
						{
							return;
						}
					}
				}
				catch
				{
					MessageBox.Show("Error with Duplicates.\r\n\r\n Processing cancelled", "Error", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
				string text3 = Path.GetDirectoryName(text2) + "\\Archive Observations recent.dat";
				if (File.Exists(Path.GetDirectoryName(text2) + "\\Archive Observations recent.dat"))
				{
					File.Delete(text3);
				}
				File.Move(Path.GetDirectoryName(text2) + "\\S_C_MergedFiles.txt", text3);
				GrazeArchiveFile = text3;
				RenumberGrazes();
				if (File.Exists(Path.GetDirectoryName(text2) + "\\C_MergedFiles.txt"))
				{
					File.Delete(Path.GetDirectoryName(text2) + "\\C_MergedFiles.txt");
				}
				if (File.Exists(Path.GetDirectoryName(text2) + "\\MergedFiles.txt"))
				{
					File.Delete(Path.GetDirectoryName(text2) + "\\MergedFiles.txt");
				}
				LunarObservations.ReduceArchiveOccultationFile(text3, UsingSiteID: false, Settings.Default.ArchiveGrazesExcludeStartEnd, Settings.Default.ArchiveGrazesExcludeInvalid);
			}
			if ((int)MessageBox.Show("Do you want to email the Log file to the regional coordinators?", "Confirm send Email", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				string subject = "Recent lunar occultation observations updated - " + DateTime.Now.ToShortDateString();
				MessageBox.Show(Emails.Email_LogFile(text, ((Control)txtEmailSignatureName).get_Text().Trim(), subject), "Success");
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdEmailLogFile_Click(object sender, EventArgs e)
		{
		}

		private void cmdGenerateGrazeIndex_Click(object sender, EventArgs e)
		{
			LunarObservations.CreateGrazeIndexFile();
		}

		private void cmdReduceArchiveFile_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000c: Expected O, but got Unknown
			//IL_0018: Unknown result type (might be due to invalid IL or missing references)
			//IL_001e: Invalid comparison between Unknown and I4
			ClearLists();
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Archive file to reduce");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				string[] files = Directory.GetFiles(Utilities.AppPath + "\\Resource Files\\Moon", "Moon*.bin");
				for (int i = 0; i < files.Length; i++)
				{
					File.Delete(files[i]);
				}
				LunarObservations.ReduceArchiveOccultationFile(((FileDialog)val).get_FileName(), chkbySiteID.get_Checked(), Settings.Default.ArchiveGrazesExcludeStartEnd, Settings.Default.ArchiveGrazesExcludeInvalid);
			}
		}

		private void cmdAutoCorrectArchive_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000c: Expected O, but got Unknown
			//IL_0018: Unknown result type (might be due to invalid IL or missing references)
			//IL_001e: Invalid comparison between Unknown and I4
			ClearLists();
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Archive file to auto-correct");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				LunarObservations.AutoCorrectArchiveFile(((FileDialog)val).get_FileName(), "A");
			}
		}

		private void cmdListLargeResiduals_Click(object sender, EventArgs e)
		{
			//IL_00fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0101: Expected O, but got Unknown
			//IL_012c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0132: Invalid comparison between Unknown and I4
			ClearLists();
			int num = 0;
			int num2 = 0;
			int result = 1900;
			bool @checked = optAll.get_Checked();
			bool flag = false;
			string text = ((Control)txtEventYear).get_Text().Trim();
			if (((Control)txtEventMonth).get_Text().Trim().Length > 0)
			{
				text += ((Control)txtEventMonth).get_Text().Trim().PadLeft(2)
					.Substring(0, 2);
			}
			else
			{
				int.TryParse(text, out result);
				flag = result > 1600 && result < 1750;
			}
			int length = text.Length;
			bool checked2 = optUnresolved.get_Checked();
			bool checked3 = optUnconsidered.get_Checked();
			bool checked4 = optPlanets.get_Checked();
			bool checked5 = optOther.get_Checked();
			bool checked6 = optHT.get_Checked();
			bool flag2 = false;
			string testSite = ((Control)txtILOCSearchSite).get_Text().ToUpper().Trim();
			_ = new char[1];
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Archive file to list large residuals");
			((FileDialog)val).set_FileName("*.*");
			((FileDialog)val).set_InitialDirectory(Settings.Default.ArchiveFile_C);
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			Settings.Default.ArchiveFile_C = ((FileDialog)val).get_InitialDirectory();
			Cursor.set_Current(Cursors.get_WaitCursor());
			LargeResidualFile = ((FileDialog)val).get_FileName();
			lstEdit.BeginUpdate();
			using (StreamReader streamReader = new StreamReader(LargeResidualFile))
			{
				while (!streamReader.EndOfStream)
				{
					string text2 = streamReader.ReadLine();
					flag2 = false;
					if (@checked)
					{
						flag2 = length == 0 || ((!flag) ? text2.Substring(0, 6).Contains(text) : (int.Parse(text2.Substring(0, 4)) < 1750));
					}
					else if (checked4)
					{
						if ("PA".Contains(text2.Substring(18, 1)))
						{
							flag2 = true;
						}
					}
					else if (checked5)
					{
						if (!"RSXPA".Contains(text2.Substring(18, 1)))
						{
							flag2 = true;
						}
					}
					else if (checked6)
					{
						if ("6789".Contains(text2.Substring(12, 1)))
						{
							flag2 = true;
						}
						if ("6789".Contains(text2.Substring(10, 1)))
						{
							flag2 = true;
						}
						if (int.Parse(text2.Substring(8, 2)) > 23)
						{
							flag2 = true;
						}
						if (!"RSXPAU".Contains(text2.Substring(18, 1)))
						{
							flag2 = true;
						}
					}
					else
					{
						byte b = Convert.ToByte(text2.Substring(73, 1).ToCharArray()[0]);
						if (checked3)
						{
							flag2 = b == 89 || b == 85;
						}
						else if (checked2)
						{
							int num3 = Convert.ToInt32(text2.Substring(0, 4));
							flag2 = b > 77 || num3 < 1850;
						}
						else
						{
							if (chkSites.get_Checked())
							{
								if (b > 67 && b < 72)
								{
									flag2 = true;
								}
							}
							else
							{
								flag2 = b < 78 && b > 32;
							}
							if (flag2)
							{
								flag2 = MeetsSiteTest(text2, testSite);
							}
						}
					}
					if (flag2)
					{
						lstEdit.get_Items().Add((object)text2);
						LargeResidualRecord[num] = num2;
						num++;
					}
					num2++;
				}
			}
			lstEdit.EndUpdate();
			Cursor.set_Current(Cursors.get_Default());
			((Control)lstEdit).Focus();
			if (lstEdit.get_Items().get_Count() > 0)
			{
				((ListControl)lstEdit).set_SelectedIndex(0);
			}
		}

		private bool MeetsSiteTest(string InLine, string TestSite)
		{
			if (TestSite.Length == 0)
			{
				return true;
			}
			return InLine.Substring(111, 16).Contains(TestSite);
		}

		private void lstEdit_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstEdit).get_SelectedIndex() >= 0)
			{
				string listLine = lstEdit.get_Items().get_Item(((ListControl)lstEdit).get_SelectedIndex()).ToString();
				DecodeEventLine(listLine);
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void DecodeEventLine(string ListLine)
		{
			//IL_035e: Unknown result type (might be due to invalid IL or missing references)
			PopulatingNewEvent = true;
			chkSecs.set_Checked(false);
			chkDR.set_Checked(false);
			((Control)updnSecond).set_Enabled(false);
			bool flag = true;
			try
			{
				updnYear.set_Value(Convert.ToDecimal(ListLine.Substring(0, 4)));
			}
			catch
			{
				updnYear.set_Value(2000m);
				flag = false;
			}
			try
			{
				updnMonth.set_Value(Convert.ToDecimal(ListLine.Substring(4, 2)));
			}
			catch
			{
				updnMonth.set_Value(1m);
				flag = false;
			}
			try
			{
				updnDay.set_Value(Convert.ToDecimal(ListLine.Substring(6, 2)));
			}
			catch
			{
				updnDay.set_Value(1m);
				flag = false;
			}
			try
			{
				updnHour.set_Value(Convert.ToDecimal(ListLine.Substring(8, 2)));
			}
			catch
			{
				updnHour.set_Value(0m);
				flag = false;
			}
			try
			{
				updnMinute.set_Value(Convert.ToDecimal(ListLine.Substring(10, 2)));
			}
			catch
			{
				updnMinute.set_Value(0m);
				flag = false;
			}
			try
			{
				updnSecond.set_Value(Convert.ToDecimal(ListLine.Substring(12, 6)));
			}
			catch
			{
				updnSecond.set_Value(0m);
				flag = false;
			}
			((Control)pnlPre1750).set_Enabled(true);
			NumericUpDown obj7 = updnAcc;
			ComboBox obj8 = cmbCertainty;
			bool flag2;
			chkAdjAcc.set_Checked(flag2 = false);
			bool enabled;
			((Control)obj8).set_Enabled(enabled = flag2);
			((Control)obj7).set_Enabled(enabled);
			try
			{
				updnAcc.set_Value(Convert.ToDecimal(ListLine.Substring(37, 5)));
			}
			catch
			{
				updnAcc.set_Value(0m);
			}
			int result = 0;
			if (!int.TryParse(ListLine.Substring(42, 1), out result))
			{
				result = 0;
			}
			((ListControl)cmbCertainty).set_SelectedIndex(result);
			chkReReducedOldObs.set_Checked(false);
			switch ("AHIMNOPRSXQ".IndexOf(ListLine.Substring(71, 1)))
			{
			case 0:
				((Control)lblSource).set_Text("new Observation");
				break;
			case 1:
				((Control)lblSource).set_Text("RGO grazes (OCR'd)");
				break;
			case 2:
				((Control)lblSource).set_Text("ILOC computer records");
				break;
			case 3:
				((Control)lblSource).set_Text("Miscellaneous");
				break;
			case 4:
				((Control)lblSource).set_Text("Newcombe - Researches....");
				break;
			case 5:
				((Control)lblSource).set_Text("Occult");
				break;
			case 6:
				((Control)lblSource).set_Text("ILOC, re-reduced");
				break;
			case 7:
				((Control)lblSource).set_Text("RGO computer records");
				break;
			case 8:
				((Control)lblSource).set_Text("Mitsuru Soma");
				break;
			case 9:
				((Control)lblSource).set_Text("ILOC, with no Site code");
				break;
			case 10:
				((Control)lblSource).set_Text("Re-reduced old observation");
				chkReReducedOldObs.set_Checked(true);
				break;
			default:
				((Control)lblSource).set_Text("No source");
				break;
			}
			((Control)lblSource).set_Text("Source: " + ((Control)lblSource).get_Text());
			if (!flag)
			{
				MessageBox.Show("A field in date or time is not valid\r\n\r\n" + ListLine.Substring(0, 74), "Invalid data");
			}
			string text = ListLine.Substring(18, 1);
			optSAO.set_Checked(text == "S");
			optXZ.set_Checked(text == "X");
			optZC.set_Checked(text == "R");
			optPlanet.set_Checked(text == "P");
			optAsteroid.set_Checked(text == "A");
			optUnknown.set_Checked(text == "U");
			((Control)txtStar).set_Text(ListLine.Substring(19, 6));
			int num = -1;
			string text2 = ListLine.Substring(25, 1);
			if (text2.ToLower() == text2)
			{
				num = 25;
			}
			((ListControl)cmbWDS_General).set_SelectedIndex(cmbWDS_General.FindStringExact(text2, num));
			chkGraze.set_Checked(ListLine.Substring(28, 1).Contains("G"));
			((Control)txtGrazeNumber).set_Text(ListLine.Substring(78, 3));
			if (ListLine.Substring(81, 20).Trim().Length < 10)
			{
				NumericUpDown obj10 = updnLongD;
				NumericUpDown obj11 = updnLongM;
				NumericUpDown obj12 = updnLongSec;
				NumericUpDown obj13 = updnLatD;
				NumericUpDown obj14 = updnLatM;
				NumericUpDown obj15 = updnLatS;
				decimal num2 = default(decimal);
				obj15.set_Value(num2);
				decimal num3;
				obj14.set_Value(num3 = num2);
				decimal num4;
				obj13.set_Value(num4 = num3);
				decimal num5;
				obj12.set_Value(num5 = num4);
				decimal value;
				obj11.set_Value(value = num5);
				obj10.set_Value(value);
			}
			else
			{
				optE.set_Checked(ListLine.Substring(81, 1) == "+");
				optW.set_Checked(!optE.get_Checked());
				updnLongD.set_Value(Convert.ToDecimal(ListLine.Substring(82, 3)));
				updnLongM.set_Value(Convert.ToDecimal(ListLine.Substring(85, 2)));
				updnLongSec.set_Value(Convert.ToDecimal(ListLine.Substring(87, 4)));
				optN.set_Checked(ListLine.Substring(92, 1) == "+");
				optS.set_Checked(!optN.get_Checked());
				updnLatD.set_Value(Convert.ToDecimal(ListLine.Substring(93, 2)));
				updnLatM.set_Value(Convert.ToDecimal(ListLine.Substring(95, 2)));
				updnLatS.set_Value(Convert.ToDecimal(ListLine.Substring(97, 4)));
			}
			string text3 = ListLine.Substring(73, 1);
			optNoCorrection.set_Checked(text3 == " ");
			optA.set_Checked(text3 == "A");
			optB.set_Checked(text3 == "B");
			optC.set_Checked(text3 == "C");
			optD.set_Checked(text3 == "D");
			optEc.set_Checked(text3 == "E");
			optF.set_Checked(text3 == "F");
			optG.set_Checked(text3 == "G");
			optSs.set_Checked(text3 == "S");
			optT.set_Checked(text3 == "T");
			optU.set_Checked(text3 == "U");
			optWe.set_Checked(text3 == "W");
			optX.set_Checked(text3 == "X");
			optY.set_Checked(text3 == "Y");
			optZ.set_Checked(text3 == "Z");
			((Control)lblCurrentEventTime).set_Text(ListLine.Substring(0, 4) + " " + ListLine.Substring(4, 2) + " " + ListLine.Substring(6, 2) + ",  " + ListLine.Substring(8, 2) + "h " + ListLine.Substring(10, 2) + "m " + ListLine.Substring(12, 6) + "s ");
			((Control)txtEventDuration).set_Text(ListLine.Substring(47, 5));
			((Control)txtSecondsForDuration).set_Text(ListLine.Substring(12, 6));
			PopulatingNewEvent = false;
		}

		private void cmdRetrieveOriginal_Click(object sender, EventArgs e)
		{
			if (((ListControl)lstEdit).get_SelectedIndex() >= 0)
			{
				PopulatingNewEvent = true;
				chkSecs.set_Checked(false);
				chkDR.set_Checked(false);
				((Control)updnSecond).set_Enabled(false);
				string text = lstEdit.get_Items().get_Item(((ListControl)lstEdit).get_SelectedIndex()).ToString();
				updnYear.set_Value(Convert.ToDecimal(text.Substring(213, 4)));
				updnMonth.set_Value(Convert.ToDecimal(text.Substring(217, 2)));
				updnDay.set_Value(Convert.ToDecimal(text.Substring(219, 2)));
				updnHour.set_Value(Convert.ToDecimal(text.Substring(221, 2)));
				updnMinute.set_Value(Convert.ToDecimal(text.Substring(223, 2)));
				updnSecond.set_Value(Convert.ToDecimal(text.Substring(225, 6)));
				string text2 = text.Substring(231, 1);
				optSAO.set_Checked(text2 == "S");
				optXZ.set_Checked(text2 == "X");
				optZC.set_Checked(text2 == "R");
				optPlanet.set_Checked(text2 == "P");
				optAsteroid.set_Checked(text2 == "A");
				optUnknown.set_Checked(text2 == "U");
				((Control)txtStar).set_Text(text.Substring(232, 6));
				optE.set_Checked(text.Substring(238, 1) == "+");
				optW.set_Checked(!optE.get_Checked());
				updnLongD.set_Value(Convert.ToDecimal(text.Substring(239, 3)));
				updnLongM.set_Value(Convert.ToDecimal(text.Substring(242, 2)));
				updnLongSec.set_Value(Convert.ToDecimal(text.Substring(244, 4)));
				optN.set_Checked(text.Substring(249, 1) == "+");
				optS.set_Checked(!optN.get_Checked());
				updnLatD.set_Value(Convert.ToDecimal(text.Substring(250, 2)));
				updnLatM.set_Value(Convert.ToDecimal(text.Substring(252, 2)));
				updnLatS.set_Value(Convert.ToDecimal(text.Substring(254, 4)));
				string text3 = text.Substring(73, 1);
				optNoCorrection.set_Checked(text3 == " ");
				optA.set_Checked(text3 == "A");
				optB.set_Checked(text3 == "B");
				optC.set_Checked(text3 == "C");
				optD.set_Checked(text3 == "D");
				optEc.set_Checked(text3 == "E");
				optF.set_Checked(text3 == "F");
				optG.set_Checked(text3 == "G");
				optSs.set_Checked(text3 == "S");
				optT.set_Checked(text3 == "T");
				optU.set_Checked(text3 == "U");
				optWe.set_Checked(text3 == "W");
				optX.set_Checked(text3 == "X");
				optY.set_Checked(text3 == "Y");
				optZ.set_Checked(text3 == "Z");
				PopulatingNewEvent = false;
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void GetParameters(string InLine)
		{
			double result = 0.0;
			double num = 0.0;
			string text = "";
			if (InLine.Trim() == "")
			{
				InLine = ((!((Control)lstCheckedEdit).get_Visible()) ? lstEdit.get_Items().get_Item(((ListControl)lstEdit).get_SelectedIndex()).ToString() : ((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(((ListControl)lstCheckedEdit).get_SelectedIndex()).ToString());
			}
			int year = Convert.ToInt32(updnYear.get_Value());
			int month = Convert.ToInt32(updnMonth.get_Value());
			double day = Convert.ToDouble(updnDay.get_Value());
			int num2 = Convert.ToInt32(updnHour.get_Value());
			int num3 = Convert.ToInt32(updnMinute.get_Value());
			double num4 = Convert.ToDouble(updnSecond.get_Value());
			if (!double.TryParse(InLine.Substring(29, 4), out result))
			{
				result = 0.0;
			}
			text = InLine.Substring(33, 1);
			EventCode = InLine.Substring(26, 1);
			num = 0.0;
			if (text == "U")
			{
				num = result;
			}
			else if (text == "X")
			{
				num = ((!((EventCode == "D") | (EventCode == "B") | (EventCode == "F"))) ? 0.8 : 0.5);
			}
			JD = Utilities.JD_from_Date(year, month, day) + ((double)num2 + (double)num3 / 60.0 + (num4 - num) / 3600.0) / 24.0;
			if (!int.TryParse(((Control)txtStar).get_Text(), out StarNumber))
			{
				StarNumber = 0;
			}
			if (((Control)panelDoubles).get_Visible() & (((ListControl)cmbWDS_General).get_SelectedIndex() == 0))
			{
				WDScode = cmbWDS.get_Items().get_Item(((ListControl)cmbWDS).get_SelectedIndex()).ToString();
			}
			else
			{
				WDScode = cmbWDS_General.get_Items().get_Item(((ListControl)cmbWDS_General).get_SelectedIndex()).ToString();
			}
			Observer = InLine.Substring(188, 25);
			if (!int.TryParse(InLine.Substring(102, 3), out DatumNumber))
			{
				DatumNumber = 0;
			}
			Longitude = (Convert.ToDouble(updnLongD.get_Value()) + Convert.ToDouble(updnLongM.get_Value()) / 60.0 + Convert.ToDouble(updnLongSec.get_Value()) / 3600.0) / (180.0 / Math.PI);
			if (optW.get_Checked())
			{
				Longitude = 0.0 - Longitude;
			}
			Latitude = (Convert.ToDouble(updnLatD.get_Value()) + Convert.ToDouble(updnLatM.get_Value()) / 60.0 + Convert.ToDouble(updnLatS.get_Value()) / 3600.0) / (180.0 / Math.PI);
			if (optS.get_Checked())
			{
				Latitude = 0.0 - Latitude;
			}
			if (!double.TryParse(InLine.Substring(104, 6), out HeightSite))
			{
				HeightSite = 0.0;
			}
			VerticalDatumIsMSL = InLine.Substring(110, 6) == "M";
			StarCat = "X";
			if (optSAO.get_Checked())
			{
				StarCat = "S";
			}
			else if (optZC.get_Checked())
			{
				StarCat = "R";
			}
			else if (optPlanet.get_Checked())
			{
				StarCat = "P";
			}
			else if (optAsteroid.get_Checked())
			{
				StarCat = "A";
			}
			SiteLocation = InLine.Substring(127, 50).Trim() + " by " + InLine.Substring(188, 25).Trim();
			TelDia = InLine.Substring(180, 4).Trim();
		}

		private void optUnknown_CheckedChanged(object sender, EventArgs e)
		{
			((ListControl)cmbWDS_General).set_SelectedIndex(0);
			if (optUnknown.get_Checked())
			{
				((Control)txtStar).set_Text("");
			}
		}

		private void cmdAdjustedReduce_Click(object sender, EventArgs e)
		{
			GetParameters("");
			LunarObservations.ReduceAnObservation(JD, Longitude, Latitude, HeightSite, Height_is_MSL: true, DatumNumber, StarCat, StarNumber, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, WDScode, ApplyLimbCorrn: true, VizierReduction: false, out Reduction, out Residual, out var MoonAlt, out var SunAlt, out var Illumination, out var MagStar, out var _, out var _);
			Label obj = lblResidual;
			string text;
			((Control)lblResidual2).set_Text(text = Reduction.Substring(0, 62) + string.Format("   {0,3:F0}", MoonAlt * (180.0 / Math.PI)) + string.Format("      {0,3:F0}", SunAlt * (180.0 / Math.PI)) + string.Format("    {0,3:F0}%", Illumination) + string.Format("   {0,3:F1}", MagStar) + TelDia.PadLeft(6) + "cm    " + Observer.Trim());
			((Control)obj).set_Text(text);
		}

		private void cmdIdentify_Click(object sender, EventArgs e)
		{
			GetParameters("");
			string eventPhase = EventCode;
			if (chkDR.get_Checked())
			{
				if (EventCode == "D")
				{
					eventPhase = "R";
				}
				else if (EventCode == "R")
				{
					eventPhase = "D";
				}
			}
			LunarObservations.IdentifyXZStar(JD, Longitude, Latitude, HeightSite, DatumNumber, eventPhase, 180.0, fromAutoCorrect: false, fromManualDetect: true);
			cmdAdjustedReduce_Click(sender, e);
		}

		private void cmdGoogleEarth_Click(object sender, EventArgs e)
		{
			GetParameters("");
			if (GoogleEarth.Create_New_GoogleEarthKMZ_File(Utilities.AppPath + "\\Observations\\ArchiveCurrentSite.KML", "Sites", AutoOpenFile: true, out var CreatedFile))
			{
				GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML(SiteLocation, Longitude * (180.0 / Math.PI), Latitude * (180.0 / Math.PI));
				CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
				GoogleEarth.DisplayGoogleMap(CreatedFile);
			}
		}

		private void UpdateArchiveEventRecord()
		{
			string text = lstEdit.get_Items().get_Item(((ListControl)lstEdit).get_SelectedIndex()).ToString();
			string text2 = "+";
			string text3 = "+";
			string text4 = " ";
			string text5 = "    ";
			string text6 = "   ";
			bool flag = false;
			if (chkSecs.get_Checked())
			{
				NewTime = updnYear.get_Value().ToString().PadLeft(4) + updnMonth.get_Value().ToString().PadLeft(2) + updnDay.get_Value().ToString().PadLeft(2) + updnHour.get_Value().ToString().PadLeft(2) + updnMinute.get_Value().ToString().PadLeft(2) + string.Format("{0,4:F1}", updnSecond.get_Value()).PadLeft(4).PadRight(6);
			}
			else
			{
				NewTime = updnYear.get_Value().ToString().PadLeft(4) + updnMonth.get_Value().ToString().PadLeft(2) + updnDay.get_Value().ToString().PadLeft(2) + updnHour.get_Value().ToString().PadLeft(2) + updnMinute.get_Value().ToString().PadLeft(2) + text.Substring(12, 6);
			}
			NewStar = "R";
			if (optSAO.get_Checked())
			{
				NewStar = "S";
			}
			else if (optXZ.get_Checked())
			{
				NewStar = "X";
			}
			else if (optPlanet.get_Checked())
			{
				NewStar = "P";
			}
			else if (optAsteroid.get_Checked())
			{
				NewStar = "A";
			}
			else if (optUnknown.get_Checked())
			{
				NewStar = "U";
			}
			string text7 = cmbWDS_General.get_Items().get_Item(((ListControl)cmbWDS_General).get_SelectedIndex()).ToString()!.Substring(0, 1);
			NewStar = NewStar + ((Control)txtStar).get_Text().Trim().PadLeft(6)
				.Substring(0, 6) + text7;
			flag = chkAdjAcc.get_Checked();
			NewAccuracy = "".PadRight(5);
			string value = " ";
			if (flag)
			{
				if (updnAcc.get_Value() > 0m)
				{
					NewAccuracy = updnAcc.get_Value().ToString().PadRight(5);
				}
				if (((ListControl)cmbCertainty).get_SelectedIndex() > 0)
				{
					value = ((ListControl)cmbCertainty).get_SelectedIndex().ToString();
				}
			}
			if (optW.get_Checked())
			{
				text2 = "-";
			}
			if (optS.get_Checked())
			{
				text3 = "-";
			}
			NewSite = text2 + updnLongD.get_Value().ToString().PadLeft(3) + updnLongM.get_Value().ToString().PadLeft(2) + text.Substring(87, 5) + text3 + updnLatD.get_Value().ToString().PadLeft(2) + updnLatM.get_Value().ToString().PadLeft(2) + text.Substring(97, 5);
			string value2 = " ";
			if (optA.get_Checked())
			{
				value2 = "A";
			}
			else if (optB.get_Checked())
			{
				value2 = "B";
			}
			else if (optC.get_Checked())
			{
				value2 = "C";
			}
			else if (optD.get_Checked())
			{
				value2 = "D";
			}
			else if (optEc.get_Checked())
			{
				value2 = "E";
			}
			else if (optF.get_Checked())
			{
				value2 = "F";
			}
			else if (optG.get_Checked())
			{
				value2 = "G";
			}
			else if (optSs.get_Checked())
			{
				value2 = "S";
			}
			else if (optT.get_Checked())
			{
				value2 = "T";
			}
			else if (optU.get_Checked())
			{
				value2 = "U";
			}
			else if (optWe.get_Checked())
			{
				value2 = "W";
			}
			else if (optX.get_Checked())
			{
				value2 = "X";
			}
			else if (optY.get_Checked())
			{
				value2 = "Y";
			}
			else if (optZ.get_Checked())
			{
				value2 = "Z";
			}
			if (chkGraze.get_Checked())
			{
				text4 = "G";
				text5 = NewTime.Substring(0, 4);
				text6 = ((Control)txtGrazeNumber).get_Text().Trim().PadLeft(3)
					.Substring(0, 3);
			}
			else
			{
				text4 = " ";
				text5 = "    ";
				text6 = "   ";
			}
			string text8 = text.Remove(0, 18).Insert(0, NewTime);
			text8 = text8.Remove(18, 8).Insert(18, NewStar);
			if (updnYear.get_Value() < 1750m)
			{
				if (flag)
				{
					text8 = text8.Remove(37, 5).Insert(37, NewAccuracy);
					text8 = text8.Remove(42, 1).Insert(42, value);
				}
				if (chkReReducedOldObs.get_Checked())
				{
					text8 = text8.Remove(71, 1).Insert(71, "Q");
				}
			}
			text8 = text8.Remove(73, 1).Insert(73, value2);
			text8 = text8.Remove(81, 21).Insert(81, NewSite);
			text8 = text8.Remove(28, 1).Insert(28, text4);
			text8 = text8.Remove(74, 4).Insert(74, text5);
			text8 = text8.Remove(78, 3).Insert(78, text6);
			FileStream fileStream = new FileStream(LargeResidualFile, FileMode.Open, FileAccess.Write);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			int num = LargeResidualRecord[((ListControl)lstEdit).get_SelectedIndex()];
			fileStream.Seek(270 * num, SeekOrigin.Begin);
			Out = text8.Substring(0, 110).ToCharArray();
			binaryWriter.Write(Out);
			fileStream.Close();
			lstEdit.get_Items().set_Item(((ListControl)lstEdit).get_SelectedIndex(), (object)text8);
		}

		private void cmdCorrect_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Update this record in the file?", "Update Record", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				UpdateArchiveEventRecord();
			}
		}

		private void chkSecs_CheckedChanged(object sender, EventArgs e)
		{
			((Control)updnSecond).set_Enabled(chkSecs.get_Checked());
		}

		private void cmdGeneratePrediction_Click(object sender, EventArgs e)
		{
			LunarOccultations.LunarPredictionsShowForm();
			LunarOccultations.LunarPrediction.optXZ.set_Checked(true);
			LunarOccultations.LunarPrediction.chkAsteroids.set_Checked(updnYear.get_Value() > 1968m);
			LunarOccultations.LunarPrediction.chkPlanets.set_Checked(true);
			LunarOccultations.LunarPrediction.chkStars.set_Checked(true);
			LunarOccultations.LunarPrediction.radioButton2.set_Checked(true);
			LunarOccultations.LunarPrediction.updnLunarYearStart.set_Value(updnYear.get_Value());
			((ListControl)LunarOccultations.LunarPrediction.cmbLunarMonthStart).set_SelectedIndex((int)updnMonth.get_Value() - 1);
			((ListControl)LunarOccultations.LunarPrediction.cmbLunarDayStart).set_SelectedIndex((int)updnDay.get_Value() - 1);
			LunarOccultations.LunarPrediction.ShowSingleSiteBox();
			((Control)LunarOccultations.LunarPrediction.txtLongD).set_Text(updnLongD.get_Value().ToString());
			if (optW.get_Checked())
			{
				((Control)LunarOccultations.LunarPrediction.txtLongD).set_Text("-" + ((Control)LunarOccultations.LunarPrediction.txtLongD).get_Text());
			}
			((Control)LunarOccultations.LunarPrediction.txtLongM).set_Text(updnLongM.get_Value().ToString());
			((Control)LunarOccultations.LunarPrediction.txtLongS).set_Text(updnLongSec.get_Value().ToString());
			((Control)LunarOccultations.LunarPrediction.txtLatD).set_Text(updnLatD.get_Value().ToString());
			if (optS.get_Checked())
			{
				((Control)LunarOccultations.LunarPrediction.txtLatD).set_Text("-" + ((Control)LunarOccultations.LunarPrediction.txtLatD).get_Text());
			}
			((Control)LunarOccultations.LunarPrediction.txtLatM).set_Text(updnLatM.get_Value().ToString());
			((Control)LunarOccultations.LunarPrediction.txtLatS).set_Text(updnLatS.get_Value().ToString());
			((Control)LunarOccultations.LunarPrediction.txtAltitude).set_Text("300");
			LunarOccultations.LunarPrediction.updnAperture.set_Value(30m);
			LunarOccultations.LunarPrediction.cmdCompute_Click(sender, e);
		}

		private void updnYear_ValueChanged(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void updnMonth_ValueChanged(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void updnDay_ValueChanged(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void updnHour_ValueChanged(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void updnMinute_ValueChanged(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void updnSecond_ValueChanged(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void optW_CheckedChanged(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void optZC_Click(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void optSAO_Click(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void optXZ_Click(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void optPlanet_Click(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				((ListControl)cmbWDS_General).set_SelectedIndex(0);
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void optAsteroid_Click(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				((ListControl)cmbWDS_General).set_SelectedIndex(0);
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void txtStar_TextChanged(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void cmbWDS_General_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (!PopulatingNewEvent)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void cmdPutEventsInEditor_Click(object sender, EventArgs e)
		{
			bool rGODisplayed = true;
			string text = lstEdit.get_Items().get_Item(((ListControl)lstEdit).get_SelectedIndex()).ToString();
			string text2 = text.Substring(120, 5);
			string text3 = text.Substring(127, 50).Trim();
			if (text3 == "")
			{
				text3 = "RGO Site " + text2;
			}
			string observerLine = "OA  " + text.Substring(188, 25).Trim();
			string siteLine = "TA  " + text.Substring(177, 3) + " " + text.Substring(180, 4) + "  " + text.Substring(184, 4) + "  " + text.Substring(81, 11) + " " + text.Substring(92, 10) + " " + text.Substring(102, 2) + " " + text.Substring(104, 7);
			if (text2.Trim().Length == 0)
			{
				text2 = lstEdit.get_Items().get_Item(((ListControl)lstEdit).get_SelectedIndex()).ToString()!.Substring(111, 9);
				if (text.Substring(127, 50).Trim() == "")
				{
					text3 = "ILOC Site " + text2;
				}
				rGODisplayed = false;
			}
			LunarObservations.CreateHistoricalFileInEditor(text2, rGODisplayed, "", siteLine, observerLine, text3, Merge: false, OccultationReport.UseOldFormat);
		}

		private void cmdSortArchive_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000c: Expected O, but got Unknown
			//IL_0023: Unknown result type (might be due to invalid IL or missing references)
			//IL_0029: Invalid comparison between Unknown and I4
			ClearLists();
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Archive file to Sort");
			((FileDialog)val).set_FileName("*.*");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				SortFile(((FileDialog)val).get_FileName());
			}
		}

		private void SortFile(string SourceFileName)
		{
			ArrayList arrayList = new ArrayList();
			Cursor.set_Current(Cursors.get_WaitCursor());
			using (StreamReader streamReader = new StreamReader(SourceFileName))
			{
				while (!streamReader.EndOfStream)
				{
					arrayList.Add(streamReader.ReadLine());
				}
			}
			arrayList.Sort();
			using (StreamWriter streamWriter = new StreamWriter(Path.GetDirectoryName(SourceFileName) + "\\S_" + Path.GetFileName(SourceFileName)))
			{
				for (int i = 0; i < arrayList.Count; i++)
				{
					streamWriter.WriteLine(arrayList[i]!.ToString());
				}
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdMergeArchiveFiles_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000c: Expected O, but got Unknown
			//IL_002a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0030: Invalid comparison between Unknown and I4
			//IL_004a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0050: Expected O, but got Unknown
			//IL_006e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0074: Invalid comparison between Unknown and I4
			ClearLists();
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_Title("Select new name for merged files");
			((FileDialog)val).set_FileName("*.*");
			val.set_OverwritePrompt(true);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				if (File.Exists(((FileDialog)val).get_FileName()))
				{
					return;
				}
				Cursor.set_Current(Cursors.get_WaitCursor());
				OpenFileDialog val2 = new OpenFileDialog();
				((FileDialog)val2).set_Title("Select Archive files to merge");
				((FileDialog)val2).set_FileName("*.*");
				val2.set_Multiselect(true);
				if ((int)((CommonDialog)val2).ShowDialog() == 1)
				{
					for (int i = 0; i <= ((FileDialog)val2).get_FileNames().GetUpperBound(0); i++)
					{
						File.AppendAllText(((FileDialog)val).get_FileName(), File.ReadAllText(((FileDialog)val2).get_FileNames()[i]));
					}
				}
				Cursor.set_Current(Cursors.get_Default());
			}
			SortFile(((FileDialog)val).get_FileName());
		}

		private void saveListToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveList(Append: false);
		}

		private void appendListToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveList(Append: true);
		}

		private void SaveList(bool Append)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0042: Unknown result type (might be due to invalid IL or missing references)
			//IL_0048: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_Title("Select new name for sublist file");
			((FileDialog)val).set_FileName(Path.GetDirectoryName(LargeResidualFile) + "\\Sublist_" + Path.GetFileName(LargeResidualFile));
			val.set_OverwritePrompt(!Append);
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			using StreamWriter streamWriter = new StreamWriter(((FileDialog)val).get_FileName(), Append);
			if (((Control)lstEdit).get_Visible())
			{
				for (int i = 0; i < lstEdit.get_Items().get_Count(); i++)
				{
					streamWriter.WriteLine(lstEdit.get_Items().get_Item(i));
				}
			}
			else
			{
				for (int j = 0; j < ((ObjectCollection)lstCheckedEdit.get_Items()).get_Count(); j++)
				{
					streamWriter.WriteLine(((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(j));
				}
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Archive observations editor");
		}

		private void optRGO_CheckedChanged(object sender, EventArgs e)
		{
			SetSiteSelectionFields();
		}

		private void optILOC_CheckedChanged(object sender, EventArgs e)
		{
			SetSiteSelectionFields();
		}

		private void optListObserver_CheckedChanged(object sender, EventArgs e)
		{
			SetSiteSelectionFields();
		}

		private void optListSite_CheckedChanged(object sender, EventArgs e)
		{
			SetSiteSelectionFields();
		}

		private void SetSiteSelectionFields()
		{
			((Control)panelRGO).set_Visible(optRGO.get_Checked());
			((Control)panelILOC).set_Visible(optILOC.get_Checked());
			((Control)txtListObserver).set_Visible(optListObserver.get_Checked());
			((Control)txtListSiteName).set_Visible(optListSite.get_Checked());
			((Control)PanelILOCsiteReplace).set_Visible(((Control)panelILOC).get_Visible());
		}

		private void cmdRetrieveRGOsite_Click(object sender, EventArgs e)
		{
			string text;
			if (((Control)panelRGO).get_Visible())
			{
				text = ((Control)txtRGO1).get_Text().Trim().PadLeft(3, '0') + " " + ((Control)txtRGO2).get_Text().Trim().PadLeft(2, '0');
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\Archive RGO Sites.dat");
				do
				{
					string text2 = streamReader.ReadLine();
					if (text == text2.Substring(0, 6))
					{
						((Control)txtLonD2).set_Text(text2.Substring(7, 4).Replace(" ", ""));
						((Control)txtLonM2).set_Text(text2.Substring(12, 2).Replace(" ", ""));
						((Control)txtLonS2).set_Text(text2.Substring(15, 5).Replace(" ", ""));
						((Control)txtLatD2).set_Text(text2.Substring(20, 3).Replace(" ", ""));
						((Control)txtLatM2).set_Text(text2.Substring(24, 2).Replace(" ", ""));
						((Control)txtLatS2).set_Text(text2.Substring(27, 5).Replace(" ", ""));
						((Control)txtAlt2).set_Text(text2.Substring(33, 4));
						((ListControl)cmbDatum2).set_SelectedIndex(int.Parse(text2.Substring(38, 1)));
						chkCoords.set_Checked(true);
						break;
					}
				}
				while (!streamReader.EndOfStream);
				return;
			}
			text = ((Control)txtILOC1).get_Text().Trim().ToUpper()
				.PadRight(5) + ((Control)txtILOC2).get_Text().Trim().PadLeft(2) + ((Control)txtILOC3).get_Text().Trim().PadLeft(2);
			using StreamReader streamReader2 = new StreamReader(Utilities.AppPath + "\\Resource Files\\Archive ILOC Sites.dat");
			do
			{
				string text2 = streamReader2.ReadLine();
				if (!(text == text2.Substring(0, 9)))
				{
					continue;
				}
				((Control)txtLonD2).set_Text(text2.Substring(10, 4).Replace(" ", ""));
				((Control)txtLonM2).set_Text(text2.Substring(14, 2).Replace(" ", ""));
				((Control)txtLonS2).set_Text(text2.Substring(16, 5).Replace(" ", ""));
				((Control)txtLatD2).set_Text(text2.Substring(21, 3).Replace(" ", ""));
				((Control)txtLatM2).set_Text(text2.Substring(24, 2).Replace(" ", ""));
				((Control)txtLatS2).set_Text(text2.Substring(26, 5).Replace(" ", ""));
				((Control)txtAlt2).set_Text(text2.Substring(33, 4));
				string value = text2.Substring(31, 2);
				for (int i = 0; i < cmbDatum2.get_Items().get_Count(); i++)
				{
					if (cmbDatum2.get_Items().get_Item(i).ToString()!.Substring(0, 4).Contains(value))
					{
						((ListControl)cmbDatum2).set_SelectedIndex(i);
						break;
					}
					((ListControl)cmbDatum2).set_SelectedIndex(0);
				}
				if (text2.Substring(39, 1) == "E")
				{
					((ListControl)cmbAltDatum2).set_SelectedIndex(1);
				}
				else
				{
					((ListControl)cmbAltDatum2).set_SelectedIndex(0);
				}
				chkCoords.set_Checked(true);
				break;
			}
			while (!streamReader2.EndOfStream);
		}

		private void cmdRetrieveNewILOCsite_Click(object sender, EventArgs e)
		{
			string text = ((Control)txtILOC1replace).get_Text().Trim().ToUpper()
				.PadRight(5) + ((Control)txtILOC2replace).get_Text().Trim().PadLeft(2) + ((Control)txtILOC3replace).get_Text().Trim().PadLeft(2);
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\Archive ILOC Sites.dat"))
			{
				do
				{
					string text2 = streamReader.ReadLine();
					if (!(text == text2.Substring(0, 9)))
					{
						continue;
					}
					((Control)txtLonD2).set_Text(text2.Substring(10, 4).Replace(" ", ""));
					((Control)txtLonM2).set_Text(text2.Substring(14, 2).Replace(" ", ""));
					((Control)txtLonS2).set_Text(text2.Substring(16, 5).Replace(" ", ""));
					((Control)txtLatD2).set_Text(text2.Substring(21, 3).Replace(" ", ""));
					((Control)txtLatM2).set_Text(text2.Substring(24, 2).Replace(" ", ""));
					((Control)txtLatS2).set_Text(text2.Substring(26, 5).Replace(" ", ""));
					((Control)txtAlt2).set_Text(text2.Substring(33, 4));
					string value = text2.Substring(31, 2);
					for (int i = 0; i < cmbDatum2.get_Items().get_Count(); i++)
					{
						if (cmbDatum2.get_Items().get_Item(i).ToString()!.Substring(0, 4).Contains(value))
						{
							((ListControl)cmbDatum2).set_SelectedIndex(i);
							break;
						}
						((ListControl)cmbDatum2).set_SelectedIndex(0);
					}
					if (text2.Substring(39, 1) == "E")
					{
						((ListControl)cmbAltDatum2).set_SelectedIndex(1);
					}
					else
					{
						((ListControl)cmbAltDatum2).set_SelectedIndex(0);
					}
					((Control)txtSite2).set_Text(text2.Substring(40, 50));
					((Control)txtTelescope).set_Text(text2.Substring(90, 11));
					chkCorrectILOCsites.set_Checked(true);
					chkCoords.set_Checked(true);
					chkChangeTelescope.set_Checked(true);
					chkCorrectSite.set_Checked(true);
					break;
				}
				while (!streamReader.EndOfStream);
			}
			text = ((Control)txtILOC1replace).get_Text().Trim().ToUpper()
				.PadRight(5) + ((Control)txtILOC2replace).get_Text().Trim().PadLeft(2) + ((Control)txtILOC3replace).get_Text().Trim().PadLeft(2);
			using StreamReader streamReader2 = new StreamReader(Utilities.AppPath + "\\Resource Files\\Archive ILOC Observers.dat");
			do
			{
				string text2 = streamReader2.ReadLine();
				if (text == text2.Substring(0, 9))
				{
					((Control)txtObs2).set_Text(text2.Substring(10).Trim() + " ?");
					chkCorrectNames.set_Checked(true);
					break;
				}
			}
			while (!streamReader2.EndOfStream);
		}

		private void cmdGetSiteEvents_Click(object sender, EventArgs e)
		{
			//IL_0021: Unknown result type (might be due to invalid IL or missing references)
			//IL_0027: Expected O, but got Unknown
			//IL_00dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e2: Invalid comparison between Unknown and I4
			bool flag = false;
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			ClearLists();
			Cursor.set_Current(Cursors.get_WaitCursor());
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select one or more Archive files to edit sites");
			((FileDialog)val).set_FileName("*.*");
			val.set_Multiselect(true);
			RGOtest = ((Control)txtRGO1).get_Text().PadLeft(3, '0').Substring(0, 3) + ((Control)txtRGO2).get_Text().PadLeft(2, '0').Substring(0, 2);
			ILOCtest = ((Control)txtILOC1).get_Text().PadRight(5).Substring(0, 5)
				.ToUpper() + ((Control)txtILOC2).get_Text().PadLeft(2).Substring(0, 2) + ((Control)txtILOC3).get_Text().PadLeft(2).Substring(0, 2);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Site_ArchiveFileRecord.Clear();
				for (int i = 0; i <= ((FileDialog)val).get_FileNames().GetUpperBound(0); i++)
				{
					using StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileNames()[i]);
					int num = 0;
					while (!streamReader.EndOfStream)
					{
						string text = streamReader.ReadLine();
						flag = false;
						if (optRGO.get_Checked())
						{
							if (text.Substring(120, 5) == RGOtest)
							{
								flag = true;
							}
						}
						else if (optILOC.get_Checked())
						{
							if (text.Substring(111, 9) == ILOCtest)
							{
								flag = true;
							}
						}
						else if (optListObserver.get_Checked())
						{
							if (text.Substring(188, 25).ToLower().Contains(((Control)txtListObserver).get_Text().ToLower()))
							{
								flag = true;
							}
						}
						else if (optListSite.get_Checked() && text.Substring(127, 50).Contains(((Control)txtListSiteName).get_Text()))
						{
							flag = true;
						}
						if (flag)
						{
							lstCheckedEdit.SetItemChecked(((ObjectCollection)lstCheckedEdit.get_Items()).Add((object)text), true);
							AFR = new Archive_File_Record();
							AFR.File = ((FileDialog)val).get_FileNames()[i];
							AFR.Record = num;
							Site_ArchiveFileRecord.Add(AFR);
						}
						num++;
					}
				}
			}
			Cursor.set_Current(Cursors.get_Default());
			if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Count() < 1)
			{
				return;
			}
			string text2 = ((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(0).ToString();
			TextBox obj = txtLonD1;
			string text3;
			((Control)txtLonD2).set_Text(text3 = text2.Substring(238, 4));
			((Control)obj).set_Text(text3);
			TextBox obj2 = txtLonM1;
			((Control)txtLonM2).set_Text(text3 = text2.Substring(242, 2));
			((Control)obj2).set_Text(text3);
			TextBox obj3 = txtLonS1;
			((Control)txtLonS2).set_Text(text3 = text2.Substring(244, 5));
			((Control)obj3).set_Text(text3);
			TextBox obj4 = txtLatD1;
			((Control)txtLatD2).set_Text(text3 = text2.Substring(249, 3));
			((Control)obj4).set_Text(text3);
			TextBox obj5 = txtLatM1;
			((Control)txtLatM2).set_Text(text3 = text2.Substring(252, 2));
			((Control)obj5).set_Text(text3);
			TextBox obj6 = txtLatS1;
			((Control)txtLatS2).set_Text(text3 = text2.Substring(254, 5));
			((Control)obj6).set_Text(text3);
			string value = text2.Substring(259, 2);
			for (int j = 0; j < cmbDatum1.get_Items().get_Count(); j++)
			{
				if (cmbDatum1.get_Items().get_Item(j).ToString()!.Substring(0, 4).Contains(value))
				{
					((ListControl)cmbDatum1).set_SelectedIndex(j);
					((ListControl)cmbDatum2).set_SelectedIndex(j);
					break;
				}
				((ListControl)cmbDatum1).set_SelectedIndex(0);
				((ListControl)cmbDatum2).set_SelectedIndex(j);
			}
			TextBox obj7 = txtAlt1;
			((Control)txtAlt2).set_Text(text3 = text2.Substring(261, 6));
			((Control)obj7).set_Text(text3);
			if (text2.Substring(267, 1) == "E")
			{
				ComboBox obj8 = cmbAltDatum1;
				int selectedIndex;
				((ListControl)cmbAltDatum2).set_SelectedIndex(selectedIndex = 1);
				((ListControl)obj8).set_SelectedIndex(selectedIndex);
			}
			else
			{
				ComboBox obj9 = cmbAltDatum1;
				int selectedIndex;
				((ListControl)cmbAltDatum2).set_SelectedIndex(selectedIndex = 0);
				((ListControl)obj9).set_SelectedIndex(selectedIndex);
			}
			TextBox obj10 = txtObs1;
			((Control)txtObs2).set_Text(text3 = text2.Substring(188, 25).Trim());
			((Control)obj10).set_Text(text3);
			TextBox obj11 = txtSite1;
			((Control)txtSite2).set_Text(text3 = text2.Substring(127, 50).Trim());
			((Control)obj11).set_Text(text3);
			CheckBox obj12 = chkCorrectILOCsites;
			CheckBox obj13 = chkCoords;
			CheckBox obj14 = chkCorrectNames;
			CheckBox obj15 = chkCorrectSite;
			bool flag2;
			chkChangeTelescope.set_Checked(flag2 = false);
			bool flag3;
			obj15.set_Checked(flag3 = flag2);
			bool flag4;
			obj14.set_Checked(flag4 = flag3);
			bool @checked;
			obj13.set_Checked(@checked = flag4);
			obj12.set_Checked(@checked);
			((Control)txtILOC1replace).set_Text(((Control)txtILOC1).get_Text());
			((Control)txtILOC2replace).set_Text(((Control)txtILOC2).get_Text());
			((Control)txtILOC3replace).set_Text(((Control)txtILOC3).get_Text());
			((Control)lblOldCoords).set_Text(text2.Substring(238, 30));
			((Control)lblCurrentCoords).set_Text(text2.Substring(81, 30));
			Cursor.set_Current(Cursors.get_Default());
		}

		private void txtObs2_TextChanged(object sender, EventArgs e)
		{
			((Control)lblObsLeft).set_Text((25 - ((Control)txtObs2).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
		}

		private void txtSite2_TextChanged(object sender, EventArgs e)
		{
			((Control)lblSiteLeft).set_Text((50 - ((Control)txtSite2).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
		}

		private void cmdGoogleEarthSites_Click(object sender, EventArgs e)
		{
			if (GoogleEarth.Create_New_GoogleEarthKMZ_File(Utilities.AppPath + "\\Observations\\Archive_OldNew_Sites.KML", "Sites", AutoOpenFile: true, out var CreatedFile))
			{
				if (!double.TryParse(((Control)txtLonD1).get_Text().Replace("+", "").Replace("-", ""), out var result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLonM1).get_Text(), out var result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLonS1).get_Text(), out var result3))
				{
					result3 = 0.0;
				}
				double num = result + result2 / 60.0 + result3 / 3600.0;
				if (((Control)txtLonD1).get_Text().Contains("-"))
				{
					num = 0.0 - num;
				}
				if (!double.TryParse(((Control)txtLatD1).get_Text().Replace("+", "").Replace("-", ""), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatM1).get_Text(), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLatS1).get_Text(), out result3))
				{
					result3 = 0.0;
				}
				double num2 = result + result2 / 60.0 + result3 / 3600.0;
				if (((Control)txtLatD1).get_Text().Contains("-"))
				{
					num2 = 0.0 - num2;
				}
				if (!int.TryParse(cmbDatum1.get_Items().get_Item(((ListControl)cmbDatum1).get_SelectedIndex()).ToString()!.Substring(0, 2), out var result4))
				{
					result4 = 0;
				}
				Utilities.GetDatumToWGS84Corrections(Utilities.RGOdatum_to_ILOCdatum(result4, num, num2), num / (180.0 / Math.PI), num2 / (180.0 / Math.PI), 0.0, out var DLongitude_arcsec, out var DLatitude_arcsec);
				num += DLongitude_arcsec / 3600.0;
				num2 += DLatitude_arcsec / 3600.0;
				GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML("Old coords", num, num2);
				if (!double.TryParse(((Control)txtLonD2).get_Text().Replace("+", "").Replace("-", ""), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLonM2).get_Text(), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLonS2).get_Text(), out result3))
				{
					result3 = 0.0;
				}
				num = result + result2 / 60.0 + result3 / 3600.0;
				if (((Control)txtLonD2).get_Text().Contains("-"))
				{
					num = 0.0 - num;
				}
				if (!double.TryParse(((Control)txtLatD2).get_Text().Replace("+", "").Replace("-", ""), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatM2).get_Text(), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLatS2).get_Text(), out result3))
				{
					result3 = 0.0;
				}
				num2 = result + result2 / 60.0 + result3 / 3600.0;
				if (((Control)txtLatD1).get_Text().Contains("-"))
				{
					num2 = 0.0 - num2;
				}
				if (!int.TryParse(cmbDatum1.get_Items().get_Item(((ListControl)cmbDatum2).get_SelectedIndex()).ToString()!.Substring(0, 2), out result4))
				{
					result4 = 0;
				}
				Utilities.GetDatumToWGS84Corrections(Utilities.RGOdatum_to_ILOCdatum(result4, num, num2), num / (180.0 / Math.PI), num2 / (180.0 / Math.PI), 0.0, out DLongitude_arcsec, out DLatitude_arcsec);
				num += DLongitude_arcsec / 3600.0;
				num2 += DLatitude_arcsec / 3600.0;
				GoogleEarth.Write_PlaceMark_PinPlusName_GoogleEarthKML("New coords", num, num2);
				CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
				GoogleEarth.DisplayGoogleMap(CreatedFile);
			}
		}

		private string FormatSiteCoords()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (!int.TryParse(((Control)txtLonD2).get_Text().Replace("+", "").Replace("-", ""), out var result))
			{
				result = 0;
			}
			if (result > 359)
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtLonM2).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (result2 > 59)
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txtLonS2).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			if (result3 > 59.9)
			{
				result3 = 0.0;
			}
			if (((Control)txtLonD2).get_Text().Contains("-"))
			{
				stringBuilder.Append("-");
			}
			else
			{
				stringBuilder.Append("+");
			}
			stringBuilder.AppendFormat("{0,3:F0}", result);
			stringBuilder.AppendFormat("{0,2:F0}", result2);
			stringBuilder.AppendFormat("{0,4:F1} ", result3);
			if (!int.TryParse(((Control)txtLatD2).get_Text().Replace("+", "").Replace("-", ""), out result))
			{
				result = 0;
			}
			if (result > 90)
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtLatM2).get_Text(), out result2))
			{
				result2 = 0;
			}
			if (result2 > 59)
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txtLatS2).get_Text(), out result3))
			{
				result3 = 0.0;
			}
			if (result3 > 59.9)
			{
				result3 = 0.0;
			}
			if (((Control)txtLatD2).get_Text().Contains("-"))
			{
				stringBuilder.Append("-");
			}
			else
			{
				stringBuilder.Append("+");
			}
			stringBuilder.AppendFormat("{0,2:F0}", result);
			stringBuilder.AppendFormat("{0,2:F0}", result2);
			stringBuilder.AppendFormat("{0,4:F1} ", result3);
			stringBuilder.Append(cmbDatum2.get_Items().get_Item(((ListControl)cmbDatum2).get_SelectedIndex()).ToString()!.Substring(0, 2));
			if (!double.TryParse(((Control)txtAlt2).get_Text(), out result3))
			{
				result3 = 0.0;
			}
			if (result3 > 9999.0)
			{
				result3 = 0.0;
			}
			if (result3 < -999.0)
			{
				result3 = 0.0;
			}
			stringBuilder.AppendFormat("{0,4:F0}. ", result3);
			if (((ListControl)cmbAltDatum2).get_SelectedIndex() == 1)
			{
				stringBuilder.Append("E");
			}
			else
			{
				stringBuilder.Append("M");
			}
			return stringBuilder.ToString();
		}

		private void txtLonD2_TextChanged(object sender, EventArgs e)
		{
			((Control)lblNewCoords).set_Text(FormatSiteCoords());
		}

		private void txtLonM2_TextChanged(object sender, EventArgs e)
		{
			((Control)lblNewCoords).set_Text(FormatSiteCoords());
		}

		private void txtLonS2_TextChanged(object sender, EventArgs e)
		{
			((Control)lblNewCoords).set_Text(FormatSiteCoords());
		}

		private void txtLatD2_TextChanged(object sender, EventArgs e)
		{
			((Control)lblNewCoords).set_Text(FormatSiteCoords());
		}

		private void txtLatM2_TextChanged(object sender, EventArgs e)
		{
			((Control)lblNewCoords).set_Text(FormatSiteCoords());
		}

		private void txtLatS2_TextChanged(object sender, EventArgs e)
		{
			((Control)lblNewCoords).set_Text(FormatSiteCoords());
		}

		private void txtAlt2_TextChanged(object sender, EventArgs e)
		{
			((Control)lblNewCoords).set_Text(FormatSiteCoords());
		}

		private void cmbDatum2_SelectedIndexChanged(object sender, EventArgs e)
		{
			((Control)lblNewCoords).set_Text(FormatSiteCoords());
		}

		private void cmbAltDatum2_SelectedIndexChanged(object sender, EventArgs e)
		{
			((Control)lblNewCoords).set_Text(FormatSiteCoords());
		}

		private void cmdUpdateSites_Click(object sender, EventArgs e)
		{
			bool @checked = chkCoords.get_Checked();
			bool checked2 = chkCorrectNames.get_Checked();
			bool checked3 = chkCorrectSite.get_Checked();
			bool flag = chkCorrectILOCsites.get_Checked() & optILOC.get_Checked();
			bool checked4 = chkChangeTelescope.get_Checked();
			string text = ((Control)txtObs2).get_Text().TrimStart(Array.Empty<char>()).PadRight(25)
				.Substring(0, 25);
			string text2 = ((Control)txtSite2).get_Text().TrimStart(Array.Empty<char>()).PadRight(50)
				.Substring(0, 50);
			string text3 = ((Control)txtTelescope).get_Text().ToUpper().TrimStart(Array.Empty<char>())
				.PadRight(11)
				.Substring(0, 11);
			string text4 = FormatSiteCoords().PadRight(30).Substring(0, 30);
			string text5 = ((Control)txtILOC1replace).get_Text().Trim().ToUpper()
				.PadRight(5) + ((Control)txtILOC2replace).get_Text().Trim().PadLeft(2) + ((Control)txtILOC3replace).get_Text().Trim().PadLeft(2);
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			for (int i = 0; i < Site_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					FileStream fileStream = new FileStream(Site_ArchiveFileRecord[i].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
					BinaryWriter binaryWriter = new BinaryWriter(fileStream);
					BinaryReader binaryReader = new BinaryReader(fileStream);
					if (@checked)
					{
						fileStream.Seek(Site_ArchiveFileRecord[i].Record * 270 + 213, SeekOrigin.Begin);
						char[] chars = binaryReader.ReadChars(25);
						fileStream.Seek(Site_ArchiveFileRecord[i].Record * 270, SeekOrigin.Begin);
						binaryWriter.Write(chars);
						fileStream.Seek(Site_ArchiveFileRecord[i].Record * 270 + 73, SeekOrigin.Begin);
						binaryWriter.Write("D".ToCharArray());
						fileStream.Seek(Site_ArchiveFileRecord[i].Record * 270 + 81, SeekOrigin.Begin);
						binaryWriter.Write(text4.ToCharArray());
					}
					if (flag)
					{
						fileStream.Seek(Site_ArchiveFileRecord[i].Record * 270 + 111, SeekOrigin.Begin);
						binaryWriter.Write(text5.ToCharArray());
					}
					if (checked3)
					{
						fileStream.Seek(Site_ArchiveFileRecord[i].Record * 270 + 127, SeekOrigin.Begin);
						binaryWriter.Write(text2.ToCharArray());
					}
					if (checked4)
					{
						fileStream.Seek(Site_ArchiveFileRecord[i].Record * 270 + 177, SeekOrigin.Begin);
						binaryWriter.Write(text3.ToCharArray());
					}
					if (checked2)
					{
						fileStream.Seek(Site_ArchiveFileRecord[i].Record * 270 + 188, SeekOrigin.Begin);
						binaryWriter.Write(text.ToCharArray());
					}
					fileStream.Close();
				}
			}
			Cursor.set_Current(Cursors.get_Default());
			lstEdit.get_Items().Clear();
			LargeResidualRecord.Initialize();
			Site_ArchiveFileRecord.Clear();
		}

		private void cmbStarList_SelectedIndexChanged(object sender, EventArgs e)
		{
			CreateStarList(((ListControl)cmbStarList).get_SelectedIndex());
		}

		private void CreateStarList(int ListType)
		{
			if (!FormCreated)
			{
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			StarList.Clear();
			cmbNames.get_Items().Clear();
			cmbNames.set_Sorted(ListType != 1 && ListType != 2);
			StarList.Add(0);
			cmbNames.get_Items().Add((object)"");
			StreamReader streamReader = new StreamReader(ZCNameFile);
			do
			{
				string? text = streamReader.ReadLine();
				int num = int.Parse(text!.Substring(0, 4));
				string text2 = text!.Substring(5).Trim();
				int num2 = text2.IndexOf("=");
				int num3 = text2.IndexOf("=", num2 + 2);
				if (num3 > num2)
				{
					text2 = text2.Substring(0, num3 - 1);
				}
				int result;
				switch (ListType)
				{
				case 0:
					if (num2 > 0 && !int.TryParse(text2.Substring(0, 2), out result))
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 1:
					if (num2 > 0)
					{
						if (!int.TryParse(text2.Substring(num2 + 1, 2), out result))
						{
							int index = cmbNames.get_Items().Add((object)text2.Substring(num2 + 1));
							StarList.Insert(index, num);
						}
					}
					else if (!int.TryParse(text2.Substring(0, 2), out result))
					{
						int index = cmbNames.get_Items().Add((object)text2.Trim());
						StarList.Insert(index, num);
					}
					break;
				case 2:
				{
					int index = cmbNames.get_Items().Add((object)text2);
					StarList.Insert(index, num);
					break;
				}
				case 3:
					if (ZCMagnitude(num) < 2.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 4:
					if (ZCMagnitude(num) < 3.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 5:
					if (ZCMagnitude(num) < 4.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				}
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			Cursor.set_Current(Cursors.get_Default());
			((ListControl)cmbNames).set_SelectedIndex(1);
		}

		private double ZCMagnitude(int ZC)
		{
			XZ80Q.Get_ZC_Star(ZC);
			return XZ80Q.Mv;
		}

		private void SetcmbNames(int ZC)
		{
			for (int i = 1; i < cmbNames.get_Items().get_Count(); i++)
			{
				if (ZC == (int)StarList[i])
				{
					((ListControl)cmbNames).set_SelectedIndex(i);
					return;
				}
			}
			((ListControl)cmbNames).set_SelectedIndex(0);
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
				XZ80Q.Get_XZ_Star(result);
				((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				SetcmbNames(XZ80Q.ZC);
				GetDoubleDetails();
				ChangingStar = false;
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
				if (XZ80Q.Get_SAO_Star(result))
				{
					((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
					((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				}
				else
				{
					TextBox obj = txtZC;
					string text;
					((Control)txtXZ).set_Text(text = "0");
					((Control)obj).set_Text(text);
				}
				SetcmbNames(XZ80Q.ZC);
				GetDoubleDetails();
				ChangingStar = false;
			}
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
				XZ80Q.Get_ZC_Star(result);
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				SetcmbNames(XZ80Q.ZC);
				GetDoubleDetails();
				ChangingStar = false;
			}
		}

		private void cmbNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				XZ80Q.Get_ZC_Star((int)StarList[((ListControl)cmbNames).get_SelectedIndex()]);
				((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				GetDoubleDetails();
				ChangingStar = false;
			}
		}

		private void GetDoubleDetails()
		{
			lstDoubles.get_Items().Clear();
			if (!int.TryParse(((Control)txtXZ).get_Text(), out var result))
			{
				result = 0;
			}
			if (result < 0)
			{
				return;
			}
			XZ80Q.Get_XZ_Star(result);
			if (!(XZ80Q.DoubleFlag == " "))
			{
				DoubleStars.GetXZDoubleList(XZ80Q.XZ);
				((Control)labelDouble).set_Text(DoubleStars.XZDoubleList[0]!.ToString());
				for (int i = 1; i < DoubleStars.XZDoubleList.Count; i++)
				{
					lstDoubles.get_Items().Add((object)DoubleStars.XZDoubleList[i]!.ToString());
				}
			}
		}

		private void cmdReadForDoubles_Click(object sender, EventArgs e)
		{
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_0097: Expected O, but got Unknown
			//IL_00b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bf: Invalid comparison between Unknown and I4
			bool flag = false;
			bool @checked = chkDoublesOnly.get_Checked();
			string text = "X" + ((Control)txtXZ).get_Text().PadLeft(6);
			string text2 = "X" + ((Control)txtSAO).get_Text().PadLeft(6);
			string text3 = "R" + ((Control)txtZC).get_Text().PadLeft(6);
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			ClearLists();
			((ListControl)cmbWDS).set_SelectedIndex(0);
			Cursor.set_Current(Cursors.get_WaitCursor());
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Archive files to edit doubles");
			((FileDialog)val).set_FileName("*.*");
			val.set_Multiselect(true);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				for (int i = 0; i <= ((FileDialog)val).get_FileNames().GetUpperBound(0); i++)
				{
					using StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileNames()[i]);
					int num = 0;
					while (!streamReader.EndOfStream)
					{
						string text4 = streamReader.ReadLine();
						flag = false;
						if (text4.Substring(18, 7) == text3)
						{
							flag = true;
						}
						else if (text4.Substring(18, 7) == text2)
						{
							flag = true;
						}
						else if (text4.Substring(18, 7) == text)
						{
							flag = true;
						}
						if (flag && @checked)
						{
							flag = (text4.Substring(55, 1) == "1") | (text4.Substring(46, 1) != " ") | (text4.Substring(60, 1) == "5") | (text4.Substring(61, 1) == "8");
						}
						if (flag)
						{
							((ObjectCollection)lstCheckedEdit.get_Items()).Add((object)text4);
							AFR = new Archive_File_Record();
							AFR.File = ((FileDialog)val).get_FileNames()[i];
							AFR.Record = num;
							Star_ArchiveFileRecord.Add(AFR);
						}
						num++;
					}
				}
			}
			Cursor.set_Current(Cursors.get_Default());
			if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Count() > 0)
			{
				((ListControl)lstCheckedEdit).set_SelectedIndex(0);
			}
			((Control)lstCheckedEdit).Focus();
		}

		private void lstCheckedEdit_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstCheckedEdit).get_SelectedIndex() >= 0)
			{
				string listLine = ((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(((ListControl)lstCheckedEdit).get_SelectedIndex()).ToString();
				DecodeEventLine(listLine);
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void cmbWDS_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (FormCreated)
			{
				cmdAdjustedReduce_Click(sender, e);
			}
		}

		private void cmdStarsClearAll_Click(object sender, EventArgs e)
		{
			if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Count() >= 0)
			{
				for (int i = 0; i < ((ObjectCollection)lstCheckedEdit.get_Items()).get_Count(); i++)
				{
					lstCheckedEdit.SetItemCheckState(i, (CheckState)0);
				}
			}
		}

		private void cmdStarsCheckAll_Click(object sender, EventArgs e)
		{
			if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Count() >= 0)
			{
				for (int i = 0; i < ((ObjectCollection)lstCheckedEdit.get_Items()).get_Count(); i++)
				{
					lstCheckedEdit.SetItemCheckState(i, (CheckState)1);
				}
			}
		}

		private void cmdCheckDoubles_Click(object sender, EventArgs e)
		{
			string text = cmbDoubleCode.get_Items().get_Item(((ListControl)cmbDoubleCodes).get_SelectedIndex()).ToString()!.PadRight(1);
			for (int i = 0; i < ((ObjectCollection)lstCheckedEdit.get_Items()).get_Count(); i++)
			{
				if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Substring(46, 1) == text)
				{
					lstCheckedEdit.SetItemChecked(i, true);
				}
			}
		}

		private void cmdApplyWDS_Click(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			string text = cmbWDS.get_Items().get_Item(((ListControl)cmbWDS).get_SelectedIndex()).ToString()!.PadLeft(1);
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					FileStream fileStream = new FileStream(Star_ArchiveFileRecord[i].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
					BinaryWriter binaryWriter = new BinaryWriter(fileStream);
					fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 25, SeekOrigin.Begin);
					binaryWriter.Write(text.ToCharArray());
					((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(25, 1).Insert(25, text));
					fileStream.Close();
				}
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void checkAllToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!((Control)grpGrazeScopes).get_Visible())
			{
				for (int i = 0; i < ((ObjectCollection)lstCheckedEdit.get_Items()).get_Count(); i++)
				{
					lstCheckedEdit.SetItemChecked(i, true);
				}
			}
		}

		private void clearAllToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ClearAllCheckBoxes();
		}

		private void ClearAllCheckBoxes()
		{
			for (int i = 0; i < ((ObjectCollection)lstCheckedEdit.get_Items()).get_Count(); i++)
			{
				lstCheckedEdit.SetItemChecked(i, false);
			}
			((Control)lblnumberChecked).set_Text("0");
		}

		private void invertSelectionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (!((Control)grpGrazeScopes).get_Visible())
			{
				for (int i = 0; i < ((ObjectCollection)lstCheckedEdit.get_Items()).get_Count(); i++)
				{
					lstCheckedEdit.SetItemChecked(i, !lstCheckedEdit.GetItemChecked(i));
				}
			}
		}

		private void checkAllSourcedFromOToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CheckSpecificSource("O");
		}

		private void checkAllSourcedFrToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CheckSpecificSource("X");
		}

		private void checkAllSourcedFromSToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CheckSpecificSource("S");
		}

		private void CheckSpecificSource(string Source)
		{
			for (int i = 0; i < ((ObjectCollection)lstCheckedEdit.get_Items()).get_Count(); i++)
			{
				if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Substring(71, 1) == Source)
				{
					lstCheckedEdit.SetItemChecked(i, true);
				}
				else
				{
					lstCheckedEdit.SetItemChecked(i, false);
				}
			}
		}

		private void txtILOC1_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtILOC1).SelectAll();
		}

		private void txtILOC2_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtILOC2).SelectAll();
		}

		private void txtILOC3_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtILOC3).SelectAll();
		}

		private void txtILOC1replace_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtILOC1replace).SelectAll();
		}

		private void txtILOC2replace_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtILOC2replace).SelectAll();
		}

		private void txtILOC3replace_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtILOC3replace).SelectAll();
		}

		private void txtEventYear_MouseEnter(object sender, EventArgs e)
		{
			((TextBoxBase)txtEventYear).SelectAll();
		}

		private void txtEventYear_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtEventYear).SelectAll();
		}

		private void txtEventMonth_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtEventMonth).SelectAll();
		}

		private void txtEventMonth_MouseEnter(object sender, EventArgs e)
		{
			((TextBoxBase)txtEventMonth).SelectAll();
		}

		private void txtILOCSearchSite_MouseEnter(object sender, EventArgs e)
		{
			((TextBoxBase)txtILOCSearchSite).SelectAll();
		}

		private void txtILOCSearchSite_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtILOCSearchSite).SelectAll();
		}

		private void cmdSelectArchiveForGrazeEdit_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			//IL_000c: Expected O, but got Unknown
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0023: Invalid comparison between Unknown and I4
			//IL_004c: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_00df: Invalid comparison between Unknown and I4
			string text = "";
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_InitialDirectory(Settings.Default.Open_WorkingArchive);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				if (Path.GetDirectoryName(((FileDialog)val).get_FileName())!.Contains(Utilities.AppPath))
				{
					MessageBox.Show("This functionality cannot be run against files in the Occult 4 directory", "Error", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return;
				}
				ClearLists();
				LoadingGrazeList = true;
				GrazeEditPath = Path.GetDirectoryName(((FileDialog)val).get_FileName());
				Settings.Default.Open_WorkingArchive = GrazeEditPath;
				GrazeArchiveFile = ((FileDialog)val).get_FileName();
				((Control)lblGrazeFile).set_Text(Path.GetFileName(((FileDialog)val).get_FileName()));
				cmbGrazeList.get_Items().Clear();
				text = GrazeEditPath + "\\Local Graze Index.dat";
				if (!File.Exists(text))
				{
					if ((int)MessageBox.Show("Do you want to create a local index file?", "No local index file", (MessageBoxButtons)4, (MessageBoxIcon)48) == 7)
					{
						return;
					}
					LunarObservations.CreateSingleFileGrazeIndexFile();
				}
				if (!File.Exists(text))
				{
					return;
				}
				using (StreamReader streamReader = new StreamReader(GrazeEditPath + "\\Local Graze Index.dat"))
				{
					do
					{
						string text2 = streamReader.ReadLine();
						streamReader.ReadLine();
						string text3 = streamReader.ReadLine();
						string text4 = streamReader.ReadLine();
						streamReader.ReadLine();
						cmbGrazeList.get_Items().Add((object)(text2 + ", " + text3 + ",   " + text4));
					}
					while (!streamReader.EndOfStream);
				}
				if (cmbGrazeList.get_Items().get_Count() > 0)
				{
					CheckEditedGrazesInList();
					((ListControl)cmbGrazeList).set_SelectedIndex(0);
				}
				LoadingGrazeList = false;
			}
			EventsForDatumsLoaded = false;
		}

		private void CheckEditedGrazesInList()
		{
			if (!File.Exists(GrazeEditPath + "\\Local Grazes Edited.dat"))
			{
				return;
			}
			bool loadingGrazeList = LoadingGrazeList;
			LoadingGrazeList = true;
			using (StreamReader streamReader = new StreamReader(GrazeEditPath + "\\Local Grazes Edited.dat"))
			{
				while (!streamReader.EndOfStream)
				{
					string text = streamReader.ReadLine();
					for (int i = 0; i < cmbGrazeList.get_Items().get_Count(); i++)
					{
						if (text == cmbGrazeList.get_Items().get_Item(i).ToString()!.Substring(0, 19))
						{
							cmbGrazeList.get_Items().set_Item(i, (object)cmbGrazeList.get_Items().get_Item(i).ToString()!.Remove(22, 1).Insert(22, "*"));
							break;
						}
					}
				}
			}
			LoadingGrazeList = loadingGrazeList;
		}

		private void cmbGrazeList_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (LoadingGrazeList)
			{
				return;
			}
			string text = cmbGrazeList.get_Items().get_Item(((ListControl)cmbGrazeList).get_SelectedIndex()).ToString();
			chkDoublesOnly.get_Checked();
			string text2 = text.Substring(0, 4) + text.Substring(5, 2) + text.Substring(8, 2);
			string text3 = text.Substring(12, 7);
			if (!int.TryParse(text.Substring(0, 4), out var result))
			{
				result = 0;
			}
			if (!int.TryParse(text.Substring(5, 2), out var result2))
			{
				result2 = 0;
			}
			if (!double.TryParse(text.Substring(8, 2), out var result3))
			{
				result3 = 0.0;
			}
			Utilities.Date_from_JD(Utilities.JD_from_Date(result, result2, result3) + 1.0, out result, out result2, out result3);
			string text4 = string.Format("{0,4:f0}", result) + string.Format("{0,2:f0}", result2) + string.Format("{0,2:f0}", result3);
			string text5 = text.Substring(19, 1).Replace("#", "2");
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			ClearLists();
			GrazeID = "       ";
			Cursor.set_Current(Cursors.get_WaitCursor());
			using (StreamReader streamReader = new StreamReader(GrazeArchiveFile))
			{
				int num = 0;
				while (!streamReader.EndOfStream)
				{
					string text6 = streamReader.ReadLine();
					if (text6.Substring(18, 7) == text3 && (!chkUseGrazeIDonly.get_Checked() | (text5 == text6.Substring(70, 1))) && ((text6.Substring(0, 8) == text2) | (text6.Substring(0, 8) == text4)))
					{
						((ObjectCollection)lstCheckedEdit.get_Items()).Add((object)text6);
						AFR = new Archive_File_Record();
						AFR.File = GrazeArchiveFile;
						AFR.Record = num;
						Star_ArchiveFileRecord.Add(AFR);
						if (GrazeID == "       ")
						{
							GrazeID = text6.Substring(74, 7);
						}
					}
					num++;
				}
			}
			Cursor.set_Current(Cursors.get_Default());
			if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Count() > 0)
			{
				((ListControl)lstCheckedEdit).set_SelectedIndex(0);
			}
			((Control)lstCheckedEdit).Focus();
			((Control)lblGrazeID).set_Text("ID = " + GrazeID);
			if (chkAutoDisplay.get_Checked())
			{
				LunarObservations.ShowGrazesInArchiveEditorInEditor(ExcludeStartEnd: false, ExcludeBad: false);
			}
		}

		private void cmdRecordGrazeAsEdited_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to mark this event as having been edited?", "Confirm Mark as edited", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			int selectedIndex = ((ListControl)cmbGrazeList).get_SelectedIndex();
			string text = cmbGrazeList.get_Items().get_Item(((ListControl)cmbGrazeList).get_SelectedIndex()).ToString()!.Substring(0, 19);
			bool flag = false;
			LoadingGrazeList = true;
			if (File.Exists(GrazeEditPath + "\\Local Grazes Edited.dat"))
			{
				using StreamReader streamReader = new StreamReader(GrazeEditPath + "\\Local Grazes Edited.dat");
				while (!streamReader.EndOfStream)
				{
					flag = streamReader.ReadLine() == text;
					if (flag)
					{
						break;
					}
				}
			}
			if (!flag)
			{
				using StreamWriter streamWriter = new StreamWriter(GrazeEditPath + "\\Local Grazes Edited.dat", append: true);
				streamWriter.WriteLine(text);
				cmbGrazeList.get_Items().set_Item(((ListControl)cmbGrazeList).get_SelectedIndex(), (object)cmbGrazeList.get_Items().get_Item(((ListControl)cmbGrazeList).get_SelectedIndex()).ToString()!.Remove(22, 1).Insert(22, "*"));
			}
			LoadingGrazeList = false;
			if (chkAutoIncrement.get_Checked() && selectedIndex < cmbGrazeList.get_Items().get_Count() - 2)
			{
				((ListControl)cmbGrazeList).set_SelectedIndex(selectedIndex + 1);
			}
		}

		private void optGraze_Grazeflag_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optGraze_NullSecs_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optGraze_LargeResiduals_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optGraze_Duplicates_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optGrazeSiteDetails_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optGrazeEventCode_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optMarkAs2ndLimit_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optGraze_RenumberGrazes_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optTagGrazes_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optTelescopeCodes_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optGrazeSetDoubles_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optGrazeSites_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optDatums_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void optGrazeEventDuration_CheckedChanged(object sender, EventArgs e)
		{
			setGrazeEditEnabled();
		}

		private void setGrazeEditEnabled()
		{
			if (EventsForDatumsLoaded)
			{
				ClearLists();
				EventsForDatumsLoaded = false;
			}
			((Control)grpGrazeFlag).set_Visible(optGraze_Grazeflag.get_Checked());
			((Control)grpNullSeconds).set_Visible(optGraze_NullSecs.get_Checked());
			((Control)grpLargeResiduals).set_Visible(optGraze_LargeResiduals.get_Checked());
			((Control)grpDuplicateGrazeEvents).set_Visible(optGraze_Duplicates.get_Checked());
			((Control)cmdClearDuplicatteResidualFlags).set_Visible(optGraze_LargeResiduals.get_Checked() | optGraze_Duplicates.get_Checked());
			((Control)grpGrazeRenumber).set_Visible(optGraze_RenumberGrazes.get_Checked());
			((Control)cmdTagAllGrazeEvents).set_Visible(optTagGrazes.get_Checked());
			((Control)grpDatums).set_Visible(optDatums.get_Checked());
			((Control)grp2ndLimit).set_Visible(optMarkAs2ndLimit.get_Checked());
			((Control)grpGrazeScopes).set_Visible(optGrazeSiteDetails.get_Checked());
			((Control)grpGrazeSites).set_Visible(optGrazeSites.get_Checked());
			((Control)lblnumberChecked).set_Text("0");
			((Control)grpGrazeDR).set_Visible(optGrazeEventCode.get_Checked());
			((Control)grpTelescopeCodes).set_Visible(optTelescopeCodes.get_Checked());
			((Control)grpGrazeDoubles).set_Visible(optGrazeSetDoubles.get_Checked());
			((Control)grpEventDuration).set_Visible(optGrazeEventDuration.get_Checked());
		}

		private void cmdSetGrazeFlags_Click(object sender, EventArgs e)
		{
			SetClearGrazeFlag(SetFlag: true);
		}

		private void cmdClearGrazeFlags_Click(object sender, EventArgs e)
		{
			SetClearGrazeFlag(SetFlag: false);
		}

		private void SetClearGrazeFlag(bool SetFlag)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			string text = " ";
			if (SetFlag)
			{
				text = "G";
			}
			string text2 = "".PadRight(7);
			if (SetFlag)
			{
				text2 = GrazeID;
			}
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					FileStream fileStream = new FileStream(Star_ArchiveFileRecord[i].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
					BinaryWriter binaryWriter = new BinaryWriter(fileStream);
					fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 28, SeekOrigin.Begin);
					binaryWriter.Write(text.ToCharArray());
					((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(28, 1).Insert(28, text));
					if (GrazeID.Length == 7)
					{
						fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 74, SeekOrigin.Begin);
						binaryWriter.Write(text2.ToCharArray());
						((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(74, 7).Insert(74, text2));
					}
					fileStream.Close();
					lstCheckedEdit.SetItemChecked(i, false);
				}
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdSet2ndLimit_Click(object sender, EventArgs e)
		{
			SetClear2ndLimitFlag(SetFlag: true);
		}

		private void cmdClear2ndLimit_Click(object sender, EventArgs e)
		{
			SetClear2ndLimitFlag(SetFlag: false);
		}

		private void SetClear2ndLimitFlag(bool SetFlag)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			string text = " ";
			if (SetFlag)
			{
				text = "2";
			}
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					FileStream fileStream = new FileStream(Star_ArchiveFileRecord[i].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
					BinaryWriter binaryWriter = new BinaryWriter(fileStream);
					fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 70, SeekOrigin.Begin);
					binaryWriter.Write(text.ToCharArray());
					((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(70, text.Length).Insert(70, text));
					fileStream.Close();
					lstCheckedEdit.SetItemChecked(i, false);
				}
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdFindBlankSecs_Click(object sender, EventArgs e)
		{
			ClearAllCheckBoxes();
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Substring(12, 6) == "      ")
				{
					lstCheckedEdit.SetItemChecked(i, true);
				}
			}
		}

		private void cmdSetZeroSecs_Click(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			string text = "0.";
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					FileStream fileStream = new FileStream(Star_ArchiveFileRecord[i].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
					BinaryWriter binaryWriter = new BinaryWriter(fileStream);
					fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 13, SeekOrigin.Begin);
					binaryWriter.Write(text.ToCharArray());
					((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(13, text.Length).Insert(13, text));
					fileStream.Close();
					lstCheckedEdit.SetItemChecked(i, false);
				}
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdGrazeEventsToEditor_Click(object sender, EventArgs e)
		{
			if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Count() >= 1)
			{
				LunarObservations.ShowGrazesInArchiveEditorInEditor(ExcludeStartEnd: false, ExcludeBad: false);
			}
		}

		private void cmdGrazeEventsInEditor_Valid_Click(object sender, EventArgs e)
		{
			if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Count() >= 1)
			{
				LunarObservations.ShowGrazesInArchiveEditorInEditor(ExcludeStartEnd: true, ExcludeBad: true);
			}
		}

		private void cmdGrazeEventsInEditor_No_StartEnd_Click(object sender, EventArgs e)
		{
			if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Count() >= 1)
			{
				LunarObservations.ShowGrazesInArchiveEditorInEditor(ExcludeStartEnd: true, ExcludeBad: false);
			}
		}

		private void cmdMarkLargeResiduals_Click(object sender, EventArgs e)
		{
			ClearAllCheckBoxes();
			bool @checked = optLimbResidual.get_Checked();
			double num;
			double num2;
			if (@checked)
			{
				num = (double)updnLimbCorrectedResidual.get_Value();
				num2 = 0.0 - num;
			}
			else
			{
				num = (double)updnSmoothTop.get_Value();
				num2 = (double)updnSmoothBottom.get_Value();
			}
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				string text = ((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString();
				if ((text.Substring(28, 1) == "G") & ("MSEO".IndexOf(text.Substring(26, 1)) < 0))
				{
					DecodeEventLine(text);
					GetParameters("");
					LunarObservations.ReduceAnObservation(JD, Longitude, Latitude, HeightSite, Height_is_MSL: true, DatumNumber, StarCat, StarNumber, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, WDScode, @checked, VizierReduction: false, out Reduction, out Residual, out var _, out var _, out var _, out var _, out var _, out var _);
					if ((Residual < num2) | (Residual > num))
					{
						lstCheckedEdit.SetItemChecked(i, true);
					}
				}
			}
		}

		private void cmdSetFlagLargeResiduals_Click(object sender, EventArgs e)
		{
			SetClearGrazeValidityFlag(InvalidObservation: true, DuplicateDelete: false, DuplicateKeep: false, ClearFlag: false);
		}

		private void cmdGrazeSetDuplicateToDelete_Click(object sender, EventArgs e)
		{
			SetClearGrazeValidityFlag(InvalidObservation: false, DuplicateDelete: true, DuplicateKeep: false, ClearFlag: false);
		}

		private void cmdGrazeSetDuplicateToKeep_Click(object sender, EventArgs e)
		{
			SetClearGrazeValidityFlag(InvalidObservation: false, DuplicateDelete: false, DuplicateKeep: true, ClearFlag: false);
		}

		private void cmdClearDuplicatteResidualFlags_Click(object sender, EventArgs e)
		{
			SetClearGrazeValidityFlag(InvalidObservation: false, DuplicateDelete: false, DuplicateKeep: false, ClearFlag: true);
		}

		private void SetClearGrazeValidityFlag(bool InvalidObservation, bool DuplicateDelete, bool DuplicateKeep, bool ClearFlag)
		{
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			string text = "";
			if (InvalidObservation)
			{
				text = "I";
			}
			else if (DuplicateDelete)
			{
				text = "D";
			}
			else if (DuplicateKeep)
			{
				text = "K";
			}
			else if (ClearFlag)
			{
				text = " ";
			}
			if (text.Length != 1)
			{
				MessageBox.Show("No replacement character set", "Error");
				return;
			}
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					FileStream fileStream = new FileStream(Star_ArchiveFileRecord[i].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
					BinaryWriter binaryWriter = new BinaryWriter(fileStream);
					fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 72, SeekOrigin.Begin);
					binaryWriter.Write(text.ToCharArray());
					((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(72, text.Length).Insert(72, text));
					fileStream.Close();
					lstCheckedEdit.SetItemChecked(i, false);
				}
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdRenumberGrazes_Click(object sender, EventArgs e)
		{
			RenumberGrazes();
		}

		private void RenumberGrazes()
		{
			if (GrazeArchiveFile.Length < 2)
			{
				return;
			}
			FileStream fileStream = new FileStream(GrazeArchiveFile, FileMode.Open, FileAccess.ReadWrite);
			BinaryReader binaryReader = new BinaryReader(fileStream);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			string text = "";
			string text2 = "       ";
			string text3 = "";
			string text4 = "";
			string[] array = new string[6] { "", "", "", "", "", "" };
			string[] array2 = new string[6] { "", "", "", "", "", "" };
			int num = 0;
			for (int i = 0; i < fileStream.Length / 270; i++)
			{
				fileStream.Seek(i * 270 + 28, SeekOrigin.Begin);
				string text5 = new string(binaryReader.ReadChars(1));
				fileStream.Seek(i * 270, SeekOrigin.Begin);
				string text6 = new string(binaryReader.ReadChars(4));
				fileStream.Seek(i * 270 + 6, SeekOrigin.Begin);
				text = new string(binaryReader.ReadChars(2));
				if (text != text4)
				{
					text4 = text;
					for (int num2 = 5; num2 > 0; num2--)
					{
						array[num2] = array[num2 - 1];
						array2[num2] = array2[num2 - 1];
					}
					array[0] = "-1000";
				}
				if (text6 != text3)
				{
					text3 = text6;
					num = 0;
				}
				if (text5 == "G")
				{
					fileStream.Seek(i * 270 + 18, SeekOrigin.Begin);
					string text7 = new string(binaryReader.ReadChars(7));
					fileStream.Seek(i * 270 + 70, SeekOrigin.Begin);
					string text8 = new string(binaryReader.ReadChars(1));
					text7 = text7 + " " + text8;
					fileStream.Seek(i * 270 + 74, SeekOrigin.Begin);
					new string(binaryReader.ReadChars(7));
					bool flag = false;
					for (int j = 0; j <= 5; j++)
					{
						if (text7 == array[j])
						{
							text2 = array2[j];
							flag = true;
							break;
						}
					}
					if (!flag)
					{
						num++;
						for (int num3 = 5; num3 > 0; num3--)
						{
							array[num3] = array[num3 - 1];
							array2[num3] = array2[num3 - 1];
						}
						array[0] = text7;
						text2 = (array2[0] = text6 + string.Format("{0,3:f0}", num));
					}
					fileStream.Seek(i * 270 + 74, SeekOrigin.Begin);
					binaryWriter.Write(text2.ToCharArray());
				}
				else
				{
					fileStream.Seek(i * 270 + 74, SeekOrigin.Begin);
					if (new string(binaryReader.ReadChars(7)).Trim().Length > 0)
					{
						fileStream.Seek(i * 270 + 74, SeekOrigin.Begin);
						binaryWriter.Write("".PadRight(7).ToCharArray());
					}
				}
			}
			fileStream.Close();
		}

		private void cmdTagAllGrazeEvents_Click(object sender, EventArgs e)
		{
			//IL_0036: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Invalid comparison between Unknown and I4
			//IL_0353: Unknown result type (might be due to invalid IL or missing references)
			//IL_0359: Invalid comparison between Unknown and I4
			int[] array = new int[5000];
			int num = 0;
			int num2 = 0;
			if ((int)MessageBox.Show("This function will flag all unidentified graze events in\r\n" + GrazeArchiveFile + "\r\r\r\nDo you want to continue?", "Confirm graze tagging", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			PBar pBar = new PBar();
			((Form)pBar).set_StartPosition((FormStartPosition)1);
			((Form)pBar).set_TopMost(true);
			((Control)pBar).Show();
			((Control)pBar).set_Text("Graze tagging " + Path.GetFileName(GrazeArchiveFile));
			pBar.pBarFTP.set_Minimum(0);
			using (StreamReader streamReader = new StreamReader(GrazeArchiveFile))
			{
				int num3 = (int)streamReader.BaseStream.Length / 270;
				pBar.pBarFTP.set_Maximum(num3);
				pBar.pBarFTP.set_Value(0);
				for (int i = 0; i < num3; i++)
				{
					pBar.pBarFTP.set_Value(i);
					if (i % 1000 == 0)
					{
						Application.DoEvents();
					}
					string text = streamReader.ReadLine();
					if (text.Substring(26, 1) == "M" || !(text.Substring(28, 1) != "G"))
					{
						continue;
					}
					DecodeEventLine(text);
					GetParameters(text);
					if (!LunarObservations.ReduceAnObservation(JD, Longitude, Latitude, HeightSite, Height_is_MSL: true, DatumNumber, StarCat, StarNumber, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, WDScode, ApplyLimbCorrn: false, VizierReduction: false, out Reduction, out this.Residual, out var MoonAlt, out var SunAlt, out var Illumination, out var MagStar, out var PA, out var MoonRadius) || Math.Abs(this.Residual) > 100.0)
					{
						continue;
					}
					LunarObservations.ReduceAnObservation(JD + 0.00011574074074074075, Longitude, Latitude, HeightSite, Height_is_MSL: true, DatumNumber, StarCat, StarNumber, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, WDScode, ApplyLimbCorrn: false, VizierReduction: false, out Reduction, out var Residual, out MoonAlt, out SunAlt, out Illumination, out MagStar, out var PA2, out MoonRadius);
					double num4 = (Residual - this.Residual) / 10.0;
					double num5 = PA2 - PA;
					if (num5 > 180.0)
					{
						num5 -= 360.0;
					}
					if (num5 < -180.0)
					{
						num5 += 360.0;
					}
					double num6 = Math.Atan2(Math.Abs(MoonRadius * 3600.0 * num5 / 10.0), Math.Abs(num4)) * (180.0 / Math.PI);
					if (num4 > 0.0)
					{
						num6 = 180.0 - num6;
					}
					num6 = Math.Abs(Math.Abs(num6) - 90.0);
					if (num6 < 4.5)
					{
						array[num] = i;
						num++;
						if ("DR".IndexOf(text.Substring(26, 1)) >= 0)
						{
							num2++;
						}
					}
				}
			}
			((Form)pBar).Close();
			if ((int)MessageBox.Show(num + " events for graze-tagging have been found.\r\nOf these " + num2 + " events were D or R events.\r\n\r\nDo you want to write the tags to the file: \r\n" + GrazeArchiveFile, "Confirm graze tagging", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				string text2 = "G";
				FileStream fileStream = new FileStream(GrazeArchiveFile, FileMode.Open, FileAccess.Write);
				BinaryWriter binaryWriter = new BinaryWriter(fileStream);
				for (int j = 0; j < num; j++)
				{
					fileStream.Seek(array[j] * 270 + 28, SeekOrigin.Begin);
					binaryWriter.Write(text2.ToCharArray());
				}
				fileStream.Close();
			}
		}

		private void ReadForDatums(bool zerodatum, bool SomaEllipsoid)
		{
			//IL_0021: Unknown result type (might be due to invalid IL or missing references)
			//IL_0027: Expected O, but got Unknown
			//IL_0045: Unknown result type (might be due to invalid IL or missing references)
			//IL_004b: Invalid comparison between Unknown and I4
			bool flag = false;
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			ClearLists();
			Cursor.set_Current(Cursors.get_WaitCursor());
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Archive files to edit datums");
			((FileDialog)val).set_FileName("*.*");
			val.set_Multiselect(false);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				using StreamReader streamReader = new StreamReader(((FileDialog)val).get_FileName());
				int num = 0;
				while (!streamReader.EndOfStream)
				{
					string text = streamReader.ReadLine();
					flag = false;
					if (zerodatum)
					{
						flag = text.Substring(102, 2) == " 0";
					}
					if (SomaEllipsoid)
					{
						flag = (text.Substring(71, 1) == "S") & (text.Substring(102, 2) == "84");
					}
					if (flag)
					{
						((ObjectCollection)lstCheckedEdit.get_Items()).Add((object)text);
						AFR = new Archive_File_Record();
						AFR.File = ((FileDialog)val).get_FileName();
						AFR.Record = num;
						Star_ArchiveFileRecord.Add(AFR);
					}
					num++;
				}
			}
			Cursor.set_Current(Cursors.get_Default());
			if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Count() > 0)
			{
				((ListControl)lstCheckedEdit).set_SelectedIndex(0);
			}
			((Control)lstCheckedEdit).Focus();
			EventsForDatumsLoaded = true;
		}

		private void cmdFindSomaEllipsoid_Click(object sender, EventArgs e)
		{
			ReadForDatums(zerodatum: false, SomaEllipsoid: true);
			Button obj = cmdSetSomaVerticaltoMSL;
			bool enabled;
			((Control)cmdSetSomaVerticalToEllipsoid).set_Enabled(enabled = true);
			((Control)obj).set_Enabled(enabled);
			Button obj2 = cmdSetSelectedToDatum;
			((Control)cmbHorizontalDatum2).set_Enabled(enabled = false);
			((Control)obj2).set_Enabled(enabled);
		}

		private void cmdFindZeroDatums_Click(object sender, EventArgs e)
		{
			ReadForDatums(zerodatum: true, SomaEllipsoid: false);
			Button obj = cmdSetSomaVerticaltoMSL;
			bool enabled;
			((Control)cmdSetSomaVerticalToEllipsoid).set_Enabled(enabled = false);
			((Control)obj).set_Enabled(enabled);
			Button obj2 = cmdSetSelectedToDatum;
			((Control)cmbHorizontalDatum2).set_Enabled(enabled = true);
			((Control)obj2).set_Enabled(enabled);
		}

		private void cmdSetSomaVerticalToEllipsoid_Click(object sender, EventArgs e)
		{
			WriteDatumUpdates(" 0", SetVerticalDatum: true, DatumIsMSL: false);
		}

		private void cmdSetSomaVerticaltoMSL_Click(object sender, EventArgs e)
		{
			WriteDatumUpdates(" 0", SetVerticalDatum: true, DatumIsMSL: true);
		}

		private void cmdSetSelectedToDatum_Click(object sender, EventArgs e)
		{
			WriteDatumUpdates(cmbHorizontalDatum2.get_Items().get_Item(((ListControl)cmbHorizontalDatum2).get_SelectedIndex()).ToString()!.Substring(0, 2), SetVerticalDatum: false, DatumIsMSL: true);
		}

		private void WriteDatumUpdates(string HorizontalDatum, bool SetVerticalDatum, bool DatumIsMSL)
		{
			//IL_002c: Unknown result type (might be due to invalid IL or missing references)
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			string text = "E";
			if (DatumIsMSL)
			{
				text = "M";
			}
			if (HorizontalDatum.Length != 2)
			{
				MessageBox.Show("Invalid Datum Number");
				return;
			}
			FileStream fileStream = new FileStream(Star_ArchiveFileRecord[0].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					if (SetVerticalDatum)
					{
						fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 110, SeekOrigin.Begin);
						binaryWriter.Write(text.ToCharArray());
						fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 267, SeekOrigin.Begin);
						binaryWriter.Write(text.ToCharArray());
						((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(110, 1).Insert(110, text).Remove(267, 1)
							.Insert(267, text));
					}
					else
					{
						fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 102, SeekOrigin.Begin);
						binaryWriter.Write(HorizontalDatum.ToCharArray());
						fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 259, SeekOrigin.Begin);
						binaryWriter.Write(HorizontalDatum.ToCharArray());
						((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(102, 2).Insert(102, HorizontalDatum).Remove(259, 2)
							.Insert(259, HorizontalDatum));
					}
					lstCheckedEdit.SetItemChecked(i, false);
				}
			}
			fileStream.Close();
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdGrazeSitesCheckeSameAsFirst_Click(object sender, EventArgs e)
		{
			TagGrazesSameAsFirst(EditSites: false);
		}

		private void TagGrazesSameAsFirst(bool EditSites)
		{
			int num = 0;
			string text = "";
			string text2 = "";
			((Control)lblnumberChecked).set_Text("0");
			int num2 = 0;
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					text = ((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString();
					break;
				}
			}
			if (text.Length < 20)
			{
				return;
			}
			string text3 = text.Substring(71, 1);
			string text4 = text.Substring(81, 21);
			string text5 = text.Substring(188, 25);
			text2 = text;
			((Control)txtAperture).set_Text(text.Substring(180, 4).Trim());
			((Control)txtFocalLength).set_Text(text.Substring(184, 4).Trim());
			num = " RNCO".IndexOf(text.Substring(177, 1));
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbTelescope).set_SelectedIndex(num);
			num = " EA".IndexOf(text.Substring(178, 1));
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbMount).set_SelectedIndex(num);
			num = " DM".IndexOf(text.Substring(179, 1));
			if (num < 0)
			{
				num = 0;
			}
			((ListControl)cmbDrive).set_SelectedIndex(num);
			((Control)txtName).set_Text(text.Substring(188, 25).Trim());
			((Control)txtPlace).set_Text(text.Substring(127, 50));
			for (int j = 0; j < Star_ArchiveFileRecord.Count; j++)
			{
				text = ((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(j).ToString();
				if ((text.Substring(71, 1) == text3) & (text.Substring(81, 21) == text4) & (text.Substring(188, 25) == text5))
				{
					lstCheckedEdit.SetItemChecked(j, true);
					num2++;
				}
				else
				{
					lstCheckedEdit.SetItemChecked(j, false);
				}
			}
			if (EditSites)
			{
				((Control)lblnumberChecked2).set_Text(num2.ToString());
				((Control)txtLonD3).set_Text(text2.Substring(81, 4).Replace(" ", ""));
				((Control)txtLonM3).set_Text(text2.Substring(85, 2).Replace(" ", ""));
				((Control)txtLonS3).set_Text(text2.Substring(87, 5).Replace(" ", ""));
				((Control)txtLatD3).set_Text(text2.Substring(92, 3).Replace(" ", ""));
				((Control)txtLatM3).set_Text(text2.Substring(95, 2).Replace(" ", ""));
				((Control)txtLatS3).set_Text(text2.Substring(97, 5).Replace(" ", ""));
				((Control)txtAlt3).set_Text(text2.Substring(104, 4));
				string value = text2.Substring(102, 2);
				for (int k = 0; k < cmbDatum3.get_Items().get_Count(); k++)
				{
					if (cmbDatum3.get_Items().get_Item(k).ToString()!.Substring(0, 4).Contains(value))
					{
						((ListControl)cmbDatum3).set_SelectedIndex(k);
						break;
					}
					((ListControl)cmbDatum3).set_SelectedIndex(0);
				}
				if (text2.Substring(110, 1) == "E")
				{
					((ListControl)cmbAltDatum3).set_SelectedIndex(1);
				}
				else
				{
					((ListControl)cmbAltDatum3).set_SelectedIndex(0);
				}
			}
			else
			{
				((Control)lblnumberChecked).set_Text(num2.ToString());
			}
		}

		private void txtName_TextChanged(object sender, EventArgs e)
		{
			((Control)lblObserverName).set_Text((25 - ((Control)txtName).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
		}

		private void txtName_KeyUp(object sender, KeyEventArgs e)
		{
			((Control)lblObserverName).set_Text((25 - ((Control)txtName).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
		}

		private void txtName_Enter(object sender, EventArgs e)
		{
			((Control)lblObserverName).set_Text((25 - ((Control)txtName).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
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
			((Control)lblCharsLeft).set_Text((50 - ((Control)txtPlace).get_Text().TrimEnd(Array.Empty<char>()).Length).ToString());
		}

		private void cmdGrazeUpdateSites_Click(object sender, EventArgs e)
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(((Control)txtPlace).get_Text().Trim().PadRight(50)
				.Substring(0, 50));
			if (((ListControl)cmbTelescope).get_SelectedIndex() < 0)
			{
				((ListControl)cmbTelescope).set_SelectedIndex(0);
			}
			stringBuilder.Append(" RNCO".Substring(((ListControl)cmbTelescope).get_SelectedIndex(), 1));
			if (((ListControl)cmbMount).get_SelectedIndex() < 0)
			{
				((ListControl)cmbMount).set_SelectedIndex(0);
			}
			stringBuilder.Append(" EA".Substring(((ListControl)cmbMount).get_SelectedIndex(), 1));
			if (((ListControl)cmbDrive).get_SelectedIndex() < 0)
			{
				((ListControl)cmbDrive).set_SelectedIndex(0);
			}
			stringBuilder.Append(" DM".Substring(((ListControl)cmbDrive).get_SelectedIndex(), 1));
			if (!double.TryParse(((Control)txtAperture).get_Text(), out var result))
			{
				result = 0.0;
			}
			if (result <= 0.0)
			{
				stringBuilder.Append("    ");
			}
			else
			{
				stringBuilder.AppendFormat("{0,4:F0}", result);
			}
			if (!double.TryParse(((Control)txtFocalLength).get_Text(), out result))
			{
				result = 0.0;
			}
			if (result <= 0.0)
			{
				stringBuilder.Append("    ");
			}
			else
			{
				stringBuilder.AppendFormat("{0,4:F0}", result);
			}
			stringBuilder.Append(((Control)txtName).get_Text().Trim().PadRight(25)
				.Substring(0, 25));
			string text = stringBuilder.ToString();
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			string text2 = "G";
			FileStream fileStream = new FileStream(Star_ArchiveFileRecord[0].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 69, SeekOrigin.Begin);
					binaryWriter.Write(text2.ToCharArray());
					fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 127, SeekOrigin.Begin);
					binaryWriter.Write(text.ToCharArray());
					((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(127, text.Length).Insert(127, text).Remove(69, 1)
						.Insert(69, text2));
					lstCheckedEdit.SetItemChecked(i, false);
				}
			}
			fileStream.Close();
			Cursor.set_Current(Cursors.get_Default());
			((Control)lblnumberChecked).set_Text("0");
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

		private void cmdGrazesSetEvent_Click(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			string text = " DRBFSEMO".Substring(((ListControl)cmbGrazeEvents).get_SelectedIndex(), 1);
			FileStream fileStream = new FileStream(Star_ArchiveFileRecord[0].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 26, SeekOrigin.Begin);
					binaryWriter.Write(text.ToCharArray());
					((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(26, text.Length).Insert(26, text));
					lstCheckedEdit.SetItemChecked(i, false);
				}
			}
			fileStream.Close();
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdGrazesGetTelescopes_Click(object sender, EventArgs e)
		{
		}

		private void cmdGrazeSitesAutoCorrect_Click(object sender, EventArgs e)
		{
		}

		private void cmdGrazeSitesCheckeSameAsFirst_2_Click(object sender, EventArgs e)
		{
			TagGrazesSameAsFirst(EditSites: true);
			chkSomaGrazeClearChecks.set_Checked(true);
		}

		private void cmdGrazeChangeSites_Click(object sender, EventArgs e)
		{
			string text = FormatGrazeSiteCoords().PadRight(30).Substring(0, 30);
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			FileStream fileStream = new FileStream(Star_ArchiveFileRecord[0].File.ToString(), FileMode.Open, FileAccess.Write);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 73, SeekOrigin.Begin);
					binaryWriter.Write("D".ToCharArray());
					fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 81, SeekOrigin.Begin);
					binaryWriter.Write(text.ToCharArray());
					((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(73, 1).Insert(73, "D").Remove(81, text.Length)
						.Insert(81, text));
					if (chkSomaGrazeClearChecks.get_Checked())
					{
						lstCheckedEdit.SetItemChecked(i, false);
					}
				}
			}
			fileStream.Close();
			Cursor.set_Current(Cursors.get_Default());
			((Control)lblnumberChecked2).set_Text("0");
		}

		private string FormatGrazeSiteCoords()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (!int.TryParse(((Control)txtLonD3).get_Text().Replace("+", "").Replace("-", ""), out var result))
			{
				result = 0;
			}
			if (result > 359)
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtLonM3).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (result2 > 59)
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txtLonS3).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			if (result3 > 59.9)
			{
				result3 = 0.0;
			}
			if (((Control)txtLonD3).get_Text().Contains("-"))
			{
				stringBuilder.Append("-");
			}
			else
			{
				stringBuilder.Append("+");
			}
			stringBuilder.AppendFormat("{0,3:F0}", result);
			stringBuilder.AppendFormat("{0,2:F0}", result2);
			stringBuilder.AppendFormat("{0,4:F1} ", result3);
			if (!int.TryParse(((Control)txtLatD3).get_Text().Replace("+", "").Replace("-", ""), out result))
			{
				result = 0;
			}
			if (result > 90)
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtLatM3).get_Text(), out result2))
			{
				result2 = 0;
			}
			if (result2 > 59)
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txtLatS3).get_Text(), out result3))
			{
				result3 = 0.0;
			}
			if (result3 > 59.9)
			{
				result3 = 0.0;
			}
			if (((Control)txtLatD3).get_Text().Contains("-"))
			{
				stringBuilder.Append("-");
			}
			else
			{
				stringBuilder.Append("+");
			}
			stringBuilder.AppendFormat("{0,2:F0}", result);
			stringBuilder.AppendFormat("{0,2:F0}", result2);
			stringBuilder.AppendFormat("{0,4:F1} ", result3);
			stringBuilder.Append(cmbDatum3.get_Items().get_Item(((ListControl)cmbDatum3).get_SelectedIndex()).ToString()!.Substring(0, 2));
			if (!double.TryParse(((Control)txtAlt3).get_Text(), out result3))
			{
				result3 = 0.0;
			}
			if (result3 > 9999.0)
			{
				result3 = 0.0;
			}
			if (result3 < -999.0)
			{
				result3 = 0.0;
			}
			stringBuilder.AppendFormat("{0,4:F0}. ", result3);
			if (((ListControl)cmbAltDatum3).get_SelectedIndex() == 1)
			{
				stringBuilder.Append("E");
			}
			else
			{
				stringBuilder.Append("M");
			}
			return stringBuilder.ToString();
		}

		private void txtLonD3_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLonD3).SelectAll();
		}

		private void txtLonD3_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLonD3).SelectAll();
		}

		private void txtLonM3_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLonM3).SelectAll();
		}

		private void txtLonM3_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLonM3).SelectAll();
		}

		private void txtLonS3_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLonS3).SelectAll();
		}

		private void txtLonS3_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLonS3).SelectAll();
		}

		private void txtLatD3_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatD3).SelectAll();
		}

		private void txtLatD3_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLatD3).SelectAll();
		}

		private void txtLatM3_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatM3).SelectAll();
		}

		private void txtLatM3_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLatM3).SelectAll();
		}

		private void txtLatS3_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtLatS3).SelectAll();
		}

		private void txtLatS3_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtLatS3).SelectAll();
		}

		private void txtAlt3_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtAlt3).SelectAll();
		}

		private void txtAlt3_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtAlt3).SelectAll();
		}

		private void cmdGrazeWDS_Click(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			string text = cmbGrazeWDS.get_Items().get_Item(((ListControl)cmbGrazeWDS).get_SelectedIndex()).ToString()!.PadLeft(1);
			for (int i = 0; i < Star_ArchiveFileRecord.Count; i++)
			{
				if (lstCheckedEdit.GetItemChecked(i))
				{
					FileStream fileStream = new FileStream(Star_ArchiveFileRecord[i].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
					BinaryWriter binaryWriter = new BinaryWriter(fileStream);
					fileStream.Seek(Star_ArchiveFileRecord[i].Record * 270 + 25, SeekOrigin.Begin);
					binaryWriter.Write(text.ToCharArray());
					((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(i, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Remove(25, 1).Insert(25, text));
					fileStream.Close();
				}
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdGrazeDoublesCheck_Click(object sender, EventArgs e)
		{
			string text = cmbDoubleCode.get_Items().get_Item(((ListControl)cmbDoubleCode).get_SelectedIndex()).ToString()!.PadRight(1);
			ClearAllCheckBoxes();
			for (int i = 0; i < ((ObjectCollection)lstCheckedEdit.get_Items()).get_Count(); i++)
			{
				if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(i).ToString()!.Substring(46, 1) == text)
				{
					lstCheckedEdit.SetItemChecked(i, true);
				}
			}
		}

		private void cmdGrazeClearChecks_Click(object sender, EventArgs e)
		{
			ClearAllCheckBoxes();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void cmdPasteObserver_Click(object sender, EventArgs e)
		{
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			string text = Utilities.ProperCase(Clipboard.GetText());
			if (text.Contains("\r"))
			{
				if (text.IndexOf("\r") < text.Length - 2)
				{
					MessageBox.Show("Pasted text must be for a single line only");
					return;
				}
				text = text.Replace("\r", "").Replace("\n", "");
			}
			((Control)txtName).set_Text(text);
		}

		private void cmdPasteLocation_Click(object sender, EventArgs e)
		{
			string text = Utilities.ProperCase(Clipboard.GetText());
			if (!text.Contains("\r"))
			{
				((Control)txtPlace).set_Text(text);
			}
		}

		private void cmdCheckFromClipboard_Click(object sender, EventArgs e)
		{
			CheckFromCopied();
		}

		private void cmdCheckFromClipboard2_Click(object sender, EventArgs e)
		{
			CheckFromCopied();
			chkSomaGrazeClearChecks.set_Checked(false);
		}

		private void CheckFromCopied()
		{
			string[] array = Clipboard.GetText().Split('\r', '\n');
			int upperBound = array.GetUpperBound(0);
			ClearAllCheckBoxes();
			for (int i = 0; i < upperBound; i++)
			{
				if (array[i].Length <= 10 || !(array[i].Substring(0, 1) != " "))
				{
					continue;
				}
				string text = FormatSomaPaste(array[i]);
				for (int j = 0; j < ((ObjectCollection)lstCheckedEdit.get_Items()).get_Count(); j++)
				{
					if (((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(j).ToString()!.Substring(71, 1) == "S" && ((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(j).ToString()!.Substring(2, 16) == text)
					{
						lstCheckedEdit.SetItemChecked(j, true);
					}
				}
			}
			Label obj = lblnumberChecked;
			string text2;
			((Control)lblnumberChecked2).set_Text(text2 = lstCheckedEdit.get_CheckedItems().get_Count().ToString());
			((Control)obj).set_Text(text2);
		}

		private string FormatSomaPaste(string X)
		{
			string x = X.Substring(2, 15).Insert(12, ".");
			x = SomaNoZeros(x, 2);
			x = SomaNoZeros(x, 4);
			x = SomaNoZeros(x, 6);
			x = SomaNoZeros(x, 8);
			x = SomaNoZeros(x, 10);
			if (x.Substring(11, 1) == " ")
			{
				x = x.Remove(11, 1).Insert(11, "0");
			}
			return x;
		}

		private string SomaNoZeros(string X, int y)
		{
			if (X.Substring(y, 1) == "0")
			{
				X = X.Remove(y, 1).Insert(y, " ");
			}
			return X;
		}

		private void cmdSomaPasteSiteCoords_Click(object sender, EventArgs e)
		{
			//IL_025b: Unknown result type (might be due to invalid IL or missing references)
			string text = Clipboard.GetText();
			if (text.Length < 30)
			{
				return;
			}
			text = text.PadRight(60);
			string text2 = "";
			string text3 = "";
			int num = text.IndexOf("E", 20);
			if (num < 0 || num > 36)
			{
				text2 = "-";
				num = text.IndexOf("W", 30);
				if (num < 0)
				{
					return;
				}
			}
			((Control)txtLonD3).set_Text(text2 + text.Substring(num - 13, 3).Trim());
			((Control)txtLonM3).set_Text(text.Substring(num - 9, 2).Trim());
			((Control)txtLonS3).set_Text(text.Substring(num - 6, 5).Trim());
			num = text.IndexOf("N", 45);
			if (num < 0 || num > 48)
			{
				text2 = "-";
				num = text.IndexOf("S", 30);
				if (num < 0)
				{
					return;
				}
			}
			((Control)txtLatD3).set_Text(text3 + text.Substring(num - 12, 2).Trim());
			((Control)txtLatM3).set_Text(text.Substring(num - 9, 2).Trim());
			((Control)txtLatS3).set_Text(text.Substring(num - 6, 5).Trim());
			((Control)txtAlt3).set_Text(text.Substring(num + 1, 4).Trim());
			int num2 = " RNCO".IndexOf(text.Substring(2, 1));
			if (num2 < 0)
			{
				num2 = 0;
			}
			((ListControl)cmbTelescope).set_SelectedIndex(num2);
			num2 = " EA".IndexOf(text.Substring(3, 1));
			if (num2 < 0)
			{
				num2 = 0;
			}
			((ListControl)cmbMount).set_SelectedIndex(num2);
			num2 = " DM".IndexOf(text.Substring(4, 1));
			if (num2 < 0)
			{
				num2 = 0;
			}
			((ListControl)cmbDrive).set_SelectedIndex(num2);
			if (!double.TryParse(text.Substring(5, 5), out var result))
			{
				((Control)txtAperture).set_Text("");
			}
			else
			{
				((Control)txtAperture).set_Text(string.Format("{0,1:f0}", result));
			}
			if (!double.TryParse(text.Substring(11, 5), out result))
			{
				((Control)txtFocalLength).set_Text("");
			}
			else
			{
				((Control)txtFocalLength).set_Text(string.Format("{0,1:f0}", result));
			}
			MessageBox.Show("Set datum", "Set Datum");
		}

		private void cmdSetPEApplication_Click(object sender, EventArgs e)
		{
		}

		private void chkAdjAcc_CheckedChanged(object sender, EventArgs e)
		{
			NumericUpDown obj = updnAcc;
			bool @checked;
			((Control)cmbCertainty).set_Enabled(@checked = chkAdjAcc.get_Checked());
			((Control)obj).set_Enabled(@checked);
		}

		private void cmdSetGrazeEventDuration_Click(object sender, EventArgs e)
		{
			//IL_0040: Unknown result type (might be due to invalid IL or missing references)
			//IL_006c: Unknown result type (might be due to invalid IL or missing references)
			double result = 0.0;
			if (((Control)txtEventDuration).get_Text().Trim().Length > 0)
			{
				if (!double.TryParse(((Control)txtEventDuration).get_Text(), out result))
				{
					MessageBox.Show("Duration is invalid - non-numeric", "Error");
					return;
				}
				if (result > 9.999 || result < 0.0)
				{
					MessageBox.Show("Duration is invalid - too large, or less than zero", "Error");
					return;
				}
			}
			string text = ((Control)txtEventDuration).get_Text().Trim().PadRight(5)
				.Substring(0, 5);
			if (text.Substring(0, 1) == ".")
			{
				text = ("0" + ((Control)txtEventDuration).get_Text().Trim()).PadRight(5).Substring(0, 5);
			}
			int selectedIndex = ((ListControl)lstCheckedEdit).get_SelectedIndex();
			FileStream fileStream = new FileStream(Star_ArchiveFileRecord[selectedIndex].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			fileStream.Seek(Star_ArchiveFileRecord[selectedIndex].Record * 270 + 47, SeekOrigin.Begin);
			binaryWriter.Write(text.ToCharArray());
			((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(selectedIndex, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(selectedIndex).ToString()!.Remove(47, text.Length).Insert(47, text));
			fileStream.Close();
		}

		private void txtEventDuration_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtEventDuration).SelectAll();
		}

		private void txtEventDuration_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtEventDuration).SelectAll();
		}

		private void txtSecondsForDuration_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtSecondsForDuration).SelectAll();
		}

		private void txtSecondsForDuration_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtSecondsForDuration).SelectAll();
		}

		private void cmdSetMidTime_Click(object sender, EventArgs e)
		{
			//IL_0040: Unknown result type (might be due to invalid IL or missing references)
			//IL_006c: Unknown result type (might be due to invalid IL or missing references)
			double result = 0.0;
			if (((Control)txtEventDuration).get_Text().Trim().Length > 0)
			{
				if (!double.TryParse(((Control)txtSecondsForDuration).get_Text(), out result))
				{
					MessageBox.Show("Seconds is invalid - non-numeric", "Error");
					return;
				}
				if (result > 59.999 || result < 0.0)
				{
					MessageBox.Show("Seconds is invalid - too large, or less than zero", "Error");
					return;
				}
			}
			string text = ((Control)txtSecondsForDuration).get_Text().Trim().PadRight(6)
				.Substring(0, 6);
			if (text.Substring(0, 1) == ".")
			{
				text = (" 0" + ((Control)txtSecondsForDuration).get_Text().Trim()).PadRight(6).Substring(0, 6);
			}
			if (text.Substring(1, 1) == ".")
			{
				text = (" " + ((Control)txtSecondsForDuration).get_Text().Trim()).PadRight(6).Substring(0, 6);
			}
			int selectedIndex = ((ListControl)lstCheckedEdit).get_SelectedIndex();
			FileStream fileStream = new FileStream(Star_ArchiveFileRecord[selectedIndex].File.ToString(), FileMode.Open, FileAccess.ReadWrite);
			BinaryWriter binaryWriter = new BinaryWriter(fileStream);
			fileStream.Seek(Star_ArchiveFileRecord[selectedIndex].Record * 270 + 12, SeekOrigin.Begin);
			binaryWriter.Write(text.ToCharArray());
			((ObjectCollection)lstCheckedEdit.get_Items()).set_Item(selectedIndex, (object)((ObjectCollection)lstCheckedEdit.get_Items()).get_Item(selectedIndex).ToString()!.Remove(12, text.Length).Insert(12, text));
			fileStream.Close();
		}

		private void cmdGrazeSetDoubleCode_Click(object sender, EventArgs e)
		{
		}

		private void cmdListDuplicates_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0023: Invalid comparison between Unknown and I4
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select Archive file to Sort");
			((FileDialog)val).set_FileName("*.*");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				ListDuplicates(((FileDialog)val).get_FileName());
			}
		}

		private void ListDuplicates(string SourceFileName)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			LunarObservations.ShowDuplicates();
			LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Clear();
			using (StreamReader streamReader = new StreamReader(SourceFileName))
			{
				string text = streamReader.ReadLine();
				do
				{
					string text2 = streamReader.ReadLine();
					if (text2.Substring(0, 25) == text.Substring(0, 25) && text2.Substring(81, 8) == text.Substring(81, 8) && text2.Substring(92, 7) == text.Substring(92, 7))
					{
						LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Add((object)text);
						LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Add((object)text2);
						LunarObservations.ArchiveDuplicates.lstDuplicates.get_Items().Add((object)"");
					}
					text = text2;
				}
				while (!streamReader.EndOfStream);
			}
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdVizier_Click(object sender, EventArgs e)
		{
			LunarObservations.CreateVizierArchiveFile();
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
			//IL_048f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0499: Expected O, but got Unknown
			//IL_049a: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a4: Expected O, but got Unknown
			//IL_04a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04af: Expected O, but got Unknown
			//IL_04b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ba: Expected O, but got Unknown
			//IL_04bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c5: Expected O, but got Unknown
			//IL_04c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d0: Expected O, but got Unknown
			//IL_04d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04db: Expected O, but got Unknown
			//IL_04dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e6: Expected O, but got Unknown
			//IL_04e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f1: Expected O, but got Unknown
			//IL_04f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_04fc: Expected O, but got Unknown
			//IL_04fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0507: Expected O, but got Unknown
			//IL_0508: Unknown result type (might be due to invalid IL or missing references)
			//IL_0512: Expected O, but got Unknown
			//IL_0513: Unknown result type (might be due to invalid IL or missing references)
			//IL_051d: Expected O, but got Unknown
			//IL_051e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0528: Expected O, but got Unknown
			//IL_0529: Unknown result type (might be due to invalid IL or missing references)
			//IL_0533: Expected O, but got Unknown
			//IL_0534: Unknown result type (might be due to invalid IL or missing references)
			//IL_053e: Expected O, but got Unknown
			//IL_053f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0549: Expected O, but got Unknown
			//IL_054a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0554: Expected O, but got Unknown
			//IL_0555: Unknown result type (might be due to invalid IL or missing references)
			//IL_055f: Expected O, but got Unknown
			//IL_0560: Unknown result type (might be due to invalid IL or missing references)
			//IL_056a: Expected O, but got Unknown
			//IL_056b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0575: Expected O, but got Unknown
			//IL_0576: Unknown result type (might be due to invalid IL or missing references)
			//IL_0580: Expected O, but got Unknown
			//IL_0581: Unknown result type (might be due to invalid IL or missing references)
			//IL_058b: Expected O, but got Unknown
			//IL_058c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0596: Expected O, but got Unknown
			//IL_0597: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a1: Expected O, but got Unknown
			//IL_05a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ac: Expected O, but got Unknown
			//IL_05ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b7: Expected O, but got Unknown
			//IL_05b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c2: Expected O, but got Unknown
			//IL_05c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05cd: Expected O, but got Unknown
			//IL_05ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d8: Expected O, but got Unknown
			//IL_05d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e3: Expected O, but got Unknown
			//IL_05e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ee: Expected O, but got Unknown
			//IL_05ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f9: Expected O, but got Unknown
			//IL_05fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0604: Expected O, but got Unknown
			//IL_0605: Unknown result type (might be due to invalid IL or missing references)
			//IL_060f: Expected O, but got Unknown
			//IL_0610: Unknown result type (might be due to invalid IL or missing references)
			//IL_061a: Expected O, but got Unknown
			//IL_061b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0625: Expected O, but got Unknown
			//IL_0626: Unknown result type (might be due to invalid IL or missing references)
			//IL_0630: Expected O, but got Unknown
			//IL_0631: Unknown result type (might be due to invalid IL or missing references)
			//IL_063b: Expected O, but got Unknown
			//IL_063c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0646: Expected O, but got Unknown
			//IL_0647: Unknown result type (might be due to invalid IL or missing references)
			//IL_0651: Expected O, but got Unknown
			//IL_0652: Unknown result type (might be due to invalid IL or missing references)
			//IL_065c: Expected O, but got Unknown
			//IL_065d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0667: Expected O, but got Unknown
			//IL_0668: Unknown result type (might be due to invalid IL or missing references)
			//IL_0672: Expected O, but got Unknown
			//IL_0673: Unknown result type (might be due to invalid IL or missing references)
			//IL_067d: Expected O, but got Unknown
			//IL_067e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0688: Expected O, but got Unknown
			//IL_0689: Unknown result type (might be due to invalid IL or missing references)
			//IL_0693: Expected O, but got Unknown
			//IL_0694: Unknown result type (might be due to invalid IL or missing references)
			//IL_069e: Expected O, but got Unknown
			//IL_069f: Unknown result type (might be due to invalid IL or missing references)
			//IL_06a9: Expected O, but got Unknown
			//IL_06aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_06b4: Expected O, but got Unknown
			//IL_06b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_06bf: Expected O, but got Unknown
			//IL_06c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ca: Expected O, but got Unknown
			//IL_06cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_06d5: Expected O, but got Unknown
			//IL_06d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_06e0: Expected O, but got Unknown
			//IL_06e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_06eb: Expected O, but got Unknown
			//IL_06ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_06f6: Expected O, but got Unknown
			//IL_06f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0701: Expected O, but got Unknown
			//IL_0702: Unknown result type (might be due to invalid IL or missing references)
			//IL_070c: Expected O, but got Unknown
			//IL_070d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0717: Expected O, but got Unknown
			//IL_0718: Unknown result type (might be due to invalid IL or missing references)
			//IL_0722: Expected O, but got Unknown
			//IL_0723: Unknown result type (might be due to invalid IL or missing references)
			//IL_072d: Expected O, but got Unknown
			//IL_072e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0738: Expected O, but got Unknown
			//IL_0739: Unknown result type (might be due to invalid IL or missing references)
			//IL_0743: Expected O, but got Unknown
			//IL_0744: Unknown result type (might be due to invalid IL or missing references)
			//IL_074e: Expected O, but got Unknown
			//IL_074f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0759: Expected O, but got Unknown
			//IL_075a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0764: Expected O, but got Unknown
			//IL_0765: Unknown result type (might be due to invalid IL or missing references)
			//IL_076f: Expected O, but got Unknown
			//IL_0770: Unknown result type (might be due to invalid IL or missing references)
			//IL_077a: Expected O, but got Unknown
			//IL_077b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0785: Expected O, but got Unknown
			//IL_0786: Unknown result type (might be due to invalid IL or missing references)
			//IL_0790: Expected O, but got Unknown
			//IL_0791: Unknown result type (might be due to invalid IL or missing references)
			//IL_079b: Expected O, but got Unknown
			//IL_079c: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a6: Expected O, but got Unknown
			//IL_07a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b1: Expected O, but got Unknown
			//IL_07b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_07bc: Expected O, but got Unknown
			//IL_07bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c7: Expected O, but got Unknown
			//IL_07c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_07d2: Expected O, but got Unknown
			//IL_07d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_07dd: Expected O, but got Unknown
			//IL_07de: Unknown result type (might be due to invalid IL or missing references)
			//IL_07e8: Expected O, but got Unknown
			//IL_07e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_07f3: Expected O, but got Unknown
			//IL_07f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_07fe: Expected O, but got Unknown
			//IL_07ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0809: Expected O, but got Unknown
			//IL_080a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0814: Expected O, but got Unknown
			//IL_0815: Unknown result type (might be due to invalid IL or missing references)
			//IL_081f: Expected O, but got Unknown
			//IL_0820: Unknown result type (might be due to invalid IL or missing references)
			//IL_082a: Expected O, but got Unknown
			//IL_082b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0835: Expected O, but got Unknown
			//IL_0836: Unknown result type (might be due to invalid IL or missing references)
			//IL_0840: Expected O, but got Unknown
			//IL_0841: Unknown result type (might be due to invalid IL or missing references)
			//IL_084b: Expected O, but got Unknown
			//IL_084c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0856: Expected O, but got Unknown
			//IL_0857: Unknown result type (might be due to invalid IL or missing references)
			//IL_0861: Expected O, but got Unknown
			//IL_0862: Unknown result type (might be due to invalid IL or missing references)
			//IL_086c: Expected O, but got Unknown
			//IL_086d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0877: Expected O, but got Unknown
			//IL_0878: Unknown result type (might be due to invalid IL or missing references)
			//IL_0882: Expected O, but got Unknown
			//IL_0883: Unknown result type (might be due to invalid IL or missing references)
			//IL_088d: Expected O, but got Unknown
			//IL_088e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0898: Expected O, but got Unknown
			//IL_0899: Unknown result type (might be due to invalid IL or missing references)
			//IL_08a3: Expected O, but got Unknown
			//IL_08a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_08ae: Expected O, but got Unknown
			//IL_08af: Unknown result type (might be due to invalid IL or missing references)
			//IL_08b9: Expected O, but got Unknown
			//IL_08ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_08c4: Expected O, but got Unknown
			//IL_08c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_08cf: Expected O, but got Unknown
			//IL_08d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_08da: Expected O, but got Unknown
			//IL_08db: Unknown result type (might be due to invalid IL or missing references)
			//IL_08e5: Expected O, but got Unknown
			//IL_08e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_08f0: Expected O, but got Unknown
			//IL_08f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_08fb: Expected O, but got Unknown
			//IL_08fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0906: Expected O, but got Unknown
			//IL_0907: Unknown result type (might be due to invalid IL or missing references)
			//IL_0911: Expected O, but got Unknown
			//IL_0912: Unknown result type (might be due to invalid IL or missing references)
			//IL_091c: Expected O, but got Unknown
			//IL_091d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0927: Expected O, but got Unknown
			//IL_0928: Unknown result type (might be due to invalid IL or missing references)
			//IL_0932: Expected O, but got Unknown
			//IL_0933: Unknown result type (might be due to invalid IL or missing references)
			//IL_093d: Expected O, but got Unknown
			//IL_093e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0948: Expected O, but got Unknown
			//IL_0949: Unknown result type (might be due to invalid IL or missing references)
			//IL_0953: Expected O, but got Unknown
			//IL_0954: Unknown result type (might be due to invalid IL or missing references)
			//IL_095e: Expected O, but got Unknown
			//IL_095f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0969: Expected O, but got Unknown
			//IL_096a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0974: Expected O, but got Unknown
			//IL_0975: Unknown result type (might be due to invalid IL or missing references)
			//IL_097f: Expected O, but got Unknown
			//IL_0980: Unknown result type (might be due to invalid IL or missing references)
			//IL_098a: Expected O, but got Unknown
			//IL_098b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0995: Expected O, but got Unknown
			//IL_0996: Unknown result type (might be due to invalid IL or missing references)
			//IL_09a0: Expected O, but got Unknown
			//IL_09a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_09ab: Expected O, but got Unknown
			//IL_09ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_09b6: Expected O, but got Unknown
			//IL_09b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_09c1: Expected O, but got Unknown
			//IL_09c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_09cc: Expected O, but got Unknown
			//IL_09cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_09d7: Expected O, but got Unknown
			//IL_09d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_09e2: Expected O, but got Unknown
			//IL_09e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_09ed: Expected O, but got Unknown
			//IL_09ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_09f8: Expected O, but got Unknown
			//IL_09f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a03: Expected O, but got Unknown
			//IL_0a04: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a0e: Expected O, but got Unknown
			//IL_0a0f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a19: Expected O, but got Unknown
			//IL_0a1a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a24: Expected O, but got Unknown
			//IL_0a25: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a2f: Expected O, but got Unknown
			//IL_0a30: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a3a: Expected O, but got Unknown
			//IL_0a3b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a45: Expected O, but got Unknown
			//IL_0a46: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a50: Expected O, but got Unknown
			//IL_0a51: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a5b: Expected O, but got Unknown
			//IL_0a5c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a66: Expected O, but got Unknown
			//IL_0a67: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a71: Expected O, but got Unknown
			//IL_0a72: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a7c: Expected O, but got Unknown
			//IL_0a7d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a87: Expected O, but got Unknown
			//IL_0a88: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a92: Expected O, but got Unknown
			//IL_0a93: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a9d: Expected O, but got Unknown
			//IL_0a9e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aa8: Expected O, but got Unknown
			//IL_0aa9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ab3: Expected O, but got Unknown
			//IL_0ab4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0abe: Expected O, but got Unknown
			//IL_0abf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ac9: Expected O, but got Unknown
			//IL_0aca: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ad4: Expected O, but got Unknown
			//IL_0ad5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0adf: Expected O, but got Unknown
			//IL_0ae0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aea: Expected O, but got Unknown
			//IL_0aeb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0af5: Expected O, but got Unknown
			//IL_0af6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b00: Expected O, but got Unknown
			//IL_0b01: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b0b: Expected O, but got Unknown
			//IL_0b0c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b16: Expected O, but got Unknown
			//IL_0b17: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b21: Expected O, but got Unknown
			//IL_0b22: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b2c: Expected O, but got Unknown
			//IL_0b2d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b37: Expected O, but got Unknown
			//IL_0b38: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b42: Expected O, but got Unknown
			//IL_0b43: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b4d: Expected O, but got Unknown
			//IL_0b4e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b58: Expected O, but got Unknown
			//IL_0b59: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b63: Expected O, but got Unknown
			//IL_0b64: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b6e: Expected O, but got Unknown
			//IL_0b6f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b79: Expected O, but got Unknown
			//IL_0b7a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b84: Expected O, but got Unknown
			//IL_0b85: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b8f: Expected O, but got Unknown
			//IL_0b90: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b9a: Expected O, but got Unknown
			//IL_0b9b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ba5: Expected O, but got Unknown
			//IL_0ba6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bb0: Expected O, but got Unknown
			//IL_0bb1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bbb: Expected O, but got Unknown
			//IL_0bbc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bc6: Expected O, but got Unknown
			//IL_0bc7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bd1: Expected O, but got Unknown
			//IL_0bd2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bdc: Expected O, but got Unknown
			//IL_0bdd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0be7: Expected O, but got Unknown
			//IL_0be8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bf2: Expected O, but got Unknown
			//IL_0bf3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bfd: Expected O, but got Unknown
			//IL_0bfe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c08: Expected O, but got Unknown
			//IL_0c09: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c13: Expected O, but got Unknown
			//IL_0c14: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c1e: Expected O, but got Unknown
			//IL_0c1f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c29: Expected O, but got Unknown
			//IL_0c2a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c34: Expected O, but got Unknown
			//IL_0c35: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c3f: Expected O, but got Unknown
			//IL_0c40: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c4a: Expected O, but got Unknown
			//IL_0c4b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c55: Expected O, but got Unknown
			//IL_0c56: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c60: Expected O, but got Unknown
			//IL_0c61: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c6b: Expected O, but got Unknown
			//IL_0c6c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c76: Expected O, but got Unknown
			//IL_0c77: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c81: Expected O, but got Unknown
			//IL_0c82: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c8c: Expected O, but got Unknown
			//IL_0c8d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c97: Expected O, but got Unknown
			//IL_0c98: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ca2: Expected O, but got Unknown
			//IL_0ca3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cad: Expected O, but got Unknown
			//IL_0cae: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cb8: Expected O, but got Unknown
			//IL_0cb9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cc3: Expected O, but got Unknown
			//IL_0cc4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cce: Expected O, but got Unknown
			//IL_0ccf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cd9: Expected O, but got Unknown
			//IL_0cda: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ce4: Expected O, but got Unknown
			//IL_0ce5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cef: Expected O, but got Unknown
			//IL_0cf0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cfa: Expected O, but got Unknown
			//IL_0cfb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d05: Expected O, but got Unknown
			//IL_0d06: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d10: Expected O, but got Unknown
			//IL_0d11: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d1b: Expected O, but got Unknown
			//IL_0d1c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d26: Expected O, but got Unknown
			//IL_0d27: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d31: Expected O, but got Unknown
			//IL_0d32: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d3c: Expected O, but got Unknown
			//IL_0d3d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d47: Expected O, but got Unknown
			//IL_0d48: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d52: Expected O, but got Unknown
			//IL_0d53: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d5d: Expected O, but got Unknown
			//IL_0d5e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d68: Expected O, but got Unknown
			//IL_0d69: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d73: Expected O, but got Unknown
			//IL_0d74: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d7e: Expected O, but got Unknown
			//IL_0d7f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d89: Expected O, but got Unknown
			//IL_0d8a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d94: Expected O, but got Unknown
			//IL_0d95: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d9f: Expected O, but got Unknown
			//IL_0da0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0daa: Expected O, but got Unknown
			//IL_0dab: Unknown result type (might be due to invalid IL or missing references)
			//IL_0db5: Expected O, but got Unknown
			//IL_0db6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dc0: Expected O, but got Unknown
			//IL_0dc1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dcb: Expected O, but got Unknown
			//IL_0dcc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dd6: Expected O, but got Unknown
			//IL_0dd7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0de1: Expected O, but got Unknown
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
			//IL_0e19: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e23: Expected O, but got Unknown
			//IL_0e24: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e2e: Expected O, but got Unknown
			//IL_0e2f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e39: Expected O, but got Unknown
			//IL_0e3a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e44: Expected O, but got Unknown
			//IL_0e45: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e4f: Expected O, but got Unknown
			//IL_0e50: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e5a: Expected O, but got Unknown
			//IL_0e5b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e65: Expected O, but got Unknown
			//IL_0e66: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e70: Expected O, but got Unknown
			//IL_0e71: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e7b: Expected O, but got Unknown
			//IL_0e7c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e86: Expected O, but got Unknown
			//IL_0e87: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e91: Expected O, but got Unknown
			//IL_0e92: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e9c: Expected O, but got Unknown
			//IL_0e9d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ea7: Expected O, but got Unknown
			//IL_0ea8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0eb2: Expected O, but got Unknown
			//IL_0eb3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ebd: Expected O, but got Unknown
			//IL_0ebe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ec8: Expected O, but got Unknown
			//IL_0ec9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ed3: Expected O, but got Unknown
			//IL_0ed4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ede: Expected O, but got Unknown
			//IL_0edf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ee9: Expected O, but got Unknown
			//IL_0eea: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ef4: Expected O, but got Unknown
			//IL_0ef5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0eff: Expected O, but got Unknown
			//IL_0f00: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f0a: Expected O, but got Unknown
			//IL_0f0b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f15: Expected O, but got Unknown
			//IL_0f16: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f20: Expected O, but got Unknown
			//IL_0f21: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f2b: Expected O, but got Unknown
			//IL_0f2c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f36: Expected O, but got Unknown
			//IL_0f37: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f41: Expected O, but got Unknown
			//IL_0f42: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f4c: Expected O, but got Unknown
			//IL_0f4d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f57: Expected O, but got Unknown
			//IL_0f58: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f62: Expected O, but got Unknown
			//IL_0f63: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f6d: Expected O, but got Unknown
			//IL_0f6e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f78: Expected O, but got Unknown
			//IL_0f79: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f83: Expected O, but got Unknown
			//IL_0f84: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f8e: Expected O, but got Unknown
			//IL_0f8f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f99: Expected O, but got Unknown
			//IL_0f9a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fa4: Expected O, but got Unknown
			//IL_0fa5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0faf: Expected O, but got Unknown
			//IL_0fb0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fba: Expected O, but got Unknown
			//IL_0fbb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fc5: Expected O, but got Unknown
			//IL_0fc6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fd0: Expected O, but got Unknown
			//IL_0fd1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fdb: Expected O, but got Unknown
			//IL_0fdc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fe6: Expected O, but got Unknown
			//IL_0fe7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ff1: Expected O, but got Unknown
			//IL_0ff2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ffc: Expected O, but got Unknown
			//IL_0ffd: Unknown result type (might be due to invalid IL or missing references)
			//IL_1007: Expected O, but got Unknown
			//IL_1008: Unknown result type (might be due to invalid IL or missing references)
			//IL_1012: Expected O, but got Unknown
			//IL_1013: Unknown result type (might be due to invalid IL or missing references)
			//IL_101d: Expected O, but got Unknown
			//IL_101e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1028: Expected O, but got Unknown
			//IL_1029: Unknown result type (might be due to invalid IL or missing references)
			//IL_1033: Expected O, but got Unknown
			//IL_1034: Unknown result type (might be due to invalid IL or missing references)
			//IL_103e: Expected O, but got Unknown
			//IL_806d: Unknown result type (might be due to invalid IL or missing references)
			//IL_8077: Expected O, but got Unknown
			//IL_8124: Unknown result type (might be due to invalid IL or missing references)
			//IL_812e: Expected O, but got Unknown
			//IL_9e6a: Unknown result type (might be due to invalid IL or missing references)
			//IL_9e74: Expected O, but got Unknown
			//IL_9f5e: Unknown result type (might be due to invalid IL or missing references)
			//IL_9f68: Expected O, but got Unknown
			//IL_a052: Unknown result type (might be due to invalid IL or missing references)
			//IL_a05c: Expected O, but got Unknown
			//IL_a14b: Unknown result type (might be due to invalid IL or missing references)
			//IL_a155: Expected O, but got Unknown
			//IL_a1c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_a1cb: Expected O, but got Unknown
			//IL_a237: Unknown result type (might be due to invalid IL or missing references)
			//IL_a241: Expected O, but got Unknown
			//IL_a5e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_a5ec: Expected O, but got Unknown
			//IL_b1cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_b1d5: Expected O, but got Unknown
			//IL_b488: Unknown result type (might be due to invalid IL or missing references)
			//IL_b492: Expected O, but got Unknown
			//IL_ba59: Unknown result type (might be due to invalid IL or missing references)
			//IL_ba63: Expected O, but got Unknown
			//IL_bc9f: Unknown result type (might be due to invalid IL or missing references)
			//IL_bca9: Expected O, but got Unknown
			//IL_be43: Unknown result type (might be due to invalid IL or missing references)
			//IL_be4d: Expected O, but got Unknown
			//IL_c3ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_c3c4: Expected O, but got Unknown
			//IL_c5c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_c5cd: Expected O, but got Unknown
			//IL_f1e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_f1f1: Expected O, but got Unknown
			//IL_f759: Unknown result type (might be due to invalid IL or missing references)
			//IL_f763: Expected O, but got Unknown
			//IL_f870: Unknown result type (might be due to invalid IL or missing references)
			//IL_f87a: Expected O, but got Unknown
			//IL_f92a: Unknown result type (might be due to invalid IL or missing references)
			//IL_f934: Expected O, but got Unknown
			//IL_fc5a: Unknown result type (might be due to invalid IL or missing references)
			//IL_fc64: Expected O, but got Unknown
			cmdAutoCorrectArchive = new Button();
			cmdReduceArchiveFile = new Button();
			lstEdit = new ListBox();
			cmdListLargeResiduals = new Button();
			pnlPre1750 = new GroupBox();
			panel1 = new Panel();
			label65 = new Label();
			cmbCertainty = new ComboBox();
			chkAdjAcc = new CheckBox();
			label64 = new Label();
			updnAcc = new NumericUpDown();
			cmdGeneratePrediction = new Button();
			chkSecs = new CheckBox();
			cmdAdjustedReduce = new Button();
			label14 = new Label();
			updnSecond = new NumericUpDown();
			label13 = new Label();
			updnPE = new NumericUpDown();
			label11 = new Label();
			label10 = new Label();
			label9 = new Label();
			label8 = new Label();
			label7 = new Label();
			updnDay = new NumericUpDown();
			updnHour = new NumericUpDown();
			updnMinute = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnYear = new NumericUpDown();
			optXZ = new RadioButton();
			optSAO = new RadioButton();
			optZC = new RadioButton();
			txtStar = new TextBox();
			label12 = new Label();
			label1 = new Label();
			lblResidual = new Label();
			cmdIdentify = new Button();
			cmdGoogleEarth = new Button();
			updnLongSec = new NumericUpDown();
			updnLongD = new NumericUpDown();
			updnLongM = new NumericUpDown();
			updnLatS = new NumericUpDown();
			updnLatD = new NumericUpDown();
			updnLatM = new NumericUpDown();
			groupBox1 = new GroupBox();
			label6 = new Label();
			label4 = new Label();
			label2 = new Label();
			optW = new RadioButton();
			optE = new RadioButton();
			groupBox2 = new GroupBox();
			label15 = new Label();
			label5 = new Label();
			label3 = new Label();
			optS = new RadioButton();
			optN = new RadioButton();
			optA = new RadioButton();
			optF = new RadioButton();
			optD = new RadioButton();
			optC = new RadioButton();
			optB = new RadioButton();
			optEc = new RadioButton();
			groupBox3 = new GroupBox();
			chkReReducedOldObs = new CheckBox();
			optNoCorrection = new RadioButton();
			optU = new RadioButton();
			optG = new RadioButton();
			optSs = new RadioButton();
			label17 = new Label();
			label16 = new Label();
			optT = new RadioButton();
			cmdCorrect = new Button();
			optWe = new RadioButton();
			optZ = new RadioButton();
			optX = new RadioButton();
			optY = new RadioButton();
			optPlanet = new RadioButton();
			groupBox4 = new GroupBox();
			label72 = new Label();
			cmbWDS_General = new ComboBox();
			optAsteroid = new RadioButton();
			optUnknown = new RadioButton();
			chkDR = new CheckBox();
			groupBox5 = new GroupBox();
			cmdSortArchive = new Button();
			cmdMergeArchiveFiles = new Button();
			optAll = new RadioButton();
			optUnresolved = new RadioButton();
			optCorrected = new RadioButton();
			optUnconsidered = new RadioButton();
			cmdRetrieveOriginal = new Button();
			menuStrip1 = new MenuStrip();
			saveListToolStripMenuItem = new ToolStripMenuItem();
			appendListToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			withCheckedListToolStripMenuItem = new ToolStripMenuItem();
			checkAllToolStripMenuItem = new ToolStripMenuItem();
			clearAllToolStripMenuItem = new ToolStripMenuItem();
			invertSelectionToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			checkAllSourcedFromOToolStripMenuItem = new ToolStripMenuItem();
			checkAllSourcedFrToolStripMenuItem = new ToolStripMenuItem();
			checkAllSourcedFromSToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdPutEventsInEditor = new Button();
			txtEventYear = new TextBox();
			panelGeneralEdit = new Panel();
			label33 = new Label();
			txtGrazeNumber = new TextBox();
			chkGraze = new CheckBox();
			panelEventTypes = new Panel();
			optSomaWGS84 = new RadioButton();
			optHT = new RadioButton();
			optPlanets = new RadioButton();
			txtEventMonth = new TextBox();
			label35 = new Label();
			txtILOCSearchSite = new TextBox();
			chkSites = new CheckBox();
			optOther = new RadioButton();
			optGeneral = new RadioButton();
			optSiteEditor = new RadioButton();
			panelSiteEdit = new Panel();
			groupBox6 = new GroupBox();
			txtListSiteName = new TextBox();
			txtListObserver = new TextBox();
			optListSite = new RadioButton();
			optListObserver = new RadioButton();
			cmdGetSiteEvents = new Button();
			panelILOC = new Panel();
			txtILOC3 = new TextBox();
			txtILOC2 = new TextBox();
			txtILOC1 = new TextBox();
			panelRGO = new Panel();
			txtRGO2 = new TextBox();
			txtRGO1 = new TextBox();
			optILOC = new RadioButton();
			optRGO = new RadioButton();
			PanelILOCsiteReplace = new Panel();
			txtTelescope = new TextBox();
			cmdRetrieveNewILOCsite = new Button();
			chkChangeTelescope = new CheckBox();
			txtILOC3replace = new TextBox();
			chkCorrectILOCsites = new CheckBox();
			txtILOC2replace = new TextBox();
			txtILOC1replace = new TextBox();
			cmdRetrieveRGOsite = new Button();
			lblCurrentCoords = new Label();
			label27 = new Label();
			label26 = new Label();
			label25 = new Label();
			lblNewCoords = new Label();
			lblOldCoords = new Label();
			cmdGoogleEarthSites = new Button();
			cmdUpdateSites = new Button();
			chkCorrectSite = new CheckBox();
			chkCorrectNames = new CheckBox();
			chkCoords = new CheckBox();
			label24 = new Label();
			label23 = new Label();
			label22 = new Label();
			label21 = new Label();
			label20 = new Label();
			label19 = new Label();
			label18 = new Label();
			lblSiteLeft = new Label();
			lblObsLeft = new Label();
			txtSite2 = new TextBox();
			txtSite1 = new TextBox();
			txtObs1 = new TextBox();
			txtObs2 = new TextBox();
			cmbAltDatum2 = new ComboBox();
			cmbAltDatum1 = new ComboBox();
			txtAlt1 = new TextBox();
			txtAlt2 = new TextBox();
			cmbDatum1 = new ComboBox();
			cmbDatum2 = new ComboBox();
			txtLatS1 = new TextBox();
			txtLatM1 = new TextBox();
			txtLatD1 = new TextBox();
			txtLatS2 = new TextBox();
			txtLatM2 = new TextBox();
			txtLatD2 = new TextBox();
			txtLonS1 = new TextBox();
			txtLonM1 = new TextBox();
			txtLonD1 = new TextBox();
			txtLonS2 = new TextBox();
			txtLonM2 = new TextBox();
			txtLonD2 = new TextBox();
			panelHeader = new Panel();
			cmdListDuplicates = new Button();
			chkExcludeInvalid = new CheckBox();
			chkExcludeStartEnd = new CheckBox();
			chkbySiteID = new CheckBox();
			cmdProcessfromCoordinators = new Button();
			optDoublesEditor = new RadioButton();
			panelDoubles = new Panel();
			cmbDoubleCodes = new ComboBox();
			cmdCheckDoubles = new Button();
			labelDouble = new Label();
			label32 = new Label();
			lstDoubles = new ListBox();
			groupBox7 = new GroupBox();
			chkDoublesOnly = new CheckBox();
			txtXZ = new TextBox();
			cmbNames = new ComboBox();
			label28 = new Label();
			label29 = new Label();
			cmbStarList = new ComboBox();
			label30 = new Label();
			label31 = new Label();
			cmdReadForDoubles = new Button();
			txtZC = new TextBox();
			txtSAO = new TextBox();
			grpSetWDS = new GroupBox();
			cmdApplyWDS = new Button();
			cmbWDS = new ComboBox();
			cmdStarsCheckAll = new Button();
			cmdStarsClearAll = new Button();
			lblResidual2 = new Label();
			label34 = new Label();
			lstCheckedEdit = new CheckedListBox();
			optGrazeEditor = new RadioButton();
			optCombineRegions = new RadioButton();
			panelGraze = new Panel();
			grpGrazeSites = new GroupBox();
			chkSomaGrazeClearChecks = new CheckBox();
			cmdSomaPasteSiteCoords = new Button();
			cmdCheckFromClipboard2 = new Button();
			cmdGrazeChangeSites = new Button();
			lblnumberChecked2 = new Label();
			label57 = new Label();
			label52 = new Label();
			label58 = new Label();
			cmdGrazeSitesCheckeSameAsFirst_2 = new Button();
			label59 = new Label();
			txtLatD3 = new TextBox();
			label60 = new Label();
			txtLonD3 = new TextBox();
			label61 = new Label();
			txtLonM3 = new TextBox();
			cmbAltDatum3 = new ComboBox();
			txtLonS3 = new TextBox();
			txtAlt3 = new TextBox();
			txtLatM3 = new TextBox();
			cmbDatum3 = new ComboBox();
			txtLatS3 = new TextBox();
			grpGrazeDoubles = new GroupBox();
			cmdGrazeClearChecks = new Button();
			cmbDoubleCode = new ComboBox();
			cmdGrazeDoublesCheck = new Button();
			groupBox8 = new GroupBox();
			cmdGrazeWDS = new Button();
			cmbGrazeWDS = new ComboBox();
			grpGrazeScopes = new GroupBox();
			cmdCheckFromClipboard = new Button();
			cmdPasteLocation = new Button();
			cmdPasteObserver = new Button();
			cmdGrazeUpdateSites = new Button();
			label75 = new Label();
			lblCharsLeft = new Label();
			label68 = new Label();
			txtPlace = new TextBox();
			label70 = new Label();
			lblCharsLeftName = new Label();
			lblObserverName = new Label();
			label50 = new Label();
			txtName = new TextBox();
			grpTelescopesAndNames = new GroupBox();
			label43 = new Label();
			cmbMount = new ComboBox();
			label44 = new Label();
			cmbDrive = new ComboBox();
			label45 = new Label();
			label46 = new Label();
			label47 = new Label();
			txtFocalLength = new TextBox();
			label48 = new Label();
			label49 = new Label();
			cmbTelescope = new ComboBox();
			txtAperture = new TextBox();
			lblnumberChecked = new Label();
			label42 = new Label();
			cmdGrazeSitesCheckeSameAsFirst = new Button();
			chkAutoIncrement = new CheckBox();
			grpEventDuration = new GroupBox();
			cmdGrazeSetDoubleCode = new Button();
			label62 = new Label();
			cmbGrazeDoubles = new ComboBox();
			label55 = new Label();
			label54 = new Label();
			cmdSetMidTime = new Button();
			txtSecondsForDuration = new TextBox();
			label53 = new Label();
			label51 = new Label();
			lblCurrentEventTime = new Label();
			cmdSetGrazeEventDuration = new Button();
			txtEventDuration = new TextBox();
			grpTelescopeCodes = new GroupBox();
			cmdSetPEApplication = new Button();
			cmdGrazeSitesAutoCorrect = new Button();
			cmdGrazesGetTelescopes = new Button();
			grpGrazeDR = new GroupBox();
			cmbGrazeEvents = new ComboBox();
			cmdGrazesSetEvent = new Button();
			chkUseGrazeIDonly = new CheckBox();
			grp2ndLimit = new GroupBox();
			cmdClear2ndLimit = new Button();
			cmdSet2ndLimit = new Button();
			grpDatums = new GroupBox();
			cmdSetSomaVerticaltoMSL = new Button();
			label41 = new Label();
			cmbHorizontalDatum2 = new ComboBox();
			cmdSetSomaVerticalToEllipsoid = new Button();
			cmdFindSomaEllipsoid = new Button();
			cmdSetSelectedToDatum = new Button();
			cmdFindZeroDatums = new Button();
			cmdTagAllGrazeEvents = new Button();
			lblGrazeFile = new Label();
			grpGrazeRenumber = new GroupBox();
			cmdRenumberGrazes = new Button();
			cmdClearDuplicatteResidualFlags = new Button();
			cmdRecordGrazeAsEdited = new Button();
			lblSource = new Label();
			grpDuplicateGrazeEvents = new GroupBox();
			cmdGrazeSetDuplicateToKeep = new Button();
			cmdGrazeSetDuplicateToDelete = new Button();
			cmdGrazeEventsInEditor_Valid = new Button();
			cmdGrazeEventsInEditor_No_StartEnd = new Button();
			groupBox11 = new GroupBox();
			optGrazeEventDuration = new RadioButton();
			optGrazeSetDoubles = new RadioButton();
			optGrazeSites = new RadioButton();
			optDatums = new RadioButton();
			optTelescopeCodes = new RadioButton();
			optGrazeEventCode = new RadioButton();
			optGrazeSiteDetails = new RadioButton();
			optMarkAs2ndLimit = new RadioButton();
			optTagGrazes = new RadioButton();
			optGraze_NullSecs = new RadioButton();
			optGraze_RenumberGrazes = new RadioButton();
			optGraze_Duplicates = new RadioButton();
			optGraze_LargeResiduals = new RadioButton();
			optGraze_Grazeflag = new RadioButton();
			cmbGrazeList = new ComboBox();
			cmdSelectArchiveForGrazeEdit = new Button();
			grpLargeResiduals = new GroupBox();
			cmdSetFlagLargeResiduals = new Button();
			label39 = new Label();
			label38 = new Label();
			updnSmoothTop = new NumericUpDown();
			updnSmoothBottom = new NumericUpDown();
			label37 = new Label();
			updnLimbCorrectedResidual = new NumericUpDown();
			optSmoothResidual = new RadioButton();
			optLimbResidual = new RadioButton();
			cmdMarkLargeResiduals = new Button();
			cmdGrazeEventsToEditor = new Button();
			grpNullSeconds = new GroupBox();
			cmdSetZeroSecs = new Button();
			cmdFindBlankSecs = new Button();
			grpGrazeFlag = new GroupBox();
			lblGrazeID = new Label();
			cmdClearGrazeFlags = new Button();
			cmdSetGrazeFlags = new Button();
			chkAutoDisplay = new CheckBox();
			cmbObservationsSource = new ComboBox();
			panelCombine = new Panel();
			groupBox9 = new GroupBox();
			chkVizierLimitTo1960 = new CheckBox();
			cmdVizier = new Button();
			label63 = new Label();
			label56 = new Label();
			txtEmailSignatureName = new TextBox();
			label40 = new Label();
			checkBox1 = new CheckBox();
			checkBox2 = new CheckBox();
			cmdGenerateGrazeIndex = new Button();
			label36 = new Label();
			panelResiduals = new Panel();
			((Control)pnlPre1750).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)updnAcc).BeginInit();
			((ISupportInitialize)updnSecond).BeginInit();
			((ISupportInitialize)updnPE).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnHour).BeginInit();
			((ISupportInitialize)updnMinute).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnYear).BeginInit();
			((ISupportInitialize)updnLongSec).BeginInit();
			((ISupportInitialize)updnLongD).BeginInit();
			((ISupportInitialize)updnLongM).BeginInit();
			((ISupportInitialize)updnLatS).BeginInit();
			((ISupportInitialize)updnLatD).BeginInit();
			((ISupportInitialize)updnLatM).BeginInit();
			((Control)groupBox1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)groupBox4).SuspendLayout();
			((Control)groupBox5).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)panelGeneralEdit).SuspendLayout();
			((Control)panelEventTypes).SuspendLayout();
			((Control)panelSiteEdit).SuspendLayout();
			((Control)groupBox6).SuspendLayout();
			((Control)panelILOC).SuspendLayout();
			((Control)panelRGO).SuspendLayout();
			((Control)PanelILOCsiteReplace).SuspendLayout();
			((Control)panelHeader).SuspendLayout();
			((Control)panelDoubles).SuspendLayout();
			((Control)groupBox7).SuspendLayout();
			((Control)grpSetWDS).SuspendLayout();
			((Control)panelGraze).SuspendLayout();
			((Control)grpGrazeSites).SuspendLayout();
			((Control)grpGrazeDoubles).SuspendLayout();
			((Control)groupBox8).SuspendLayout();
			((Control)grpGrazeScopes).SuspendLayout();
			((Control)grpTelescopesAndNames).SuspendLayout();
			((Control)grpEventDuration).SuspendLayout();
			((Control)grpTelescopeCodes).SuspendLayout();
			((Control)grpGrazeDR).SuspendLayout();
			((Control)grp2ndLimit).SuspendLayout();
			((Control)grpDatums).SuspendLayout();
			((Control)grpGrazeRenumber).SuspendLayout();
			((Control)grpDuplicateGrazeEvents).SuspendLayout();
			((Control)groupBox11).SuspendLayout();
			((Control)grpLargeResiduals).SuspendLayout();
			((ISupportInitialize)updnSmoothTop).BeginInit();
			((ISupportInitialize)updnSmoothBottom).BeginInit();
			((ISupportInitialize)updnLimbCorrectedResidual).BeginInit();
			((Control)grpNullSeconds).SuspendLayout();
			((Control)grpGrazeFlag).SuspendLayout();
			((Control)panelCombine).SuspendLayout();
			((Control)groupBox9).SuspendLayout();
			((Control)panelResiduals).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)cmdAutoCorrectArchive).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAutoCorrectArchive).set_Location(new Point(5, 12));
			((Control)cmdAutoCorrectArchive).set_Name("cmdAutoCorrectArchive");
			((Control)cmdAutoCorrectArchive).set_Size(new Size(75, 44));
			((Control)cmdAutoCorrectArchive).set_TabIndex(29);
			((Control)cmdAutoCorrectArchive).set_Text("Auto-correct Archive file");
			((ButtonBase)cmdAutoCorrectArchive).set_UseVisualStyleBackColor(true);
			((Control)cmdAutoCorrectArchive).add_Click((EventHandler)cmdAutoCorrectArchive_Click);
			((Control)cmdReduceArchiveFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdReduceArchiveFile).set_Location(new Point(719, 12));
			((Control)cmdReduceArchiveFile).set_Name("cmdReduceArchiveFile");
			((Control)cmdReduceArchiveFile).set_Size(new Size(75, 44));
			((Control)cmdReduceArchiveFile).set_TabIndex(28);
			((Control)cmdReduceArchiveFile).set_Text("Reduce Archive file");
			((ButtonBase)cmdReduceArchiveFile).set_UseVisualStyleBackColor(true);
			((Control)cmdReduceArchiveFile).add_Click((EventHandler)cmdReduceArchiveFile_Click);
			((Control)lstEdit).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEdit).set_FormattingEnabled(true);
			lstEdit.set_HorizontalExtent(1880);
			lstEdit.set_HorizontalScrollbar(true);
			lstEdit.set_ItemHeight(14);
			((Control)lstEdit).set_Location(new Point(12, 99));
			((Control)lstEdit).set_Name("lstEdit");
			((Control)lstEdit).set_Size(new Size(1104, 298));
			((Control)lstEdit).set_TabIndex(30);
			lstEdit.add_SelectedIndexChanged((EventHandler)lstEdit_SelectedIndexChanged);
			((Control)cmdListLargeResiduals).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdListLargeResiduals).set_Location(new Point(95, 13));
			((Control)cmdListLargeResiduals).set_Name("cmdListLargeResiduals");
			((Control)cmdListLargeResiduals).set_Size(new Size(75, 44));
			((Control)cmdListLargeResiduals).set_TabIndex(31);
			((Control)cmdListLargeResiduals).set_Text("Edit events");
			((ButtonBase)cmdListLargeResiduals).set_UseVisualStyleBackColor(true);
			((Control)cmdListLargeResiduals).add_Click((EventHandler)cmdListLargeResiduals_Click);
			((Control)pnlPre1750).set_Anchor((AnchorStyles)1);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)panel1);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)cmdGeneratePrediction);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)chkSecs);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)cmdAdjustedReduce);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)label14);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)updnSecond);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)label13);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)updnPE);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)label11);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)label10);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)label9);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)label8);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)label7);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)updnDay);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)updnHour);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)updnMinute);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)updnMonth);
			((Control)pnlPre1750).get_Controls().Add((Control)(object)updnYear);
			((Control)pnlPre1750).set_Location(new Point(9, 50));
			((Control)pnlPre1750).set_Name("pnlPre1750");
			((Control)pnlPre1750).set_Size(new Size(538, 91));
			((Control)pnlPre1750).set_TabIndex(32);
			pnlPre1750.set_TabStop(false);
			((Control)pnlPre1750).set_Text("Adjustments - use the following date/time and star");
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)label65);
			((Control)panel1).get_Controls().Add((Control)(object)cmbCertainty);
			((Control)panel1).get_Controls().Add((Control)(object)chkAdjAcc);
			((Control)panel1).get_Controls().Add((Control)(object)label64);
			((Control)panel1).get_Controls().Add((Control)(object)updnAcc);
			((Control)panel1).set_Location(new Point(350, 16));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(101, 72));
			((Control)panel1).set_TabIndex(64);
			((Control)label65).set_AutoSize(true);
			((Control)label65).set_Location(new Point(17, 35));
			((Control)label65).set_Name("label65");
			((Control)label65).set_Size(new Size(48, 13));
			((Control)label65).set_TabIndex(63);
			((Control)label65).set_Text("Certainty");
			((Control)cmbCertainty).set_Enabled(false);
			((ListControl)cmbCertainty).set_FormattingEnabled(true);
			cmbCertainty.get_Items().AddRange(new object[4] { "Not set", "Sure", "Pos. Spurious", "Spurious" });
			((Control)cmbCertainty).set_Location(new Point(3, 48));
			((Control)cmbCertainty).set_Name("cmbCertainty");
			((Control)cmbCertainty).set_Size(new Size(94, 21));
			((Control)cmbCertainty).set_TabIndex(62);
			((Control)chkAdjAcc).set_AutoSize(true);
			chkAdjAcc.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)chkAdjAcc).set_Location(new Point(2, 2));
			((Control)chkAdjAcc).set_Name("chkAdjAcc");
			((Control)chkAdjAcc).set_Size(new Size(40, 31));
			((Control)chkAdjAcc).set_TabIndex(61);
			((Control)chkAdjAcc).set_Text("Adjust");
			((ButtonBase)chkAdjAcc).set_UseVisualStyleBackColor(true);
			chkAdjAcc.add_CheckedChanged((EventHandler)chkAdjAcc_CheckedChanged);
			((Control)label64).set_AutoSize(true);
			((Control)label64).set_Location(new Point(44, 1));
			((Control)label64).set_Name("label64");
			((Control)label64).set_Size(new Size(52, 13));
			((Control)label64).set_TabIndex(60);
			((Control)label64).set_Text("Accuracy");
			updnAcc.set_DecimalPlaces(1);
			((Control)updnAcc).set_Enabled(false);
			((Control)updnAcc).set_Location(new Point(49, 14));
			updnAcc.set_Maximum(new decimal(new int[4] { 999, 0, 0, 65536 }));
			((Control)updnAcc).set_Name("updnAcc");
			((Control)updnAcc).set_Size(new Size(43, 20));
			((Control)updnAcc).set_TabIndex(59);
			updnAcc.set_Value(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)cmdGeneratePrediction).set_Location(new Point(459, 45));
			((Control)cmdGeneratePrediction).set_Name("cmdGeneratePrediction");
			((Control)cmdGeneratePrediction).set_Size(new Size(64, 30));
			((Control)cmdGeneratePrediction).set_TabIndex(58);
			((Control)cmdGeneratePrediction).set_Text("Prediction");
			((ButtonBase)cmdGeneratePrediction).set_UseVisualStyleBackColor(true);
			((Control)cmdGeneratePrediction).add_Click((EventHandler)cmdGeneratePrediction_Click);
			((Control)chkSecs).set_AutoSize(true);
			((Control)chkSecs).set_Location(new Point(241, 53));
			((Control)chkSecs).set_Name("chkSecs");
			((Control)chkSecs).set_Size(new Size(66, 17));
			((Control)chkSecs).set_TabIndex(32);
			((Control)chkSecs).set_Text("Adj secs");
			((ButtonBase)chkSecs).set_UseVisualStyleBackColor(true);
			chkSecs.add_CheckedChanged((EventHandler)chkSecs_CheckedChanged);
			((Control)cmdAdjustedReduce).set_Location(new Point(459, 12));
			((Control)cmdAdjustedReduce).set_Name("cmdAdjustedReduce");
			((Control)cmdAdjustedReduce).set_Size(new Size(64, 30));
			((Control)cmdAdjustedReduce).set_TabIndex(27);
			((Control)cmdAdjustedReduce).set_Text("Compute");
			((ButtonBase)cmdAdjustedReduce).set_UseVisualStyleBackColor(true);
			((Control)cmdAdjustedReduce).add_Click((EventHandler)cmdAdjustedReduce_Click);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(238, 17));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(26, 13));
			((Control)label14).set_TabIndex(31);
			((Control)label14).set_Text("Sec");
			updnSecond.set_DecimalPlaces(2);
			((Control)updnSecond).set_Enabled(false);
			((Control)updnSecond).set_Location(new Point(241, 30));
			updnSecond.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)updnSecond).set_Name("updnSecond");
			((Control)updnSecond).set_Size(new Size(51, 20));
			((Control)updnSecond).set_TabIndex(30);
			updnSecond.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnSecond.add_ValueChanged((EventHandler)updnSecond_ValueChanged);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(291, 17));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(61, 13));
			((Control)label13).set_TabIndex(29);
			((Control)label13).set_Text("PE to apply");
			updnPE.set_DecimalPlaces(2);
			((Control)updnPE).set_Enabled(false);
			((Control)updnPE).set_Location(new Point(302, 30));
			updnPE.set_Maximum(new decimal(new int[4] { 99, 0, 0, 131072 }));
			((Control)updnPE).set_Name("updnPE");
			((Control)updnPE).set_Size(new Size(43, 20));
			((Control)updnPE).set_TabIndex(28);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(69, 17));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(37, 13));
			((Control)label11).set_TabIndex(9);
			((Control)label11).set_Text("Month");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(110, 17));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(26, 13));
			((Control)label10).set_TabIndex(8);
			((Control)label10).set_Text("Day");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(152, 16));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(30, 13));
			((Control)label9).set_TabIndex(7);
			((Control)label9).set_Text("Hour");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(195, 17));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(27, 13));
			((Control)label8).set_TabIndex(6);
			((Control)label8).set_Text("Min.");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(16, 16));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(29, 13));
			((Control)label7).set_TabIndex(5);
			((Control)label7).set_Text("Year");
			((Control)updnDay).set_Location(new Point(114, 30));
			updnDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(34, 20));
			((Control)updnDay).set_TabIndex(4);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnDay.add_ValueChanged((EventHandler)updnDay_ValueChanged);
			((Control)updnHour).set_Location(new Point(156, 30));
			updnHour.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(34, 20));
			((Control)updnHour).set_TabIndex(3);
			updnHour.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnHour.add_ValueChanged((EventHandler)updnHour_ValueChanged);
			((Control)updnMinute).set_Location(new Point(198, 30));
			updnMinute.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)updnMinute).set_Name("updnMinute");
			((Control)updnMinute).set_Size(new Size(34, 20));
			((Control)updnMinute).set_TabIndex(2);
			updnMinute.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnMinute.add_ValueChanged((EventHandler)updnMinute_ValueChanged);
			((Control)updnMonth).set_Location(new Point(72, 30));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(34, 20));
			((Control)updnMonth).set_TabIndex(1);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnMonth.add_ValueChanged((EventHandler)updnMonth_ValueChanged);
			((Control)updnYear).set_Location(new Point(16, 30));
			updnYear.set_Maximum(new decimal(new int[4] { 2200, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 900, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(48, 20));
			((Control)updnYear).set_TabIndex(0);
			updnYear.set_Value(new decimal(new int[4] { 2008, 0, 0, 0 }));
			updnYear.add_ValueChanged((EventHandler)updnYear_ValueChanged);
			((Control)optXZ).set_AutoSize(true);
			optXZ.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optXZ).set_Location(new Point(31, 45));
			((Control)optXZ).set_Name("optXZ");
			((Control)optXZ).set_Size(new Size(39, 17));
			((Control)optXZ).set_TabIndex(14);
			((Control)optXZ).set_Text("XZ");
			((ButtonBase)optXZ).set_UseVisualStyleBackColor(true);
			((Control)optXZ).add_Click((EventHandler)optXZ_Click);
			((Control)optSAO).set_AutoSize(true);
			optSAO.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optSAO).set_Location(new Point(23, 28));
			((Control)optSAO).set_Name("optSAO");
			((Control)optSAO).set_Size(new Size(47, 17));
			((Control)optSAO).set_TabIndex(13);
			((Control)optSAO).set_Text("SAO");
			((ButtonBase)optSAO).set_UseVisualStyleBackColor(true);
			((Control)optSAO).add_Click((EventHandler)optSAO_Click);
			((Control)optZC).set_AutoSize(true);
			optZC.set_CheckAlign(ContentAlignment.MiddleRight);
			optZC.set_Checked(true);
			((Control)optZC).set_Location(new Point(31, 11));
			((Control)optZC).set_Name("optZC");
			((Control)optZC).set_Size(new Size(39, 17));
			((Control)optZC).set_TabIndex(12);
			optZC.set_TabStop(true);
			((Control)optZC).set_Text("ZC");
			((ButtonBase)optZC).set_UseVisualStyleBackColor(true);
			((Control)optZC).add_Click((EventHandler)optZC_Click);
			((Control)txtStar).set_Location(new Point(90, 32));
			((Control)txtStar).set_Name("txtStar");
			((Control)txtStar).set_Size(new Size(57, 20));
			((Control)txtStar).set_TabIndex(11);
			((Control)txtStar).add_TextChanged((EventHandler)txtStar_TextChanged);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(89, 17));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(44, 13));
			((Control)label12).set_TabIndex(10);
			((Control)label12).set_Text("Number");
			((Control)label1).set_Anchor((AnchorStyles)1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(202, 6));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(700, 14));
			((Control)label1).set_TabIndex(33);
			((Control)label1).set_Text("    O-C    PA       l     b     AA    Limb Scale    P     D    MoonAlt  SunAlt  %ill   mag  Tel-dia");
			((Control)lblResidual).set_Anchor((AnchorStyles)1);
			((Control)lblResidual).set_AutoSize(true);
			((Control)lblResidual).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblResidual).set_Location(new Point(202, 20));
			((Control)lblResidual).set_Name("lblResidual");
			((Control)lblResidual).set_Size(new Size(14, 14));
			((Control)lblResidual).set_TabIndex(34);
			((Control)lblResidual).set_Text("=");
			((Control)cmdIdentify).set_Location(new Point(8, 119));
			((Control)cmdIdentify).set_Name("cmdIdentify");
			((Control)cmdIdentify).set_Size(new Size(109, 28));
			((Control)cmdIdentify).set_TabIndex(32);
			((Control)cmdIdentify).set_Text("Identify stars");
			((ButtonBase)cmdIdentify).set_UseVisualStyleBackColor(true);
			((Control)cmdIdentify).add_Click((EventHandler)cmdIdentify_Click);
			((Control)cmdGoogleEarth).set_Location(new Point(408, 26));
			((Control)cmdGoogleEarth).set_Name("cmdGoogleEarth");
			((Control)cmdGoogleEarth).set_Size(new Size(72, 56));
			((Control)cmdGoogleEarth).set_TabIndex(35);
			((Control)cmdGoogleEarth).set_Text("Plot Site in Google Earth");
			((ButtonBase)cmdGoogleEarth).set_UseVisualStyleBackColor(true);
			((Control)cmdGoogleEarth).add_Click((EventHandler)cmdGoogleEarth_Click);
			updnLongSec.set_DecimalPlaces(1);
			((Control)updnLongSec).set_Enabled(false);
			((Control)updnLongSec).set_Location(new Point(136, 35));
			updnLongSec.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)updnLongSec).set_Name("updnLongSec");
			((Control)updnLongSec).set_Size(new Size(51, 20));
			((Control)updnLongSec).set_TabIndex(35);
			updnLongSec.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnLongD).set_Location(new Point(43, 35));
			updnLongD.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			((Control)updnLongD).set_Name("updnLongD");
			((Control)updnLongD).set_Size(new Size(44, 20));
			((Control)updnLongD).set_TabIndex(34);
			updnLongD.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnLongM).set_Location(new Point(93, 35));
			updnLongM.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)updnLongM).set_Name("updnLongM");
			((Control)updnLongM).set_Size(new Size(34, 20));
			((Control)updnLongM).set_TabIndex(33);
			updnLongM.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnLatS.set_DecimalPlaces(1);
			((Control)updnLatS).set_Enabled(false);
			((Control)updnLatS).set_Location(new Point(129, 34));
			updnLatS.set_Maximum(new decimal(new int[4] { 5999, 0, 0, 131072 }));
			((Control)updnLatS).set_Name("updnLatS");
			((Control)updnLatS).set_Size(new Size(51, 20));
			((Control)updnLatS).set_TabIndex(38);
			updnLatS.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnLatD).set_Location(new Point(40, 34));
			updnLatD.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			((Control)updnLatD).set_Name("updnLatD");
			((Control)updnLatD).set_Size(new Size(38, 20));
			((Control)updnLatD).set_TabIndex(37);
			updnLatD.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnLatM).set_Location(new Point(86, 34));
			updnLatM.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)updnLatM).set_Name("updnLatM");
			((Control)updnLatM).set_Size(new Size(34, 20));
			((Control)updnLatM).set_TabIndex(36);
			updnLatM.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)groupBox1).get_Controls().Add((Control)(object)updnLongD);
			((Control)groupBox1).get_Controls().Add((Control)(object)label6);
			((Control)groupBox1).get_Controls().Add((Control)(object)label4);
			((Control)groupBox1).get_Controls().Add((Control)(object)label2);
			((Control)groupBox1).get_Controls().Add((Control)(object)optW);
			((Control)groupBox1).get_Controls().Add((Control)(object)optE);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnLongSec);
			((Control)groupBox1).get_Controls().Add((Control)(object)updnLongM);
			((Control)groupBox1).set_Location(new Point(9, 19));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(196, 70));
			((Control)groupBox1).set_TabIndex(39);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Longitude");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(44, 19));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(27, 13));
			((Control)label6).set_TabIndex(40);
			((Control)label6).set_Text("Deg");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(133, 19));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(26, 13));
			((Control)label4).set_TabIndex(39);
			((Control)label4).set_Text("Sec");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(93, 19));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(27, 13));
			((Control)label2).set_TabIndex(38);
			((Control)label2).set_Text("Min.");
			((Control)optW).set_AutoSize(true);
			((Control)optW).set_Location(new Point(8, 36));
			((Control)optW).set_Name("optW");
			((Control)optW).set_Size(new Size(36, 17));
			((Control)optW).set_TabIndex(37);
			((Control)optW).set_Text("W");
			((ButtonBase)optW).set_UseVisualStyleBackColor(true);
			optW.add_CheckedChanged((EventHandler)optW_CheckedChanged);
			((Control)optE).set_AutoSize(true);
			optE.set_Checked(true);
			((Control)optE).set_Location(new Point(8, 19));
			((Control)optE).set_Name("optE");
			((Control)optE).set_Size(new Size(32, 17));
			((Control)optE).set_TabIndex(36);
			optE.set_TabStop(true);
			((Control)optE).set_Text("E");
			((ButtonBase)optE).set_UseVisualStyleBackColor(true);
			((Control)groupBox2).get_Controls().Add((Control)(object)label15);
			((Control)groupBox2).get_Controls().Add((Control)(object)label5);
			((Control)groupBox2).get_Controls().Add((Control)(object)label3);
			((Control)groupBox2).get_Controls().Add((Control)(object)optS);
			((Control)groupBox2).get_Controls().Add((Control)(object)optN);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnLatS);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnLatD);
			((Control)groupBox2).get_Controls().Add((Control)(object)updnLatM);
			((Control)groupBox2).set_Location(new Point(211, 20));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(191, 69));
			((Control)groupBox2).set_TabIndex(40);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Latitude");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(37, 18));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(27, 13));
			((Control)label15).set_TabIndex(43);
			((Control)label15).set_Text("Deg");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(126, 18));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(26, 13));
			((Control)label5).set_TabIndex(42);
			((Control)label5).set_Text("Sec");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(83, 18));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(27, 13));
			((Control)label3).set_TabIndex(41);
			((Control)label3).set_Text("Min.");
			((Control)optS).set_AutoSize(true);
			((Control)optS).set_Location(new Point(7, 36));
			((Control)optS).set_Name("optS");
			((Control)optS).set_Size(new Size(32, 17));
			((Control)optS).set_TabIndex(40);
			((Control)optS).set_Text("S");
			((ButtonBase)optS).set_UseVisualStyleBackColor(true);
			((Control)optN).set_AutoSize(true);
			optN.set_Checked(true);
			((Control)optN).set_Location(new Point(7, 19));
			((Control)optN).set_Name("optN");
			((Control)optN).set_Size(new Size(33, 17));
			((Control)optN).set_TabIndex(39);
			optN.set_TabStop(true);
			((Control)optN).set_Text("N");
			((ButtonBase)optN).set_UseVisualStyleBackColor(true);
			((Control)optA).set_AutoSize(true);
			((Control)optA).set_Location(new Point(12, 48));
			((Control)optA).set_Name("optA");
			((Control)optA).set_Size(new Size(64, 17));
			((Control)optA).set_TabIndex(36);
			((Control)optA).set_Text("A - Time");
			((ButtonBase)optA).set_UseVisualStyleBackColor(true);
			((Control)optF).set_AutoSize(true);
			((Control)optF).set_Location(new Point(12, 133));
			((Control)optF).set_Name("optF");
			((Control)optF).set_Size(new Size(127, 17));
			((Control)optF).set_TabIndex(38);
			((Control)optF).set_Text("F - Site + Star number");
			((ButtonBase)optF).set_UseVisualStyleBackColor(true);
			((Control)optD).set_AutoSize(true);
			((Control)optD).set_Location(new Point(12, 99));
			((Control)optD).set_Name("optD");
			((Control)optD).set_Size(new Size(60, 17));
			((Control)optD).set_TabIndex(39);
			((Control)optD).set_Text("D - Site");
			((ButtonBase)optD).set_UseVisualStyleBackColor(true);
			((Control)optC).set_AutoSize(true);
			((Control)optC).set_Location(new Point(12, 82));
			((Control)optC).set_Name("optC");
			((Control)optC).set_Size(new Size(133, 17));
			((Control)optC).set_TabIndex(40);
			((Control)optC).set_Text("C - Time + Star number");
			((ButtonBase)optC).set_UseVisualStyleBackColor(true);
			((Control)optB).set_AutoSize(true);
			((Control)optB).set_Location(new Point(12, 65));
			((Control)optB).set_Name("optB");
			((Control)optB).set_Size(new Size(98, 17));
			((Control)optB).set_TabIndex(41);
			((Control)optB).set_Text("B - Star number");
			((ButtonBase)optB).set_UseVisualStyleBackColor(true);
			((Control)optEc).set_AutoSize(true);
			((Control)optEc).set_Location(new Point(12, 116));
			((Control)optEc).set_Name("optEc");
			((Control)optEc).set_Size(new Size(94, 17));
			((Control)optEc).set_TabIndex(42);
			((Control)optEc).set_Text("E - Site + Time");
			((ButtonBase)optEc).set_UseVisualStyleBackColor(true);
			((Control)groupBox3).set_Anchor((AnchorStyles)1);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkReReducedOldObs);
			((Control)groupBox3).get_Controls().Add((Control)(object)optNoCorrection);
			((Control)groupBox3).get_Controls().Add((Control)(object)optU);
			((Control)groupBox3).get_Controls().Add((Control)(object)optG);
			((Control)groupBox3).get_Controls().Add((Control)(object)optSs);
			((Control)groupBox3).get_Controls().Add((Control)(object)label17);
			((Control)groupBox3).get_Controls().Add((Control)(object)label16);
			((Control)groupBox3).get_Controls().Add((Control)(object)optT);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdCorrect);
			((Control)groupBox3).get_Controls().Add((Control)(object)optWe);
			((Control)groupBox3).get_Controls().Add((Control)(object)optZ);
			((Control)groupBox3).get_Controls().Add((Control)(object)optX);
			((Control)groupBox3).get_Controls().Add((Control)(object)optY);
			((Control)groupBox3).get_Controls().Add((Control)(object)optEc);
			((Control)groupBox3).get_Controls().Add((Control)(object)optB);
			((Control)groupBox3).get_Controls().Add((Control)(object)optC);
			((Control)groupBox3).get_Controls().Add((Control)(object)optD);
			((Control)groupBox3).get_Controls().Add((Control)(object)optF);
			((Control)groupBox3).get_Controls().Add((Control)(object)optA);
			((Control)groupBox3).set_Location(new Point(721, 37));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(344, 203));
			((Control)groupBox3).set_TabIndex(43);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Correct the record");
			((Control)chkReReducedOldObs).set_AutoSize(true);
			chkReReducedOldObs.set_CheckAlign(ContentAlignment.TopLeft);
			((Control)chkReReducedOldObs).set_Location(new Point(12, 169));
			((Control)chkReReducedOldObs).set_Name("chkReReducedOldObs");
			((Control)chkReReducedOldObs).set_Size(new Size(157, 30));
			((Control)chkReReducedOldObs).set_TabIndex(54);
			((Control)chkReReducedOldObs).set_Text("Re-reduced old observation\r\n(before 1750)");
			((ButtonBase)chkReReducedOldObs).set_TextAlign(ContentAlignment.BottomCenter);
			((ButtonBase)chkReReducedOldObs).set_UseVisualStyleBackColor(true);
			((Control)optNoCorrection).set_AutoSize(true);
			((Control)optNoCorrection).set_Location(new Point(12, 14));
			((Control)optNoCorrection).set_Name("optNoCorrection");
			((Control)optNoCorrection).set_Size(new Size(68, 17));
			((Control)optNoCorrection).set_TabIndex(53);
			((Control)optNoCorrection).set_Text("OK 'as is'");
			((ButtonBase)optNoCorrection).set_UseVisualStyleBackColor(true);
			((Control)optU).set_AutoSize(true);
			((Control)optU).set_Location(new Point(163, 65));
			((Control)optU).set_Name("optU");
			((Control)optU).set_Size(new Size(118, 17));
			((Control)optU).set_TabIndex(52);
			((Control)optU).set_Text("U - Unidentified star");
			((ButtonBase)optU).set_UseVisualStyleBackColor(true);
			((Control)optG).set_AutoSize(true);
			((Control)optG).set_Location(new Point(12, 150));
			((Control)optG).set_Name("optG");
			((Control)optG).set_Size(new Size(144, 17));
			((Control)optG).set_TabIndex(51);
			((Control)optG).set_Text("G - Site + Time + Star no.");
			((ButtonBase)optG).set_UseVisualStyleBackColor(true);
			((Control)optSs).set_AutoSize(true);
			((Control)optSs).set_Enabled(false);
			((Control)optSs).set_Location(new Point(163, 31));
			((Control)optSs).set_Name("optSs");
			((Control)optSs).set_Size(new Size(146, 17));
			((Control)optSs).set_TabIndex(50);
			((Control)optSs).set_Text("S - Unknown site location");
			((ButtonBase)optSs).set_UseVisualStyleBackColor(true);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(9, 32));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(98, 13));
			((Control)label17).set_TabIndex(49);
			((Control)label17).set_Text("Corrected event");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(159, 16));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(153, 13));
			((Control)label16).set_TabIndex(48);
			((Control)label16).set_Text("No satisfactory correction");
			((Control)optT).set_AutoSize(true);
			((Control)optT).set_Enabled(false);
			((Control)optT).set_Location(new Point(163, 48));
			((Control)optT).set_Name("optT");
			((Control)optT).set_Size(new Size(183, 17));
			((Control)optT).set_TabIndex(47);
			((Control)optT).set_Text("T - poor time base (pre 1850 only)");
			((ButtonBase)optT).set_UseVisualStyleBackColor(true);
			((Control)cmdCorrect).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCorrect).set_Location(new Point(201, 159));
			((Control)cmdCorrect).set_Name("cmdCorrect");
			((Control)cmdCorrect).set_Size(new Size(113, 40));
			((Control)cmdCorrect).set_TabIndex(44);
			((Control)cmdCorrect).set_Text("Write correction to file");
			((ButtonBase)cmdCorrect).set_UseVisualStyleBackColor(true);
			((Control)cmdCorrect).add_Click((EventHandler)cmdCorrect_Click);
			((Control)optWe).set_AutoSize(true);
			((Control)optWe).set_Location(new Point(163, 82));
			((Control)optWe).set_Name("optWe");
			((Control)optWe).set_Size(new Size(135, 17));
			((Control)optWe).set_TabIndex(46);
			((Control)optWe).set_Text("W - Invalid site location");
			((ButtonBase)optWe).set_UseVisualStyleBackColor(true);
			((Control)optZ).set_AutoSize(true);
			((Control)optZ).set_Location(new Point(163, 133));
			((Control)optZ).set_Name("optZ");
			((Control)optZ).set_Size(new Size(148, 17));
			((Control)optZ).set_TabIndex(45);
			((Control)optZ).set_Text("Z - Unresolved data errors");
			((ButtonBase)optZ).set_UseVisualStyleBackColor(true);
			((Control)optX).set_AutoSize(true);
			((Control)optX).set_Location(new Point(163, 99));
			((Control)optX).set_Name("optX");
			((Control)optX).set_Size(new Size(130, 17));
			((Control)optX).set_TabIndex(44);
			((Control)optX).set_Text("X - Invalid observation");
			((ButtonBase)optX).set_UseVisualStyleBackColor(true);
			((Control)optY).set_AutoSize(true);
			optY.set_Checked(true);
			((Control)optY).set_Location(new Point(163, 116));
			((Control)optY).set_Name("optY");
			((Control)optY).set_Size(new Size(157, 17));
			((Control)optY).set_TabIndex(43);
			optY.set_TabStop(true);
			((Control)optY).set_Text("Y - Auto-assessed as invalid");
			((ButtonBase)optY).set_UseVisualStyleBackColor(true);
			((Control)optPlanet).set_AutoSize(true);
			optPlanet.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optPlanet).set_Location(new Point(15, 62));
			((Control)optPlanet).set_Name("optPlanet");
			((Control)optPlanet).set_Size(new Size(55, 17));
			((Control)optPlanet).set_TabIndex(15);
			((Control)optPlanet).set_Text("Planet");
			((ButtonBase)optPlanet).set_UseVisualStyleBackColor(true);
			((Control)optPlanet).add_Click((EventHandler)optPlanet_Click);
			((Control)groupBox4).set_Anchor((AnchorStyles)1);
			((Control)groupBox4).get_Controls().Add((Control)(object)label72);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmbWDS_General);
			((Control)groupBox4).get_Controls().Add((Control)(object)optAsteroid);
			((Control)groupBox4).get_Controls().Add((Control)(object)optUnknown);
			((Control)groupBox4).get_Controls().Add((Control)(object)optPlanet);
			((Control)groupBox4).get_Controls().Add((Control)(object)optSAO);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdIdentify);
			((Control)groupBox4).get_Controls().Add((Control)(object)optXZ);
			((Control)groupBox4).get_Controls().Add((Control)(object)label12);
			((Control)groupBox4).get_Controls().Add((Control)(object)txtStar);
			((Control)groupBox4).get_Controls().Add((Control)(object)optZC);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkDR);
			((Control)groupBox4).set_Location(new Point(554, 77));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(162, 162));
			((Control)groupBox4).set_TabIndex(44);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("Star ID");
			((Control)label72).set_AutoSize(true);
			((Control)label72).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label72).set_Location(new Point(105, 60));
			((Control)label72).set_Name("label72");
			((Control)label72).set_Size(new Size(27, 12));
			((Control)label72).set_TabIndex(37);
			((Control)label72).set_Text("WDS");
			cmbWDS_General.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbWDS_General).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbWDS_General).set_FormattingEnabled(true);
			cmbWDS_General.get_Items().AddRange(new object[53]
			{
				" ", "A", "B", "C", "D", "E", "F", "G", "H", "I",
				"J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
				"T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c",
				"d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
				"n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
				"x", "y", "z"
			});
			((Control)cmbWDS_General).set_Location(new Point(101, 75));
			cmbWDS_General.set_MaxDropDownItems(20);
			((Control)cmbWDS_General).set_Name("cmbWDS_General");
			((Control)cmbWDS_General).set_Size(new Size(35, 21));
			((Control)cmbWDS_General).set_TabIndex(36);
			cmbWDS_General.add_SelectedIndexChanged((EventHandler)cmbWDS_General_SelectedIndexChanged);
			((Control)optAsteroid).set_AutoSize(true);
			optAsteroid.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optAsteroid).set_Location(new Point(7, 79));
			((Control)optAsteroid).set_Name("optAsteroid");
			((Control)optAsteroid).set_Size(new Size(63, 17));
			((Control)optAsteroid).set_TabIndex(35);
			((Control)optAsteroid).set_Text("Asteroid");
			((ButtonBase)optAsteroid).set_UseVisualStyleBackColor(true);
			((Control)optAsteroid).add_Click((EventHandler)optAsteroid_Click);
			((Control)optUnknown).set_AutoSize(true);
			optUnknown.set_CheckAlign(ContentAlignment.MiddleRight);
			((ButtonBase)optUnknown).set_ImageAlign(ContentAlignment.MiddleRight);
			((Control)optUnknown).set_Location(new Point(1, 96));
			((Control)optUnknown).set_Name("optUnknown");
			((Control)optUnknown).set_Size(new Size(69, 17));
			((Control)optUnknown).set_TabIndex(34);
			((Control)optUnknown).set_Text("unknown");
			((ButtonBase)optUnknown).set_UseVisualStyleBackColor(true);
			optUnknown.add_CheckedChanged((EventHandler)optUnknown_CheckedChanged);
			((Control)chkDR).set_AutoSize(true);
			chkDR.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)chkDR).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDR).set_Location(new Point(119, 105));
			((Control)chkDR).set_Name("chkDR");
			((Control)chkDR).set_Size(new Size(41, 44));
			((Control)chkDR).set_TabIndex(33);
			((Control)chkDR).set_Text("Swap \r\nD && R");
			((ButtonBase)chkDR).set_UseVisualStyleBackColor(true);
			((Control)groupBox5).set_Anchor((AnchorStyles)1);
			((Control)groupBox5).get_Controls().Add((Control)(object)groupBox1);
			((Control)groupBox5).get_Controls().Add((Control)(object)groupBox2);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdGoogleEarth);
			((Control)groupBox5).set_Location(new Point(9, 142));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(495, 97));
			((Control)groupBox5).set_TabIndex(45);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("Site location");
			((Control)cmdSortArchive).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSortArchive).set_Location(new Point(507, 12));
			((Control)cmdSortArchive).set_Name("cmdSortArchive");
			((Control)cmdSortArchive).set_Size(new Size(75, 35));
			((Control)cmdSortArchive).set_TabIndex(46);
			((Control)cmdSortArchive).set_Text("Sort Archive file");
			((ButtonBase)cmdSortArchive).set_UseVisualStyleBackColor(true);
			((Control)cmdSortArchive).add_Click((EventHandler)cmdSortArchive_Click);
			((Control)cmdMergeArchiveFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMergeArchiveFiles).set_Location(new Point(606, 12));
			((Control)cmdMergeArchiveFiles).set_Name("cmdMergeArchiveFiles");
			((Control)cmdMergeArchiveFiles).set_Size(new Size(75, 35));
			((Control)cmdMergeArchiveFiles).set_TabIndex(47);
			((Control)cmdMergeArchiveFiles).set_Text("Merge && sort Archive files");
			((ButtonBase)cmdMergeArchiveFiles).set_UseVisualStyleBackColor(true);
			((Control)cmdMergeArchiveFiles).add_Click((EventHandler)cmdMergeArchiveFiles_Click);
			((Control)optAll).set_AutoSize(true);
			((Control)optAll).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optAll).set_Location(new Point(6, 35));
			((Control)optAll).set_Name("optAll");
			((Control)optAll).set_Size(new Size(96, 17));
			((Control)optAll).set_TabIndex(2);
			((Control)optAll).set_Text("All  in yr (+ mth)");
			((ButtonBase)optAll).set_UseVisualStyleBackColor(true);
			((Control)optUnresolved).set_AutoSize(true);
			((Control)optUnresolved).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optUnresolved).set_Location(new Point(6, 16));
			((Control)optUnresolved).set_Name("optUnresolved");
			((Control)optUnresolved).set_Size(new Size(144, 17));
			((Control)optUnresolved).set_TabIndex(1);
			((Control)optUnresolved).set_Text("Unresolved events [S - Z]");
			((ButtonBase)optUnresolved).set_UseVisualStyleBackColor(true);
			((Control)optCorrected).set_AutoSize(true);
			((Control)optCorrected).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optCorrected).set_Location(new Point(6, 54));
			((Control)optCorrected).set_Name("optCorrected");
			((Control)optCorrected).set_Size(new Size(105, 17));
			((Control)optCorrected).set_TabIndex(6);
			((Control)optCorrected).set_Text("Corrected events");
			((ButtonBase)optCorrected).set_UseVisualStyleBackColor(true);
			((Control)optUnconsidered).set_AutoSize(true);
			optUnconsidered.set_Checked(true);
			((Control)optUnconsidered).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optUnconsidered).set_Location(new Point(6, 1));
			((Control)optUnconsidered).set_Name("optUnconsidered");
			((Control)optUnconsidered).set_Size(new Size(153, 17));
			((Control)optUnconsidered).set_TabIndex(0);
			optUnconsidered.set_TabStop(true);
			((Control)optUnconsidered).set_Text("Unconsidered events [U, Y]");
			((ButtonBase)optUnconsidered).set_UseVisualStyleBackColor(true);
			((Control)cmdRetrieveOriginal).set_Anchor((AnchorStyles)1);
			((Control)cmdRetrieveOriginal).set_Location(new Point(4, 16));
			((Control)cmdRetrieveOriginal).set_Name("cmdRetrieveOriginal");
			((Control)cmdRetrieveOriginal).set_Size(new Size(124, 22));
			((Control)cmdRetrieveOriginal).set_TabIndex(52);
			((Control)cmdRetrieveOriginal).set_Text("Retrieve original data");
			((ButtonBase)cmdRetrieveOriginal).set_UseVisualStyleBackColor(true);
			((Control)cmdRetrieveOriginal).add_Click((EventHandler)cmdRetrieveOriginal_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)saveListToolStripMenuItem,
				(ToolStripItem)appendListToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)withCheckedListToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1128, 24));
			((Control)menuStrip1).set_TabIndex(53);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)saveListToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveListToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveListToolStripMenuItem).set_Name("saveListToolStripMenuItem");
			((ToolStripItem)saveListToolStripMenuItem).set_Size(new Size(92, 20));
			((ToolStripItem)saveListToolStripMenuItem).set_Text("Save list     ");
			((ToolStripItem)saveListToolStripMenuItem).add_Click((EventHandler)saveListToolStripMenuItem_Click);
			((ToolStripItem)appendListToolStripMenuItem).set_Name("appendListToolStripMenuItem");
			((ToolStripItem)appendListToolStripMenuItem).set_Size(new Size(94, 20));
			((ToolStripItem)appendListToolStripMenuItem).set_Text("Append List    ");
			((ToolStripItem)appendListToolStripMenuItem).add_Click((EventHandler)appendListToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripDropDownItem)withCheckedListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)checkAllToolStripMenuItem,
				(ToolStripItem)clearAllToolStripMenuItem,
				(ToolStripItem)invertSelectionToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)checkAllSourcedFromOToolStripMenuItem,
				(ToolStripItem)checkAllSourcedFrToolStripMenuItem,
				(ToolStripItem)checkAllSourcedFromSToolStripMenuItem
			});
			((ToolStripItem)withCheckedListToolStripMenuItem).set_Name("withCheckedListToolStripMenuItem");
			((ToolStripItem)withCheckedListToolStripMenuItem).set_Size(new Size(145, 20));
			((ToolStripItem)withCheckedListToolStripMenuItem).set_Text("     with Checked List...   ");
			((ToolStripItem)checkAllToolStripMenuItem).set_Name("checkAllToolStripMenuItem");
			((ToolStripItem)checkAllToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)checkAllToolStripMenuItem).set_Text("Check all");
			((ToolStripItem)checkAllToolStripMenuItem).add_Click((EventHandler)checkAllToolStripMenuItem_Click);
			((ToolStripItem)clearAllToolStripMenuItem).set_Name("clearAllToolStripMenuItem");
			((ToolStripItem)clearAllToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)clearAllToolStripMenuItem).set_Text("Clear all");
			((ToolStripItem)clearAllToolStripMenuItem).add_Click((EventHandler)clearAllToolStripMenuItem_Click);
			((ToolStripItem)invertSelectionToolStripMenuItem).set_Name("invertSelectionToolStripMenuItem");
			((ToolStripItem)invertSelectionToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)invertSelectionToolStripMenuItem).set_Text("Invert selection");
			((ToolStripItem)invertSelectionToolStripMenuItem).add_Click((EventHandler)invertSelectionToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(211, 6));
			((ToolStripItem)checkAllSourcedFromOToolStripMenuItem).set_Name("checkAllSourcedFromOToolStripMenuItem");
			((ToolStripItem)checkAllSourcedFromOToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)checkAllSourcedFromOToolStripMenuItem).set_Text("Check all sourced from 'O'");
			((ToolStripItem)checkAllSourcedFromOToolStripMenuItem).add_Click((EventHandler)checkAllSourcedFromOToolStripMenuItem_Click);
			((ToolStripItem)checkAllSourcedFrToolStripMenuItem).set_Name("checkAllSourcedFrToolStripMenuItem");
			((ToolStripItem)checkAllSourcedFrToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)checkAllSourcedFrToolStripMenuItem).set_Text("Check all sourced from 'X'");
			((ToolStripItem)checkAllSourcedFrToolStripMenuItem).add_Click((EventHandler)checkAllSourcedFrToolStripMenuItem_Click);
			((ToolStripItem)checkAllSourcedFromSToolStripMenuItem).set_Name("checkAllSourcedFromSToolStripMenuItem");
			((ToolStripItem)checkAllSourcedFromSToolStripMenuItem).set_Size(new Size(214, 22));
			((ToolStripItem)checkAllSourcedFromSToolStripMenuItem).set_Text("Check all sourced from 'S'");
			((ToolStripItem)checkAllSourcedFromSToolStripMenuItem).add_Click((EventHandler)checkAllSourcedFromSToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdPutEventsInEditor).set_Anchor((AnchorStyles)1);
			((Control)cmdPutEventsInEditor).set_Location(new Point(958, 2));
			((Control)cmdPutEventsInEditor).set_Name("cmdPutEventsInEditor");
			((Control)cmdPutEventsInEditor).set_Size(new Size(124, 20));
			((Control)cmdPutEventsInEditor).set_TabIndex(54);
			((Control)cmdPutEventsInEditor).set_Text("Put site in Editor");
			((ButtonBase)cmdPutEventsInEditor).set_UseVisualStyleBackColor(true);
			((Control)cmdPutEventsInEditor).add_Click((EventHandler)cmdPutEventsInEditor_Click);
			((Control)txtEventYear).set_Location(new Point(102, 34));
			((Control)txtEventYear).set_Name("txtEventYear");
			((Control)txtEventYear).set_Size(new Size(30, 20));
			((Control)txtEventYear).set_TabIndex(3);
			((Control)txtEventYear).add_Enter((EventHandler)txtEventYear_Enter);
			((Control)txtEventYear).add_MouseEnter((EventHandler)txtEventYear_MouseEnter);
			((Control)panelGeneralEdit).get_Controls().Add((Control)(object)label33);
			((Control)panelGeneralEdit).get_Controls().Add((Control)(object)txtGrazeNumber);
			((Control)panelGeneralEdit).get_Controls().Add((Control)(object)chkGraze);
			((Control)panelGeneralEdit).get_Controls().Add((Control)(object)cmdPutEventsInEditor);
			((Control)panelGeneralEdit).get_Controls().Add((Control)(object)cmdRetrieveOriginal);
			((Control)panelGeneralEdit).get_Controls().Add((Control)(object)groupBox5);
			((Control)panelGeneralEdit).get_Controls().Add((Control)(object)groupBox4);
			((Control)panelGeneralEdit).get_Controls().Add((Control)(object)groupBox3);
			((Control)panelGeneralEdit).get_Controls().Add((Control)(object)lblResidual);
			((Control)panelGeneralEdit).get_Controls().Add((Control)(object)label1);
			((Control)panelGeneralEdit).get_Controls().Add((Control)(object)pnlPre1750);
			((Control)panelGeneralEdit).set_Location(new Point(27, 184));
			((Control)panelGeneralEdit).set_Name("panelGeneralEdit");
			((Control)panelGeneralEdit).set_Size(new Size(1085, 244));
			((Control)panelGeneralEdit).set_TabIndex(56);
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label33).set_Location(new Point(669, 54));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(14, 13));
			((Control)label33).set_TabIndex(57);
			((Control)label33).set_Text("#");
			((Control)txtGrazeNumber).set_Location(new Point(684, 51));
			((Control)txtGrazeNumber).set_Name("txtGrazeNumber");
			((Control)txtGrazeNumber).set_Size(new Size(31, 20));
			((Control)txtGrazeNumber).set_TabIndex(56);
			((Control)chkGraze).set_AutoSize(true);
			((Control)chkGraze).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkGraze).set_Location(new Point(555, 50));
			((Control)chkGraze).set_Name("chkGraze");
			((Control)chkGraze).set_Size(new Size(107, 17));
			((Control)chkGraze).set_TabIndex(55);
			((Control)chkGraze).set_Text("Is graze event");
			((ButtonBase)chkGraze).set_UseVisualStyleBackColor(true);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)optSomaWGS84);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)optHT);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)optPlanets);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)txtEventMonth);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)label35);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)txtILOCSearchSite);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)chkSites);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)txtEventYear);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)optUnconsidered);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)optCorrected);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)optUnresolved);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)optAll);
			((Control)panelEventTypes).get_Controls().Add((Control)(object)optOther);
			((Control)panelEventTypes).set_Location(new Point(175, 3));
			((Control)panelEventTypes).set_Name("panelEventTypes");
			((Control)panelEventTypes).set_Size(new Size(331, 72));
			((Control)panelEventTypes).set_TabIndex(57);
			((Control)optSomaWGS84).set_AutoSize(true);
			((Control)optSomaWGS84).set_Location(new Point(238, 33));
			((Control)optSomaWGS84).set_Name("optSomaWGS84");
			((Control)optSomaWGS84).set_Size(new Size(93, 17));
			((Control)optSomaWGS84).set_TabIndex(12);
			optSomaWGS84.set_TabStop(true);
			((Control)optSomaWGS84).set_Text("Soma WGS84");
			((ButtonBase)optSomaWGS84).set_UseVisualStyleBackColor(true);
			((Control)optHT).set_AutoSize(true);
			((Control)optHT).set_Location(new Point(246, 3));
			((Control)optHT).set_Name("optHT");
			((Control)optHT).set_Size(new Size(71, 17));
			((Control)optHT).set_TabIndex(11);
			optHT.set_TabStop(true);
			((Control)optHT).set_Text("Hcat, sec");
			((ButtonBase)optHT).set_UseVisualStyleBackColor(true);
			((Control)optPlanets).set_AutoSize(true);
			((Control)optPlanets).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optPlanets).set_Location(new Point(180, 3));
			((Control)optPlanets).set_Name("optPlanets");
			((Control)optPlanets).set_Size(new Size(60, 17));
			((Control)optPlanets).set_TabIndex(9);
			((Control)optPlanets).set_Text("Planets");
			((ButtonBase)optPlanets).set_UseVisualStyleBackColor(true);
			((Control)txtEventMonth).set_Location(new Point(134, 33));
			((Control)txtEventMonth).set_Name("txtEventMonth");
			((Control)txtEventMonth).set_Size(new Size(18, 20));
			((Control)txtEventMonth).set_TabIndex(4);
			((Control)txtEventMonth).add_Enter((EventHandler)txtEventMonth_Enter);
			((Control)txtEventMonth).add_MouseEnter((EventHandler)txtEventMonth_MouseEnter);
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label35).set_Location(new Point(177, 37));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(61, 13));
			((Control)label35).set_TabIndex(5);
			((Control)label35).set_Text("Limit to Site");
			label35.set_TextAlign(ContentAlignment.BottomCenter);
			((Control)txtILOCSearchSite).set_Location(new Point(183, 51));
			((Control)txtILOCSearchSite).set_Name("txtILOCSearchSite");
			((Control)txtILOCSearchSite).set_Size(new Size(47, 20));
			((Control)txtILOCSearchSite).set_TabIndex(8);
			((Control)txtILOCSearchSite).add_Enter((EventHandler)txtILOCSearchSite_Enter);
			((Control)txtILOCSearchSite).add_MouseEnter((EventHandler)txtILOCSearchSite_MouseEnter);
			((Control)chkSites).set_AutoSize(true);
			((Control)chkSites).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSites).set_Location(new Point(114, 55));
			((Control)chkSites).set_Name("chkSites");
			((Control)chkSites).set_Size(new Size(71, 16));
			((Control)chkSites).set_TabIndex(7);
			((Control)chkSites).set_Text("D to G  only");
			((ButtonBase)chkSites).set_UseVisualStyleBackColor(true);
			((Control)optOther).set_AutoSize(true);
			((Control)optOther).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optOther).set_Location(new Point(181, 17));
			((Control)optOther).set_Name("optOther");
			((Control)optOther).set_Size(new Size(69, 17));
			((Control)optOther).set_TabIndex(10);
			((Control)optOther).set_Text("Unknown");
			((ButtonBase)optOther).set_UseVisualStyleBackColor(true);
			((Control)optGeneral).set_AutoSize(true);
			((Control)optGeneral).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optGeneral).set_Location(new Point(17, 34));
			((Control)optGeneral).set_Name("optGeneral");
			((Control)optGeneral).set_Size(new Size(106, 17));
			((Control)optGeneral).set_TabIndex(58);
			((Control)optGeneral).set_Text("General Editor");
			((ButtonBase)optGeneral).set_UseVisualStyleBackColor(true);
			optGeneral.add_CheckedChanged((EventHandler)optGeneral_CheckedChanged);
			((Control)optSiteEditor).set_AutoSize(true);
			((Control)optSiteEditor).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optSiteEditor).set_Location(new Point(17, 49));
			((Control)optSiteEditor).set_Name("optSiteEditor");
			((Control)optSiteEditor).set_Size(new Size(83, 17));
			((Control)optSiteEditor).set_TabIndex(59);
			((Control)optSiteEditor).set_Text("Site editor");
			((ButtonBase)optSiteEditor).set_UseVisualStyleBackColor(true);
			optSiteEditor.add_CheckedChanged((EventHandler)optSiteEditor_CheckedChanged);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)groupBox6);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)PanelILOCsiteReplace);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)cmdRetrieveRGOsite);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)lblCurrentCoords);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)label27);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)label26);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)label25);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)lblNewCoords);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)lblOldCoords);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)cmdGoogleEarthSites);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)cmdUpdateSites);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)chkCorrectSite);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)chkCorrectNames);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)chkCoords);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)label24);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)label23);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)label22);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)label21);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)label20);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)label19);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)label18);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)lblSiteLeft);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)lblObsLeft);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtSite2);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtSite1);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtObs1);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtObs2);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)cmbAltDatum2);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)cmbAltDatum1);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtAlt1);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtAlt2);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)cmbDatum1);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)cmbDatum2);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLatS1);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLatM1);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLatD1);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLatS2);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLatM2);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLatD2);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLonS1);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLonM1);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLonD1);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLonS2);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLonM2);
			((Control)panelSiteEdit).get_Controls().Add((Control)(object)txtLonD2);
			((Control)panelSiteEdit).set_Location(new Point(5, 304));
			((Control)panelSiteEdit).set_Name("panelSiteEdit");
			((Control)panelSiteEdit).set_Size(new Size(1100, 237));
			((Control)panelSiteEdit).set_TabIndex(60);
			((Control)panelSiteEdit).set_Visible(false);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtListSiteName);
			((Control)groupBox6).get_Controls().Add((Control)(object)txtListObserver);
			((Control)groupBox6).get_Controls().Add((Control)(object)optListSite);
			((Control)groupBox6).get_Controls().Add((Control)(object)optListObserver);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmdGetSiteEvents);
			((Control)groupBox6).get_Controls().Add((Control)(object)panelILOC);
			((Control)groupBox6).get_Controls().Add((Control)(object)panelRGO);
			((Control)groupBox6).get_Controls().Add((Control)(object)optILOC);
			((Control)groupBox6).get_Controls().Add((Control)(object)optRGO);
			((Control)groupBox6).set_Location(new Point(5, 6));
			((Control)groupBox6).set_Name("groupBox6");
			((Control)groupBox6).set_Size(new Size(195, 155));
			((Control)groupBox6).set_TabIndex(5);
			groupBox6.set_TabStop(false);
			((Control)groupBox6).set_Text("List events from site");
			((Control)txtListSiteName).set_Location(new Point(91, 73));
			((Control)txtListSiteName).set_Name("txtListSiteName");
			((Control)txtListSiteName).set_Size(new Size(95, 20));
			((Control)txtListSiteName).set_TabIndex(8);
			((Control)txtListObserver).set_Location(new Point(92, 55));
			((Control)txtListObserver).set_Name("txtListObserver");
			((Control)txtListObserver).set_Size(new Size(95, 20));
			((Control)txtListObserver).set_TabIndex(7);
			((Control)optListSite).set_AutoSize(true);
			((Control)optListSite).set_Location(new Point(17, 75));
			((Control)optListSite).set_Name("optListSite");
			((Control)optListSite).set_Size(new Size(72, 17));
			((Control)optListSite).set_TabIndex(6);
			optListSite.set_TabStop(true);
			((Control)optListSite).set_Text("Site name");
			((ButtonBase)optListSite).set_UseVisualStyleBackColor(true);
			optListSite.add_CheckedChanged((EventHandler)optListSite_CheckedChanged);
			((Control)optListObserver).set_AutoSize(true);
			((Control)optListObserver).set_Location(new Point(17, 57));
			((Control)optListObserver).set_Name("optListObserver");
			((Control)optListObserver).set_Size(new Size(68, 17));
			((Control)optListObserver).set_TabIndex(5);
			optListObserver.set_TabStop(true);
			((Control)optListObserver).set_Text("Observer");
			((ButtonBase)optListObserver).set_UseVisualStyleBackColor(true);
			optListObserver.add_CheckedChanged((EventHandler)optListObserver_CheckedChanged);
			((Control)cmdGetSiteEvents).set_Location(new Point(17, 109));
			((Control)cmdGetSiteEvents).set_Name("cmdGetSiteEvents");
			((Control)cmdGetSiteEvents).set_Size(new Size(108, 36));
			((Control)cmdGetSiteEvents).set_TabIndex(4);
			((Control)cmdGetSiteEvents).set_Text("List events from site");
			((ButtonBase)cmdGetSiteEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdGetSiteEvents).add_Click((EventHandler)cmdGetSiteEvents_Click);
			((Control)panelILOC).get_Controls().Add((Control)(object)txtILOC3);
			((Control)panelILOC).get_Controls().Add((Control)(object)txtILOC2);
			((Control)panelILOC).get_Controls().Add((Control)(object)txtILOC1);
			((Control)panelILOC).set_Location(new Point(91, 30));
			((Control)panelILOC).set_Name("panelILOC");
			((Control)panelILOC).set_Size(new Size(102, 35));
			((Control)panelILOC).set_TabIndex(3);
			((Control)txtILOC3).set_Location(new Point(72, 7));
			((Control)txtILOC3).set_Name("txtILOC3");
			((Control)txtILOC3).set_Size(new Size(25, 20));
			((Control)txtILOC3).set_TabIndex(4);
			((Control)txtILOC3).add_Enter((EventHandler)txtILOC3_Enter);
			((Control)txtILOC2).set_Location(new Point(47, 7));
			((Control)txtILOC2).set_Name("txtILOC2");
			((Control)txtILOC2).set_Size(new Size(25, 20));
			((Control)txtILOC2).set_TabIndex(3);
			((Control)txtILOC2).add_Enter((EventHandler)txtILOC2_Enter);
			((Control)txtILOC1).set_Location(new Point(3, 7));
			((Control)txtILOC1).set_Name("txtILOC1");
			((Control)txtILOC1).set_Size(new Size(44, 20));
			((Control)txtILOC1).set_TabIndex(2);
			((Control)txtILOC1).add_Enter((EventHandler)txtILOC1_Enter);
			((Control)panelRGO).get_Controls().Add((Control)(object)txtRGO2);
			((Control)panelRGO).get_Controls().Add((Control)(object)txtRGO1);
			((Control)panelRGO).set_Location(new Point(89, 12));
			((Control)panelRGO).set_Name("panelRGO");
			((Control)panelRGO).set_Size(new Size(71, 35));
			((Control)panelRGO).set_TabIndex(2);
			((Control)panelRGO).set_Visible(false);
			((Control)txtRGO2).set_Location(new Point(40, 8));
			((Control)txtRGO2).set_Name("txtRGO2");
			((Control)txtRGO2).set_Size(new Size(25, 20));
			((Control)txtRGO2).set_TabIndex(1);
			((Control)txtRGO1).set_Location(new Point(6, 8));
			((Control)txtRGO1).set_Name("txtRGO1");
			((Control)txtRGO1).set_Size(new Size(34, 20));
			((Control)txtRGO1).set_TabIndex(0);
			((Control)optILOC).set_AutoSize(true);
			optILOC.set_Checked(true);
			((Control)optILOC).set_Location(new Point(17, 39));
			((Control)optILOC).set_Name("optILOC");
			((Control)optILOC).set_Size(new Size(68, 17));
			((Control)optILOC).set_TabIndex(1);
			optILOC.set_TabStop(true);
			((Control)optILOC).set_Text("ILOC site");
			((ButtonBase)optILOC).set_UseVisualStyleBackColor(true);
			optILOC.add_CheckedChanged((EventHandler)optILOC_CheckedChanged);
			((Control)optRGO).set_AutoSize(true);
			((Control)optRGO).set_Location(new Point(17, 21));
			((Control)optRGO).set_Name("optRGO");
			((Control)optRGO).set_Size(new Size(68, 17));
			((Control)optRGO).set_TabIndex(0);
			((Control)optRGO).set_Text("RGO site");
			((ButtonBase)optRGO).set_UseVisualStyleBackColor(true);
			optRGO.add_CheckedChanged((EventHandler)optRGO_CheckedChanged);
			((Control)PanelILOCsiteReplace).get_Controls().Add((Control)(object)txtTelescope);
			((Control)PanelILOCsiteReplace).get_Controls().Add((Control)(object)cmdRetrieveNewILOCsite);
			((Control)PanelILOCsiteReplace).get_Controls().Add((Control)(object)chkChangeTelescope);
			((Control)PanelILOCsiteReplace).get_Controls().Add((Control)(object)txtILOC3replace);
			((Control)PanelILOCsiteReplace).get_Controls().Add((Control)(object)chkCorrectILOCsites);
			((Control)PanelILOCsiteReplace).get_Controls().Add((Control)(object)txtILOC2replace);
			((Control)PanelILOCsiteReplace).get_Controls().Add((Control)(object)txtILOC1replace);
			((Control)PanelILOCsiteReplace).set_Location(new Point(221, 199));
			((Control)PanelILOCsiteReplace).set_Name("PanelILOCsiteReplace");
			((Control)PanelILOCsiteReplace).set_Size(new Size(678, 35));
			((Control)PanelILOCsiteReplace).set_TabIndex(51);
			((Control)txtTelescope).set_Location(new Point(121, 10));
			((Control)txtTelescope).set_Name("txtTelescope");
			((Control)txtTelescope).set_Size(new Size(68, 20));
			((Control)txtTelescope).set_TabIndex(53);
			((Control)cmdRetrieveNewILOCsite).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRetrieveNewILOCsite).set_Location(new Point(534, 10));
			((Control)cmdRetrieveNewILOCsite).set_Name("cmdRetrieveNewILOCsite");
			((Control)cmdRetrieveNewILOCsite).set_Size(new Size(136, 20));
			((Control)cmdRetrieveNewILOCsite).set_TabIndex(51);
			((Control)cmdRetrieveNewILOCsite).set_Text("retrieve site and names");
			((ButtonBase)cmdRetrieveNewILOCsite).set_UseVisualStyleBackColor(true);
			((Control)cmdRetrieveNewILOCsite).add_Click((EventHandler)cmdRetrieveNewILOCsite_Click);
			((Control)chkChangeTelescope).set_AutoSize(true);
			((Control)chkChangeTelescope).set_Location(new Point(3, 12));
			((Control)chkChangeTelescope).set_Name("chkChangeTelescope");
			((Control)chkChangeTelescope).set_Size(new Size(109, 17));
			((Control)chkChangeTelescope).set_TabIndex(52);
			((Control)chkChangeTelescope).set_Text("Correct telescope");
			((ButtonBase)chkChangeTelescope).set_UseVisualStyleBackColor(true);
			((Control)txtILOC3replace).set_Location(new Point(489, 10));
			((Control)txtILOC3replace).set_Name("txtILOC3replace");
			((Control)txtILOC3replace).set_Size(new Size(25, 20));
			((Control)txtILOC3replace).set_TabIndex(4);
			((Control)txtILOC3replace).add_Enter((EventHandler)txtILOC3replace_Enter);
			((Control)chkCorrectILOCsites).set_AutoSize(true);
			((Control)chkCorrectILOCsites).set_Location(new Point(261, 12));
			((Control)chkCorrectILOCsites).set_Name("chkCorrectILOCsites");
			((Control)chkCorrectILOCsites).set_Size(new Size(151, 17));
			((Control)chkCorrectILOCsites).set_TabIndex(50);
			((Control)chkCorrectILOCsites).set_Text("Change ILOC site identifier");
			((ButtonBase)chkCorrectILOCsites).set_UseVisualStyleBackColor(true);
			((Control)txtILOC2replace).set_Location(new Point(464, 10));
			((Control)txtILOC2replace).set_Name("txtILOC2replace");
			((Control)txtILOC2replace).set_Size(new Size(25, 20));
			((Control)txtILOC2replace).set_TabIndex(3);
			((Control)txtILOC2replace).add_Enter((EventHandler)txtILOC2replace_Enter);
			((Control)txtILOC1replace).set_Location(new Point(420, 10));
			((Control)txtILOC1replace).set_Name("txtILOC1replace");
			((Control)txtILOC1replace).set_Size(new Size(44, 20));
			((Control)txtILOC1replace).set_TabIndex(2);
			((Control)txtILOC1replace).add_Enter((EventHandler)txtILOC1replace_Enter);
			((Control)cmdRetrieveRGOsite).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRetrieveRGOsite).set_Location(new Point(235, 21));
			((Control)cmdRetrieveRGOsite).set_Name("cmdRetrieveRGOsite");
			((Control)cmdRetrieveRGOsite).set_Size(new Size(72, 19));
			((Control)cmdRetrieveRGOsite).set_TabIndex(49);
			((Control)cmdRetrieveRGOsite).set_Text("retrieve site");
			((ButtonBase)cmdRetrieveRGOsite).set_UseVisualStyleBackColor(true);
			((Control)cmdRetrieveRGOsite).add_Click((EventHandler)cmdRetrieveRGOsite_Click);
			((Control)lblCurrentCoords).set_AutoSize(true);
			((Control)lblCurrentCoords).set_Enabled(false);
			((Control)lblCurrentCoords).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCurrentCoords).set_Location(new Point(682, 93));
			((Control)lblCurrentCoords).set_Name("lblCurrentCoords");
			((Control)lblCurrentCoords).set_Size(new Size(56, 14));
			((Control)lblCurrentCoords).set_TabIndex(48);
			((Control)lblCurrentCoords).set_Text("Current");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(579, 94));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(98, 13));
			((Control)label27).set_TabIndex(47);
			((Control)label27).set_Text("Current coordinates");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(579, 108));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(94, 13));
			((Control)label26).set_TabIndex(46);
			((Control)label26).set_Text("Edited coordinates");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(579, 80));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(99, 13));
			((Control)label25).set_TabIndex(45);
			((Control)label25).set_Text("Original coordinates");
			((Control)lblNewCoords).set_AutoSize(true);
			((Control)lblNewCoords).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblNewCoords).set_Location(new Point(682, 107));
			((Control)lblNewCoords).set_Name("lblNewCoords");
			((Control)lblNewCoords).set_Size(new Size(28, 14));
			((Control)lblNewCoords).set_TabIndex(44);
			((Control)lblNewCoords).set_Text("New");
			((Control)lblOldCoords).set_AutoSize(true);
			((Control)lblOldCoords).set_Enabled(false);
			((Control)lblOldCoords).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblOldCoords).set_Location(new Point(682, 79));
			((Control)lblOldCoords).set_Name("lblOldCoords");
			((Control)lblOldCoords).set_Size(new Size(28, 14));
			((Control)lblOldCoords).set_TabIndex(43);
			((Control)lblOldCoords).set_Text("Old");
			((Control)cmdGoogleEarthSites).set_Location(new Point(919, 82));
			((Control)cmdGoogleEarthSites).set_Name("cmdGoogleEarthSites");
			((Control)cmdGoogleEarthSites).set_Size(new Size(105, 37));
			((Control)cmdGoogleEarthSites).set_TabIndex(42);
			((Control)cmdGoogleEarthSites).set_Text("Display old && new in GoogleEarth");
			((ButtonBase)cmdGoogleEarthSites).set_UseVisualStyleBackColor(true);
			((Control)cmdGoogleEarthSites).add_Click((EventHandler)cmdGoogleEarthSites_Click);
			((Control)cmdUpdateSites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdUpdateSites).set_Location(new Point(27, 178));
			((Control)cmdUpdateSites).set_Name("cmdUpdateSites");
			((Control)cmdUpdateSites).set_Size(new Size(129, 40));
			((Control)cmdUpdateSites).set_TabIndex(41);
			((Control)cmdUpdateSites).set_Text("Update site details");
			((ButtonBase)cmdUpdateSites).set_UseVisualStyleBackColor(true);
			((Control)cmdUpdateSites).add_Click((EventHandler)cmdUpdateSites_Click);
			((Control)chkCorrectSite).set_AutoSize(true);
			((Control)chkCorrectSite).set_Location(new Point(223, 161));
			((Control)chkCorrectSite).set_Name("chkCorrectSite");
			((Control)chkCorrectSite).set_Size(new Size(110, 17));
			((Control)chkCorrectSite).set_TabIndex(40);
			((Control)chkCorrectSite).set_Text("Correct Site name");
			((ButtonBase)chkCorrectSite).set_UseVisualStyleBackColor(true);
			((Control)chkCorrectNames).set_AutoSize(true);
			((Control)chkCorrectNames).set_Location(new Point(223, 108));
			((Control)chkCorrectNames).set_Name("chkCorrectNames");
			((Control)chkCorrectNames).set_Size(new Size(106, 17));
			((Control)chkCorrectNames).set_TabIndex(39);
			((Control)chkCorrectNames).set_Text("Correct Observer");
			((ButtonBase)chkCorrectNames).set_UseVisualStyleBackColor(true);
			((Control)chkCoords).set_AutoSize(true);
			((Control)chkCoords).set_Location(new Point(223, 52));
			((Control)chkCoords).set_Name("chkCoords");
			((Control)chkCoords).set_Size(new Size(118, 17));
			((Control)chkCoords).set_TabIndex(38);
			((Control)chkCoords).set_Text("Correct coordinates");
			((ButtonBase)chkCoords).set_UseVisualStyleBackColor(true);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(353, 76));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(50, 13));
			((Control)label24).set_TabIndex(37);
			((Control)label24).set_Text("Observer");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(353, 129));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(64, 13));
			((Control)label23).set_TabIndex(36);
			((Control)label23).set_Text("Site location");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(353, 15));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(54, 13));
			((Control)label22).set_TabIndex(35);
			((Control)label22).set_Text("Longitude");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(488, 15));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(45, 13));
			((Control)label21).set_TabIndex(34);
			((Control)label21).set_Text("Latitude");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(579, 9));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(85, 13));
			((Control)label20).set_TabIndex(33);
			((Control)label20).set_Text("Horizontal datum");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(870, 15));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(42, 13));
			((Control)label19).set_TabIndex(32);
			((Control)label19).set_Text("Altitude");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(941, 9));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(73, 13));
			((Control)label18).set_TabIndex(31);
			((Control)label18).set_Text("Vertical datum");
			((Control)lblSiteLeft).set_AutoSize(true);
			((Control)lblSiteLeft).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblSiteLeft).set_Location(new Point(845, 159));
			((Control)lblSiteLeft).set_Name("lblSiteLeft");
			((Control)lblSiteLeft).set_Size(new Size(19, 13));
			((Control)lblSiteLeft).set_TabIndex(30);
			((Control)lblSiteLeft).set_Text("50");
			((Control)lblObsLeft).set_AutoSize(true);
			((Control)lblObsLeft).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblObsLeft).set_Location(new Point(511, 107));
			((Control)lblObsLeft).set_Name("lblObsLeft");
			((Control)lblObsLeft).set_Size(new Size(19, 13));
			((Control)lblObsLeft).set_TabIndex(29);
			((Control)lblObsLeft).set_Text("25");
			((Control)txtSite2).set_Location(new Point(341, 158));
			((Control)txtSite2).set_Name("txtSite2");
			((Control)txtSite2).set_Size(new Size(498, 20));
			((Control)txtSite2).set_TabIndex(28);
			((Control)txtSite2).add_TextChanged((EventHandler)txtSite2_TextChanged);
			((TextBoxBase)txtSite1).set_BorderStyle((BorderStyle)0);
			((Control)txtSite1).set_Enabled(false);
			((Control)txtSite1).set_Location(new Point(344, 143));
			((Control)txtSite1).set_Name("txtSite1");
			((Control)txtSite1).set_Size(new Size(498, 13));
			((Control)txtSite1).set_TabIndex(27);
			((TextBoxBase)txtObs1).set_BorderStyle((BorderStyle)0);
			((Control)txtObs1).set_Enabled(false);
			((Control)txtObs1).set_Location(new Point(344, 90));
			((Control)txtObs1).set_Name("txtObs1");
			((Control)txtObs1).set_Size(new Size(166, 13));
			((Control)txtObs1).set_TabIndex(26);
			((Control)txtObs2).set_Location(new Point(341, 105));
			((Control)txtObs2).set_Name("txtObs2");
			((Control)txtObs2).set_Size(new Size(166, 20));
			((Control)txtObs2).set_TabIndex(25);
			((Control)txtObs2).add_TextChanged((EventHandler)txtObs2_TextChanged);
			cmbAltDatum2.set_FlatStyle((FlatStyle)0);
			((ListControl)cmbAltDatum2).set_FormattingEnabled(true);
			cmbAltDatum2.get_Items().AddRange(new object[2] { "Mean Sea Level", "Spheroid" });
			((Control)cmbAltDatum2).set_Location(new Point(935, 49));
			((Control)cmbAltDatum2).set_Name("cmbAltDatum2");
			((Control)cmbAltDatum2).set_Size(new Size(107, 21));
			((Control)cmbAltDatum2).set_TabIndex(24);
			cmbAltDatum2.add_SelectedIndexChanged((EventHandler)cmbAltDatum2_SelectedIndexChanged);
			((Control)cmbAltDatum1).set_Enabled(false);
			cmbAltDatum1.set_FlatStyle((FlatStyle)0);
			((ListControl)cmbAltDatum1).set_FormattingEnabled(true);
			cmbAltDatum1.get_Items().AddRange(new object[2] { "Mean Sea Level", "Spheroid" });
			((Control)cmbAltDatum1).set_Location(new Point(935, 25));
			((Control)cmbAltDatum1).set_Name("cmbAltDatum1");
			((Control)cmbAltDatum1).set_Size(new Size(107, 21));
			((Control)cmbAltDatum1).set_TabIndex(23);
			((TextBoxBase)txtAlt1).set_BorderStyle((BorderStyle)0);
			((Control)txtAlt1).set_Enabled(false);
			((Control)txtAlt1).set_Location(new Point(868, 33));
			((Control)txtAlt1).set_Name("txtAlt1");
			((Control)txtAlt1).set_Size(new Size(44, 13));
			((Control)txtAlt1).set_TabIndex(22);
			((Control)txtAlt2).set_Location(new Point(868, 49));
			((Control)txtAlt2).set_Name("txtAlt2");
			((Control)txtAlt2).set_Size(new Size(44, 20));
			((Control)txtAlt2).set_TabIndex(21);
			((Control)txtAlt2).add_TextChanged((EventHandler)txtAlt2_TextChanged);
			((Control)cmbDatum1).set_Enabled(false);
			cmbDatum1.set_FlatStyle((FlatStyle)0);
			((ListControl)cmbDatum1).set_FormattingEnabled(true);
			cmbDatum1.get_Items().AddRange(new object[79]
			{
				" 0  unknown (WGS84 used)", " 1  RGO code 1", " 2  RGO code 2", " 3  RGO code 3", " 4  RGO code 4", " 5  RGO code 5", "10  GoogleEarth (WGS84)", "12  Christmas Island Astro 1967", "13  Chua Astro (Brazil Geodetic)", "14  Corrego Alegre (Brazil)",
				"15  Easter Island Astro 1967", "16  European 1950  ", "17  Graciosa Island (Azores)", "18  Gizo, Provisional DOS  ", "19  Guam", "20  Heard Astro 1969", "21  Iben Astro, Navy 1947 (Truk) ", "22  Indian", "23  Isla Socorro Astro", "24  Johnston Island 1961 ",
				"25  Kusaie Astro 1962,1965", "26  Luzon 1911 (Philippines) ", "27  Midway Astro 1961", "28  New Zealand 1949 ", "29  North American 1927  ", "30 *Cape Canaveral ", "31 *White Sands", "32  Old Bavarian ", "33  Old Hawaiian ", "34  Ordnance Survey of Great Britain 1936 ",
				"35  Pico de las Nieves (Canaries) ", "36  Pitcairn Island Astro ", "37  Potsdam", "38  Provisional South American 1956", "39  Provisional South Chile 1963  ", "40  Pulkovo 1942  ", "41  South American 1969", "42  Southeast Island (Mahe)", "43  South Georgia Astro", "44  Swallow Islands (Solomons)",
				"45  Tananarive", "46  Tokyo ", "47  Tristan Astro 1968", "48  Viti Levu 1916 (Fiji) ", "49  Wake Island Astro 1952", "50  Yof Astro 1967 (Dakar)", "51  Palmer Astro 1969 (Antarctica)", "52  Efate (New Hebrides)", "53  Marcus Island 1965", "54  Canton Astro 1966  ",
				"56  Yap Island", "58  Kourou (French Guiana)", "59  Ordnance Survey of Great Britain 1970 ", "60  Qornoq (Greenland)", "61  Adindan (Ethiopia) ", "62  American Samoa 1962", "63  Arc-Cape (South Africa)", "64  Argentine  ", "65  Ascension Island 1958  ", "66  Australian Geodetic",
				"67  Bermuda 1957", "68  Berne 1898 ", "69  Betio Island 1966  ", "70  Camp Area Astro 1961-62 USGS", "71  Batavia (Java)", "72  Palestine (Israel,Jordan)", "73  Hermannskogel (Austria,Czech.,Yugoslavia)", "74  Kandawala (Ceylon)", "80  ETRS89 (European Terrestial Reference System)", "81  Amersfoort 1885 (Netherlands)",
				"82  NAD83/NAD1983 (=WGS84)", "84  WGS84", "85  JGD2000 (=WGS84)", "86  GDA94 (=WGS84)", "87  NZGD2000 (=WGS84)", "88  NGRF2000 (=WGS84)", "89  KDG2000 (=WGS84)", "90  Hartebeesthoek94 (=WGS84)", "91  TWD94 (=WGS84)"
			});
			((Control)cmbDatum1).set_Location(new Point(573, 25));
			((Control)cmbDatum1).set_Name("cmbDatum1");
			((Control)cmbDatum1).set_Size(new Size(266, 21));
			((Control)cmbDatum1).set_TabIndex(20);
			cmbDatum2.set_FlatStyle((FlatStyle)0);
			((ListControl)cmbDatum2).set_FormattingEnabled(true);
			cmbDatum2.get_Items().AddRange(new object[79]
			{
				" 0  unknown (WGS84 used)", " 1  RGO code 1", " 2  RGO code 2", " 3  RGO code 3", " 4  RGO code 4", " 5  RGO code 5", "10  GoogleEarth (WGS84)", "12  Christmas Island Astro 1967", "13  Chua Astro (Brazil Geodetic)", "14  Corrego Alegre (Brazil)",
				"15  Easter Island Astro 1967", "16  European 1950  ", "17  Graciosa Island (Azores)", "18  Gizo, Provisional DOS  ", "19  Guam", "20  Heard Astro 1969", "21  Iben Astro, Navy 1947 (Truk) ", "22  Indian", "23  Isla Socorro Astro", "24  Johnston Island 1961 ",
				"25  Kusaie Astro 1962,1965", "26  Luzon 1911 (Philippines) ", "27  Midway Astro 1961", "28  New Zealand 1949 ", "29  North American 1927  ", "30 *Cape Canaveral ", "31 *White Sands", "32  Old Bavarian ", "33  Old Hawaiian ", "34  Ordnance Survey of Great Britain 1936 ",
				"35  Pico de las Nieves (Canaries) ", "36  Pitcairn Island Astro ", "37  Potsdam", "38  Provisional South American 1956", "39  Provisional South Chile 1963  ", "40  Pulkovo 1942  ", "41  South American 1969", "42  Southeast Island (Mahe)", "43  South Georgia Astro", "44  Swallow Islands (Solomons)",
				"45  Tananarive", "46  Tokyo ", "47  Tristan Astro 1968", "48  Viti Levu 1916 (Fiji) ", "49  Wake Island Astro 1952", "50  Yof Astro 1967 (Dakar)", "51  Palmer Astro 1969 (Antarctica)", "52  Efate (New Hebrides)", "53  Marcus Island 1965", "54  Canton Astro 1966  ",
				"56  Yap Island", "58  Kourou (French Guiana)", "59  Ordnance Survey of Great Britain 1970 ", "60  Qornoq (Greenland)", "61  Adindan (Ethiopia) ", "62  American Samoa 1962", "63  Arc-Cape (South Africa)", "64  Argentine  ", "65  Ascension Island 1958  ", "66  Australian Geodetic",
				"67  Bermuda 1957", "68  Berne 1898 ", "69  Betio Island 1966  ", "70  Camp Area Astro 1961-62 USGS", "71  Batavia (Java)", "72  Palestine (Israel,Jordan)", "73  Hermannskogel (Austria,Czech.,Yugoslavia)", "74  Kandawala (Ceylon)", "80  ETRS89 (European Terrestial Reference System)", "81  Amersfoort 1885 (Netherlands)",
				"82  NAD83/NAD1983 (=WGS84)", "84  WGS84", "85  JGD2000 (=WGS84)", "86  GDA94 (=WGS84)", "87  NZGD2000 (=WGS84)", "88  NGRF2000 (=WGS84)", "89  KDG2000 (=WGS84)", "90  Hartebeesthoek94 (=WGS84)", "91  TWD94 (=WGS84)"
			});
			((Control)cmbDatum2).set_Location(new Point(573, 49));
			((Control)cmbDatum2).set_Name("cmbDatum2");
			((Control)cmbDatum2).set_Size(new Size(266, 21));
			((Control)cmbDatum2).set_TabIndex(19);
			cmbDatum2.add_SelectedIndexChanged((EventHandler)cmbDatum2_SelectedIndexChanged);
			((TextBoxBase)txtLatS1).set_BorderStyle((BorderStyle)0);
			((Control)txtLatS1).set_Enabled(false);
			((Control)txtLatS1).set_Location(new Point(534, 33));
			((Control)txtLatS1).set_Name("txtLatS1");
			((Control)txtLatS1).set_Size(new Size(35, 13));
			((Control)txtLatS1).set_TabIndex(17);
			((TextBoxBase)txtLatM1).set_BorderStyle((BorderStyle)0);
			((Control)txtLatM1).set_Enabled(false);
			((Control)txtLatM1).set_Location(new Point(509, 33));
			((Control)txtLatM1).set_Name("txtLatM1");
			((Control)txtLatM1).set_Size(new Size(25, 13));
			((Control)txtLatM1).set_TabIndex(16);
			((TextBoxBase)txtLatD1).set_BorderStyle((BorderStyle)0);
			((Control)txtLatD1).set_Enabled(false);
			((Control)txtLatD1).set_Location(new Point(465, 33));
			((Control)txtLatD1).set_Name("txtLatD1");
			((Control)txtLatD1).set_Size(new Size(44, 13));
			((Control)txtLatD1).set_TabIndex(15);
			((Control)txtLatS2).set_Location(new Point(532, 49));
			((Control)txtLatS2).set_Name("txtLatS2");
			((Control)txtLatS2).set_Size(new Size(36, 20));
			((Control)txtLatS2).set_TabIndex(14);
			((Control)txtLatS2).add_TextChanged((EventHandler)txtLatS2_TextChanged);
			((Control)txtLatM2).set_Location(new Point(507, 49));
			((Control)txtLatM2).set_Name("txtLatM2");
			((Control)txtLatM2).set_Size(new Size(25, 20));
			((Control)txtLatM2).set_TabIndex(13);
			((Control)txtLatM2).add_TextChanged((EventHandler)txtLatM2_TextChanged);
			((Control)txtLatD2).set_Location(new Point(463, 49));
			((Control)txtLatD2).set_Name("txtLatD2");
			((Control)txtLatD2).set_Size(new Size(44, 20));
			((Control)txtLatD2).set_TabIndex(12);
			((Control)txtLatD2).add_TextChanged((EventHandler)txtLatD2_TextChanged);
			((TextBoxBase)txtLonS1).set_BorderStyle((BorderStyle)0);
			((Control)txtLonS1).set_Enabled(false);
			((Control)txtLonS1).set_Location(new Point(412, 33));
			((Control)txtLonS1).set_Name("txtLonS1");
			((Control)txtLonS1).set_Size(new Size(35, 13));
			((Control)txtLonS1).set_TabIndex(11);
			((TextBoxBase)txtLonM1).set_BorderStyle((BorderStyle)0);
			((Control)txtLonM1).set_Enabled(false);
			((Control)txtLonM1).set_Location(new Point(387, 33));
			((Control)txtLonM1).set_Name("txtLonM1");
			((Control)txtLonM1).set_Size(new Size(25, 13));
			((Control)txtLonM1).set_TabIndex(10);
			((TextBoxBase)txtLonD1).set_BorderStyle((BorderStyle)0);
			((Control)txtLonD1).set_Enabled(false);
			((Control)txtLonD1).set_Location(new Point(343, 33));
			((Control)txtLonD1).set_Name("txtLonD1");
			((Control)txtLonD1).set_Size(new Size(44, 13));
			((Control)txtLonD1).set_TabIndex(9);
			((Control)txtLonS2).set_Location(new Point(410, 49));
			((Control)txtLonS2).set_Name("txtLonS2");
			((Control)txtLonS2).set_Size(new Size(36, 20));
			((Control)txtLonS2).set_TabIndex(8);
			((Control)txtLonS2).add_TextChanged((EventHandler)txtLonS2_TextChanged);
			((Control)txtLonM2).set_Location(new Point(385, 49));
			((Control)txtLonM2).set_Name("txtLonM2");
			((Control)txtLonM2).set_Size(new Size(25, 20));
			((Control)txtLonM2).set_TabIndex(7);
			((Control)txtLonM2).add_TextChanged((EventHandler)txtLonM2_TextChanged);
			((Control)txtLonD2).set_Location(new Point(341, 49));
			((Control)txtLonD2).set_Name("txtLonD2");
			((Control)txtLonD2).set_Size(new Size(44, 20));
			((Control)txtLonD2).set_TabIndex(6);
			((Control)txtLonD2).add_TextChanged((EventHandler)txtLonD2_TextChanged);
			((Control)panelHeader).get_Controls().Add((Control)(object)cmdListDuplicates);
			((Control)panelHeader).get_Controls().Add((Control)(object)chkExcludeInvalid);
			((Control)panelHeader).get_Controls().Add((Control)(object)chkExcludeStartEnd);
			((Control)panelHeader).get_Controls().Add((Control)(object)chkbySiteID);
			((Control)panelHeader).get_Controls().Add((Control)(object)panelEventTypes);
			((Control)panelHeader).get_Controls().Add((Control)(object)cmdMergeArchiveFiles);
			((Control)panelHeader).get_Controls().Add((Control)(object)cmdSortArchive);
			((Control)panelHeader).get_Controls().Add((Control)(object)cmdListLargeResiduals);
			((Control)panelHeader).get_Controls().Add((Control)(object)cmdAutoCorrectArchive);
			((Control)panelHeader).get_Controls().Add((Control)(object)cmdReduceArchiveFile);
			((Control)panelHeader).set_Location(new Point(192, 39));
			((Control)panelHeader).set_Name("panelHeader");
			((Control)panelHeader).set_Size(new Size(936, 77));
			((Control)panelHeader).set_TabIndex(61);
			((Control)cmdListDuplicates).set_Location(new Point(543, 52));
			((Control)cmdListDuplicates).set_Name("cmdListDuplicates");
			((Control)cmdListDuplicates).set_Size(new Size(102, 21));
			((Control)cmdListDuplicates).set_TabIndex(61);
			((Control)cmdListDuplicates).set_Text("List Duplicates");
			((ButtonBase)cmdListDuplicates).set_UseVisualStyleBackColor(true);
			((Control)cmdListDuplicates).add_Click((EventHandler)cmdListDuplicates_Click);
			((Control)chkExcludeInvalid).set_AutoSize(true);
			chkExcludeInvalid.set_Checked(Settings.Default.ArchiveGrazesExcludeInvalid);
			chkExcludeInvalid.set_CheckState((CheckState)1);
			((Control)chkExcludeInvalid).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ArchiveGrazesExcludeInvalid", true, (DataSourceUpdateMode)1));
			((Control)chkExcludeInvalid).set_Location(new Point(804, 13));
			((Control)chkExcludeInvalid).set_Name("chkExcludeInvalid");
			((Control)chkExcludeInvalid).set_Size(new Size(97, 17));
			((Control)chkExcludeInvalid).set_TabIndex(60);
			((Control)chkExcludeInvalid).set_Text("Exclude invalid");
			((ButtonBase)chkExcludeInvalid).set_UseVisualStyleBackColor(true);
			((Control)chkExcludeStartEnd).set_AutoSize(true);
			chkExcludeStartEnd.set_Checked(Settings.Default.ArchiveGrazesExcludeStartEnd);
			chkExcludeStartEnd.set_CheckState((CheckState)1);
			((Control)chkExcludeStartEnd).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ArchiveGrazesExcludeStartEnd", true, (DataSourceUpdateMode)1));
			((Control)chkExcludeStartEnd).set_Location(new Point(804, 30));
			((Control)chkExcludeStartEnd).set_Name("chkExcludeStartEnd");
			((Control)chkExcludeStartEnd).set_Size(new Size(113, 17));
			((Control)chkExcludeStartEnd).set_TabIndex(59);
			((Control)chkExcludeStartEnd).set_Text("Exclude Start/End");
			((ButtonBase)chkExcludeStartEnd).set_UseVisualStyleBackColor(true);
			((Control)chkbySiteID).set_AutoSize(true);
			((Control)chkbySiteID).set_Location(new Point(804, 52));
			((Control)chkbySiteID).set_Name("chkbySiteID");
			((Control)chkbySiteID).set_Size(new Size(105, 17));
			((Control)chkbySiteID).set_TabIndex(58);
			((Control)chkbySiteID).set_Text("Output by site ID");
			((ButtonBase)chkbySiteID).set_UseVisualStyleBackColor(true);
			((Control)cmdProcessfromCoordinators).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdProcessfromCoordinators).set_Location(new Point(248, 118));
			((Control)cmdProcessfromCoordinators).set_Name("cmdProcessfromCoordinators");
			((Control)cmdProcessfromCoordinators).set_Size(new Size(124, 41));
			((Control)cmdProcessfromCoordinators).set_TabIndex(59);
			((Control)cmdProcessfromCoordinators).set_Text("Process files from\r\ncoordinators");
			((ButtonBase)cmdProcessfromCoordinators).set_UseVisualStyleBackColor(true);
			((Control)cmdProcessfromCoordinators).add_Click((EventHandler)cmdProcessfromCoordinators_Click);
			((Control)optDoublesEditor).set_AutoSize(true);
			((Control)optDoublesEditor).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optDoublesEditor).set_Location(new Point(17, 64));
			((Control)optDoublesEditor).set_Name("optDoublesEditor");
			((Control)optDoublesEditor).set_Size(new Size(107, 17));
			((Control)optDoublesEditor).set_TabIndex(62);
			((Control)optDoublesEditor).set_Text("Doubles editor");
			((ButtonBase)optDoublesEditor).set_UseVisualStyleBackColor(true);
			optDoublesEditor.add_CheckedChanged((EventHandler)optDoublesEditor_CheckedChanged);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmbDoubleCodes);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmdCheckDoubles);
			((Control)panelDoubles).get_Controls().Add((Control)(object)labelDouble);
			((Control)panelDoubles).get_Controls().Add((Control)(object)label32);
			((Control)panelDoubles).get_Controls().Add((Control)(object)lstDoubles);
			((Control)panelDoubles).get_Controls().Add((Control)(object)groupBox7);
			((Control)panelDoubles).get_Controls().Add((Control)(object)grpSetWDS);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmdStarsCheckAll);
			((Control)panelDoubles).get_Controls().Add((Control)(object)cmdStarsClearAll);
			((Control)panelDoubles).set_Location(new Point(914, 218));
			((Control)panelDoubles).set_Name("panelDoubles");
			((Control)panelDoubles).set_Size(new Size(1085, 237));
			((Control)panelDoubles).set_TabIndex(63);
			((Control)panelDoubles).set_Visible(false);
			((ListControl)cmbDoubleCodes).set_FormattingEnabled(true);
			cmbDoubleCodes.get_Items().AddRange(new object[7] { " ", "W", "E", "N", "S", "B", "F" });
			((Control)cmbDoubleCodes).set_Location(new Point(525, 60));
			((Control)cmbDoubleCodes).set_Name("cmbDoubleCodes");
			((Control)cmbDoubleCodes).set_Size(new Size(38, 21));
			((Control)cmbDoubleCodes).set_TabIndex(45);
			((Control)cmdCheckDoubles).set_Location(new Point(402, 52));
			((Control)cmdCheckDoubles).set_Name("cmdCheckDoubles");
			((Control)cmdCheckDoubles).set_Size(new Size(117, 36));
			((Control)cmdCheckDoubles).set_TabIndex(44);
			((Control)cmdCheckDoubles).set_Text("Check all events with double star code");
			((ButtonBase)cmdCheckDoubles).set_UseVisualStyleBackColor(true);
			((Control)cmdCheckDoubles).add_Click((EventHandler)cmdCheckDoubles_Click);
			((Control)labelDouble).set_AutoSize(true);
			((Control)labelDouble).set_Font(new Font("Courier New", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)labelDouble).set_Location(new Point(326, 110));
			((Control)labelDouble).set_Name("labelDouble");
			((Control)labelDouble).set_Size(new Size(50, 16));
			((Control)labelDouble).set_TabIndex(43);
			((Control)labelDouble).set_Text("Double");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Courier New", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label32).set_Location(new Point(326, 126));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(469, 15));
			((Control)label32).set_TabIndex(42);
			((Control)label32).set_Text("Name    Cmpt   Yr1  Yr2: PA1   PA2  :   Sep1   Sep2  :  Mag1  Mag2");
			((Control)lstDoubles).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDoubles).set_FormattingEnabled(true);
			lstDoubles.set_ItemHeight(14);
			((Control)lstDoubles).set_Location(new Point(325, 141));
			((Control)lstDoubles).set_Name("lstDoubles");
			lstDoubles.set_ScrollAlwaysVisible(true);
			((Control)lstDoubles).set_Size(new Size(490, 88));
			((Control)lstDoubles).set_TabIndex(41);
			((Control)groupBox7).get_Controls().Add((Control)(object)chkDoublesOnly);
			((Control)groupBox7).get_Controls().Add((Control)(object)txtXZ);
			((Control)groupBox7).get_Controls().Add((Control)(object)cmbNames);
			((Control)groupBox7).get_Controls().Add((Control)(object)label28);
			((Control)groupBox7).get_Controls().Add((Control)(object)label29);
			((Control)groupBox7).get_Controls().Add((Control)(object)cmbStarList);
			((Control)groupBox7).get_Controls().Add((Control)(object)label30);
			((Control)groupBox7).get_Controls().Add((Control)(object)label31);
			((Control)groupBox7).get_Controls().Add((Control)(object)cmdReadForDoubles);
			((Control)groupBox7).get_Controls().Add((Control)(object)txtZC);
			((Control)groupBox7).get_Controls().Add((Control)(object)txtSAO);
			((Control)groupBox7).set_Location(new Point(12, 46));
			((Control)groupBox7).set_Name("groupBox7");
			((Control)groupBox7).set_Size(new Size(300, 160));
			((Control)groupBox7).set_TabIndex(40);
			groupBox7.set_TabStop(false);
			((Control)groupBox7).set_Text("List selected star");
			((Control)chkDoublesOnly).set_AutoSize(true);
			((Control)chkDoublesOnly).set_Location(new Point(11, 89));
			((Control)chkDoublesOnly).set_Name("chkDoublesOnly");
			((Control)chkDoublesOnly).set_Size(new Size(199, 17));
			((Control)chkDoublesOnly).set_TabIndex(25);
			((Control)chkDoublesOnly).set_Text("only list events that might be doubles");
			((ButtonBase)chkDoublesOnly).set_UseVisualStyleBackColor(true);
			((Control)txtXZ).set_Location(new Point(243, 91));
			((Control)txtXZ).set_Name("txtXZ");
			((Control)txtXZ).set_Size(new Size(46, 20));
			((Control)txtXZ).set_TabIndex(21);
			((Control)txtXZ).add_TextChanged((EventHandler)txtXZ_TextChanged);
			((ListControl)cmbNames).set_FormattingEnabled(true);
			((Control)cmbNames).set_Location(new Point(11, 62));
			((Control)cmbNames).set_Name("cmbNames");
			((Control)cmbNames).set_Size(new Size(186, 21));
			((Control)cmbNames).set_TabIndex(15);
			cmbNames.add_SelectedIndexChanged((EventHandler)cmbNames_SelectedIndexChanged);
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Location(new Point(51, 47));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(67, 13));
			((Control)label28).set_TabIndex(14);
			((Control)label28).set_Text("Name of star");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Location(new Point(220, 39));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(21, 13));
			((Control)label29).set_TabIndex(16);
			((Control)label29).set_Text("ZC");
			((ListControl)cmbStarList).set_FormattingEnabled(true);
			cmbStarList.get_Items().AddRange(new object[6] { "Proper names", "Bayer letters", "All names", "Magnitude brighter than 2", "Magnitude brighter than 3", "Magnitude brighter than 4" });
			((Control)cmbStarList).set_Location(new Point(9, 23));
			((Control)cmbStarList).set_Name("cmbStarList");
			((Control)cmbStarList).set_Size(new Size(188, 21));
			((Control)cmbStarList).set_TabIndex(24);
			cmbStarList.add_SelectedIndexChanged((EventHandler)cmbStarList_SelectedIndexChanged);
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Location(new Point(212, 67));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(29, 13));
			((Control)label30).set_TabIndex(18);
			((Control)label30).set_Text("SAO");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Location(new Point(220, 95));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(21, 13));
			((Control)label31).set_TabIndex(20);
			((Control)label31).set_Text("XZ");
			((Control)cmdReadForDoubles).set_Location(new Point(34, 114));
			((Control)cmdReadForDoubles).set_Name("cmdReadForDoubles");
			((Control)cmdReadForDoubles).set_Size(new Size(108, 36));
			((Control)cmdReadForDoubles).set_TabIndex(5);
			((Control)cmdReadForDoubles).set_Text("List events for star");
			((ButtonBase)cmdReadForDoubles).set_UseVisualStyleBackColor(true);
			((Control)cmdReadForDoubles).add_Click((EventHandler)cmdReadForDoubles_Click);
			((Control)txtZC).set_Location(new Point(243, 35));
			((Control)txtZC).set_Name("txtZC");
			((Control)txtZC).set_Size(new Size(40, 20));
			((Control)txtZC).set_TabIndex(17);
			((Control)txtZC).add_TextChanged((EventHandler)txtZC_TextChanged);
			((Control)txtSAO).set_Location(new Point(243, 63));
			((Control)txtSAO).set_Name("txtSAO");
			((Control)txtSAO).set_Size(new Size(46, 20));
			((Control)txtSAO).set_TabIndex(19);
			((Control)txtSAO).add_TextChanged((EventHandler)txtSAO_TextChanged);
			((Control)grpSetWDS).get_Controls().Add((Control)(object)cmdApplyWDS);
			((Control)grpSetWDS).get_Controls().Add((Control)(object)cmbWDS);
			((Control)grpSetWDS).set_Location(new Point(878, 46));
			((Control)grpSetWDS).set_Name("grpSetWDS");
			((Control)grpSetWDS).set_Size(new Size(170, 130));
			((Control)grpSetWDS).set_TabIndex(39);
			grpSetWDS.set_TabStop(false);
			((Control)grpSetWDS).set_Text("Set WDS component letter");
			((Control)cmdApplyWDS).set_Location(new Point(7, 72));
			((Control)cmdApplyWDS).set_Name("cmdApplyWDS");
			((Control)cmdApplyWDS).set_Size(new Size(157, 39));
			((Control)cmdApplyWDS).set_TabIndex(6);
			((Control)cmdApplyWDS).set_Text("Apply this WDS component\r\nletter to the checked stars");
			((ButtonBase)cmdApplyWDS).set_UseVisualStyleBackColor(true);
			((Control)cmdApplyWDS).add_Click((EventHandler)cmdApplyWDS_Click);
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
			((Control)cmbWDS).set_Location(new Point(68, 31));
			((Control)cmbWDS).set_Name("cmbWDS");
			((Control)cmbWDS).set_Size(new Size(35, 21));
			((Control)cmbWDS).set_TabIndex(0);
			cmbWDS.add_SelectedIndexChanged((EventHandler)cmbWDS_SelectedIndexChanged);
			((Control)cmdStarsCheckAll).set_Location(new Point(706, 52));
			((Control)cmdStarsCheckAll).set_Name("cmdStarsCheckAll");
			((Control)cmdStarsCheckAll).set_Size(new Size(73, 36));
			((Control)cmdStarsCheckAll).set_TabIndex(38);
			((Control)cmdStarsCheckAll).set_Text("Set all checks");
			((ButtonBase)cmdStarsCheckAll).set_UseVisualStyleBackColor(true);
			((Control)cmdStarsCheckAll).add_Click((EventHandler)cmdStarsCheckAll_Click);
			((Control)cmdStarsClearAll).set_Location(new Point(586, 52));
			((Control)cmdStarsClearAll).set_Name("cmdStarsClearAll");
			((Control)cmdStarsClearAll).set_Size(new Size(73, 36));
			((Control)cmdStarsClearAll).set_TabIndex(37);
			((Control)cmdStarsClearAll).set_Text("Clear all checks");
			((ButtonBase)cmdStarsClearAll).set_UseVisualStyleBackColor(true);
			((Control)cmdStarsClearAll).add_Click((EventHandler)cmdStarsClearAll_Click);
			((Control)lblResidual2).set_AutoSize(true);
			((Control)lblResidual2).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblResidual2).set_Location(new Point(5, 16));
			((Control)lblResidual2).set_Name("lblResidual2");
			((Control)lblResidual2).set_Size(new Size(14, 14));
			((Control)lblResidual2).set_TabIndex(36);
			((Control)lblResidual2).set_Text("=");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(5, 2));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(784, 14));
			((Control)label34).set_TabIndex(35);
			((Control)label34).set_Text("    O-C    PA       l     b     AA    Limb Scale    P     D    MoonAlt  SunAlt  %ill   mag  Tel-dia    Observer");
			lstCheckedEdit.set_CheckOnClick(true);
			((Control)lstCheckedEdit).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstCheckedEdit).set_FormattingEnabled(true);
			((ListBox)lstCheckedEdit).set_HorizontalExtent(1900);
			((ListBox)lstCheckedEdit).set_HorizontalScrollbar(true);
			((Control)lstCheckedEdit).set_Location(new Point(12, 241));
			((Control)lstCheckedEdit).set_Name("lstCheckedEdit");
			((Control)lstCheckedEdit).set_Size(new Size(1104, 139));
			((Control)lstCheckedEdit).set_TabIndex(64);
			((ListBox)lstCheckedEdit).add_SelectedIndexChanged((EventHandler)lstCheckedEdit_SelectedIndexChanged);
			((Control)optGrazeEditor).set_AutoSize(true);
			((Control)optGrazeEditor).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optGrazeEditor).set_Location(new Point(17, 79));
			((Control)optGrazeEditor).set_Name("optGrazeEditor");
			((Control)optGrazeEditor).set_Size(new Size(94, 17));
			((Control)optGrazeEditor).set_TabIndex(65);
			((Control)optGrazeEditor).set_Text("Graze editor");
			((ButtonBase)optGrazeEditor).set_UseVisualStyleBackColor(true);
			optGrazeEditor.add_CheckedChanged((EventHandler)optGrazeEditor_CheckedChanged);
			((Control)optCombineRegions).set_AutoSize(true);
			optCombineRegions.set_Checked(true);
			((Control)optCombineRegions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optCombineRegions).set_Location(new Point(17, 20));
			((Control)optCombineRegions).set_Name("optCombineRegions");
			((Control)optCombineRegions).set_Size(new Size(123, 17));
			((Control)optCombineRegions).set_TabIndex(66);
			optCombineRegions.set_TabStop(true);
			((Control)optCombineRegions).set_Text("Combine Regions");
			((ButtonBase)optCombineRegions).set_UseVisualStyleBackColor(true);
			optCombineRegions.add_CheckedChanged((EventHandler)optCombineRegions_CheckedChanged);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpGrazeSites);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpGrazeDoubles);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpGrazeScopes);
			((Control)panelGraze).get_Controls().Add((Control)(object)chkAutoIncrement);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpEventDuration);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpTelescopeCodes);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpGrazeDR);
			((Control)panelGraze).get_Controls().Add((Control)(object)chkUseGrazeIDonly);
			((Control)panelGraze).get_Controls().Add((Control)(object)grp2ndLimit);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpDatums);
			((Control)panelGraze).get_Controls().Add((Control)(object)cmdTagAllGrazeEvents);
			((Control)panelGraze).get_Controls().Add((Control)(object)lblGrazeFile);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpGrazeRenumber);
			((Control)panelGraze).get_Controls().Add((Control)(object)cmdClearDuplicatteResidualFlags);
			((Control)panelGraze).get_Controls().Add((Control)(object)cmdRecordGrazeAsEdited);
			((Control)panelGraze).get_Controls().Add((Control)(object)lblSource);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpDuplicateGrazeEvents);
			((Control)panelGraze).get_Controls().Add((Control)(object)cmdGrazeEventsInEditor_Valid);
			((Control)panelGraze).get_Controls().Add((Control)(object)cmdGrazeEventsInEditor_No_StartEnd);
			((Control)panelGraze).get_Controls().Add((Control)(object)groupBox11);
			((Control)panelGraze).get_Controls().Add((Control)(object)cmbGrazeList);
			((Control)panelGraze).get_Controls().Add((Control)(object)cmdSelectArchiveForGrazeEdit);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpLargeResiduals);
			((Control)panelGraze).get_Controls().Add((Control)(object)cmdGrazeEventsToEditor);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpNullSeconds);
			((Control)panelGraze).get_Controls().Add((Control)(object)grpGrazeFlag);
			((Control)panelGraze).get_Controls().Add((Control)(object)chkAutoDisplay);
			((Control)panelGraze).set_Location(new Point(1, 399));
			((Control)panelGraze).set_Name("panelGraze");
			((Control)panelGraze).set_Size(new Size(1100, 272));
			((Control)panelGraze).set_TabIndex(67);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)chkSomaGrazeClearChecks);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)cmdSomaPasteSiteCoords);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)cmdCheckFromClipboard2);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)cmdGrazeChangeSites);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)lblnumberChecked2);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)label57);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)label52);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)label58);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)cmdGrazeSitesCheckeSameAsFirst_2);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)label59);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)txtLatD3);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)label60);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)txtLonD3);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)label61);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)txtLonM3);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)cmbAltDatum3);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)txtLonS3);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)txtAlt3);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)txtLatM3);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)cmbDatum3);
			((Control)grpGrazeSites).get_Controls().Add((Control)(object)txtLatS3);
			((Control)grpGrazeSites).set_Location(new Point(4, 100));
			((Control)grpGrazeSites).set_Name("grpGrazeSites");
			((Control)grpGrazeSites).set_Size(new Size(636, 195));
			((Control)grpGrazeSites).set_TabIndex(32);
			grpGrazeSites.set_TabStop(false);
			((Control)grpGrazeSites).set_Text("Graze sites");
			((Control)chkSomaGrazeClearChecks).set_AutoSize(true);
			chkSomaGrazeClearChecks.set_Checked(true);
			chkSomaGrazeClearChecks.set_CheckState((CheckState)1);
			((Control)chkSomaGrazeClearChecks).set_Location(new Point(449, 153));
			((Control)chkSomaGrazeClearChecks).set_Name("chkSomaGrazeClearChecks");
			((Control)chkSomaGrazeClearChecks).set_Size(new Size(141, 17));
			((Control)chkSomaGrazeClearChecks).set_TabIndex(53);
			((Control)chkSomaGrazeClearChecks).set_Text("Uncheck when updated");
			((ButtonBase)chkSomaGrazeClearChecks).set_UseVisualStyleBackColor(true);
			((Control)cmdSomaPasteSiteCoords).set_Location(new Point(455, 60));
			((Control)cmdSomaPasteSiteCoords).set_Name("cmdSomaPasteSiteCoords");
			((Control)cmdSomaPasteSiteCoords).set_Size(new Size(165, 21));
			((Control)cmdSomaPasteSiteCoords).set_TabIndex(52);
			((Control)cmdSomaPasteSiteCoords).set_Text("Soma - paste site co-ords");
			((ButtonBase)cmdSomaPasteSiteCoords).set_UseVisualStyleBackColor(true);
			((Control)cmdSomaPasteSiteCoords).add_Click((EventHandler)cmdSomaPasteSiteCoords_Click);
			((Control)cmdCheckFromClipboard2).set_Location(new Point(455, 18));
			((Control)cmdCheckFromClipboard2).set_Name("cmdCheckFromClipboard2");
			((Control)cmdCheckFromClipboard2).set_Size(new Size(165, 21));
			((Control)cmdCheckFromClipboard2).set_TabIndex(51);
			((Control)cmdCheckFromClipboard2).set_Text("Soma - check copied events");
			((ButtonBase)cmdCheckFromClipboard2).set_UseVisualStyleBackColor(true);
			((Control)cmdCheckFromClipboard2).add_Click((EventHandler)cmdCheckFromClipboard2_Click);
			((Control)cmdGrazeChangeSites).set_Location(new Point(449, 101));
			((Control)cmdGrazeChangeSites).set_Name("cmdGrazeChangeSites");
			((Control)cmdGrazeChangeSites).set_Size(new Size(124, 46));
			((Control)cmdGrazeChangeSites).set_TabIndex(50);
			((Control)cmdGrazeChangeSites).set_Text("Update site coords for all checked");
			((ButtonBase)cmdGrazeChangeSites).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeChangeSites).add_Click((EventHandler)cmdGrazeChangeSites_Click);
			((Control)lblnumberChecked2).set_Location(new Point(19, 21));
			((Control)lblnumberChecked2).set_Name("lblnumberChecked2");
			((Control)lblnumberChecked2).set_RightToLeft((RightToLeft)0);
			((Control)lblnumberChecked2).set_Size(new Size(20, 11));
			((Control)lblnumberChecked2).set_TabIndex(5);
			((Control)lblnumberChecked2).set_Text("0");
			lblnumberChecked2.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label57).set_Location(new Point(62, 46));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(54, 13));
			((Control)label57).set_TabIndex(49);
			((Control)label57).set_Text("Longitude");
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_Location(new Point(57, 21));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(390, 13));
			((Control)label52).set_TabIndex(4);
			((Control)label52).set_Text("Check all events with same Source, Site and Observer as the first checked event");
			((Control)label58).set_AutoSize(true);
			((Control)label58).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label58).set_Location(new Point(189, 46));
			((Control)label58).set_Name("label58");
			((Control)label58).set_Size(new Size(45, 13));
			((Control)label58).set_TabIndex(48);
			((Control)label58).set_Text("Latitude");
			((Control)cmdGrazeSitesCheckeSameAsFirst_2).set_Location(new Point(39, 19));
			((Control)cmdGrazeSitesCheckeSameAsFirst_2).set_Name("cmdGrazeSitesCheckeSameAsFirst_2");
			((Control)cmdGrazeSitesCheckeSameAsFirst_2).set_Size(new Size(15, 17));
			((Control)cmdGrazeSitesCheckeSameAsFirst_2).set_TabIndex(3);
			((Control)cmdGrazeSitesCheckeSameAsFirst_2).set_Text(" ");
			((ButtonBase)cmdGrazeSitesCheckeSameAsFirst_2).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeSitesCheckeSameAsFirst_2).add_Click((EventHandler)cmdGrazeSitesCheckeSameAsFirst_2_Click);
			((Control)label59).set_AutoSize(true);
			((Control)label59).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label59).set_Location(new Point(17, 95));
			((Control)label59).set_Name("label59");
			((Control)label59).set_Size(new Size(85, 13));
			((Control)label59).set_TabIndex(47);
			((Control)label59).set_Text("Horizontal datum");
			((Control)txtLatD3).set_Location(new Point(160, 65));
			((Control)txtLatD3).set_Name("txtLatD3");
			((Control)txtLatD3).set_Size(new Size(44, 20));
			((Control)txtLatD3).set_TabIndex(39);
			((TextBoxBase)txtLatD3).add_MouseClick(new MouseEventHandler(txtLatD3_MouseClick));
			((Control)txtLatD3).add_Enter((EventHandler)txtLatD3_Enter);
			((Control)label60).set_AutoSize(true);
			((Control)label60).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label60).set_Location(new Point(313, 46));
			((Control)label60).set_Name("label60");
			((Control)label60).set_Size(new Size(42, 13));
			((Control)label60).set_TabIndex(46);
			((Control)label60).set_Text("Altitude");
			((Control)txtLonD3).set_Location(new Point(38, 65));
			((Control)txtLonD3).set_Name("txtLonD3");
			((Control)txtLonD3).set_Size(new Size(44, 20));
			((Control)txtLonD3).set_TabIndex(36);
			((TextBoxBase)txtLonD3).add_MouseClick(new MouseEventHandler(txtLonD3_MouseClick));
			((Control)txtLonD3).add_Enter((EventHandler)txtLonD3_Enter);
			((Control)label61).set_AutoSize(true);
			((Control)label61).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label61).set_Location(new Point(290, 95));
			((Control)label61).set_Name("label61");
			((Control)label61).set_Size(new Size(73, 13));
			((Control)label61).set_TabIndex(45);
			((Control)label61).set_Text("Vertical datum");
			((Control)txtLonM3).set_Location(new Point(82, 65));
			((Control)txtLonM3).set_Name("txtLonM3");
			((Control)txtLonM3).set_Size(new Size(25, 20));
			((Control)txtLonM3).set_TabIndex(37);
			((TextBoxBase)txtLonM3).add_MouseClick(new MouseEventHandler(txtLonM3_MouseClick));
			((Control)txtLonM3).add_Enter((EventHandler)txtLonM3_Enter);
			cmbAltDatum3.set_FlatStyle((FlatStyle)0);
			((ListControl)cmbAltDatum3).set_FormattingEnabled(true);
			cmbAltDatum3.get_Items().AddRange(new object[2] { "Mean Sea Level", "Spheroid" });
			((Control)cmbAltDatum3).set_Location(new Point(285, 112));
			((Control)cmbAltDatum3).set_Name("cmbAltDatum3");
			((Control)cmbAltDatum3).set_Size(new Size(107, 21));
			((Control)cmbAltDatum3).set_TabIndex(44);
			((Control)txtLonS3).set_Location(new Point(107, 65));
			((Control)txtLonS3).set_Name("txtLonS3");
			((Control)txtLonS3).set_Size(new Size(36, 20));
			((Control)txtLonS3).set_TabIndex(38);
			((TextBoxBase)txtLonS3).add_MouseClick(new MouseEventHandler(txtLonS3_MouseClick));
			((Control)txtLonS3).add_Enter((EventHandler)txtLonS3_Enter);
			((Control)txtAlt3).set_Location(new Point(311, 65));
			((Control)txtAlt3).set_Name("txtAlt3");
			((Control)txtAlt3).set_Size(new Size(44, 20));
			((Control)txtAlt3).set_TabIndex(43);
			((TextBoxBase)txtAlt3).add_MouseClick(new MouseEventHandler(txtAlt3_MouseClick));
			((Control)txtAlt3).add_Enter((EventHandler)txtAlt3_Enter);
			((Control)txtLatM3).set_Location(new Point(204, 65));
			((Control)txtLatM3).set_Name("txtLatM3");
			((Control)txtLatM3).set_Size(new Size(25, 20));
			((Control)txtLatM3).set_TabIndex(40);
			((TextBoxBase)txtLatM3).add_MouseClick(new MouseEventHandler(txtLatM3_MouseClick));
			((Control)txtLatM3).add_Enter((EventHandler)txtLatM3_Enter);
			cmbDatum3.set_FlatStyle((FlatStyle)0);
			((ListControl)cmbDatum3).set_FormattingEnabled(true);
			cmbDatum3.get_Items().AddRange(new object[79]
			{
				" 0  unknown (WGS84 used)", " 1  RGO code 1", " 2  RGO code 2", " 3  RGO code 3", " 4  RGO code 4", " 5  RGO code 5", "10  GoogleEarth (WGS84)", "12  Christmas Island Astro 1967", "13  Chua Astro (Brazil Geodetic)", "14  Corrego Alegre (Brazil)",
				"15  Easter Island Astro 1967", "16  European 1950  ", "17  Graciosa Island (Azores)", "18  Gizo, Provisional DOS  ", "19  Guam", "20  Heard Astro 1969", "21  Iben Astro, Navy 1947 (Truk) ", "22  Indian", "23  Isla Socorro Astro", "24  Johnston Island 1961 ",
				"25  Kusaie Astro 1962,1965", "26  Luzon 1911 (Philippines) ", "27  Midway Astro 1961", "28  New Zealand 1949 ", "29  North American 1927  ", "30 *Cape Canaveral ", "31 *White Sands", "32  Old Bavarian ", "33  Old Hawaiian ", "34  Ordnance Survey of Great Britain 1936 ",
				"35  Pico de las Nieves (Canaries) ", "36  Pitcairn Island Astro ", "37  Potsdam", "38  Provisional South American 1956", "39  Provisional South Chile 1963  ", "40  Pulkovo 1942  ", "41  South American 1969", "42  Southeast Island (Mahe)", "43  South Georgia Astro", "44  Swallow Islands (Solomons)",
				"45  Tananarive", "46  Tokyo ", "47  Tristan Astro 1968", "48  Viti Levu 1916 (Fiji) ", "49  Wake Island Astro 1952", "50  Yof Astro 1967 (Dakar)", "51  Palmer Astro 1969 (Antarctica)", "52  Efate (New Hebrides)", "53  Marcus Island 1965", "54  Canton Astro 1966  ",
				"56  Yap Island", "58  Kourou (French Guiana)", "59  Ordnance Survey of Great Britain 1970 ", "60  Qornoq (Greenland)", "61  Adindan (Ethiopia) ", "62  American Samoa 1962", "63  Arc-Cape (South Africa)", "64  Argentine  ", "65  Ascension Island 1958  ", "66  Australian Geodetic",
				"67  Bermuda 1957", "68  Berne 1898 ", "69  Betio Island 1966  ", "70  Camp Area Astro 1961-62 USGS", "71  Batavia (Java)", "72  Palestine (Israel,Jordan)", "73  Hermannskogel (Austria,Czech.,Yugoslavia)", "74  Kandawala (Ceylon)", "80  ETRS89 (European Terrestial Reference System)", "81  Amersfoort 1885 (Netherlands)",
				"82  NAD83/NAD1983 (=WGS84)", "84  WGS84", "85  JGD2000 (=WGS84)", "86  GDA94 (=WGS84)", "87  NZGD2000 (=WGS84)", "88  NGRF2000 (=WGS84)", "89  KDG2000 (=WGS84)", "90  Hartebeesthoek94 (=WGS84)", "91  TWD94 (=WGS84)"
			});
			((Control)cmbDatum3).set_Location(new Point(12, 112));
			((Control)cmbDatum3).set_Name("cmbDatum3");
			((Control)cmbDatum3).set_Size(new Size(266, 21));
			((Control)cmbDatum3).set_TabIndex(42);
			((Control)txtLatS3).set_Location(new Point(229, 65));
			((Control)txtLatS3).set_Name("txtLatS3");
			((Control)txtLatS3).set_Size(new Size(36, 20));
			((Control)txtLatS3).set_TabIndex(41);
			((TextBoxBase)txtLatS3).add_MouseClick(new MouseEventHandler(txtLatS3_MouseClick));
			((Control)txtLatS3).add_Enter((EventHandler)txtLatS3_Enter);
			((Control)grpGrazeDoubles).get_Controls().Add((Control)(object)cmdGrazeClearChecks);
			((Control)grpGrazeDoubles).get_Controls().Add((Control)(object)cmbDoubleCode);
			((Control)grpGrazeDoubles).get_Controls().Add((Control)(object)cmdGrazeDoublesCheck);
			((Control)grpGrazeDoubles).get_Controls().Add((Control)(object)groupBox8);
			((Control)grpGrazeDoubles).set_Location(new Point(1366, 45));
			((Control)grpGrazeDoubles).set_Name("grpGrazeDoubles");
			((Control)grpGrazeDoubles).set_Size(new Size(490, 189));
			((Control)grpGrazeDoubles).set_TabIndex(33);
			grpGrazeDoubles.set_TabStop(false);
			((Control)grpGrazeDoubles).set_Text("Double star settings");
			((Control)cmdGrazeClearChecks).set_Location(new Point(277, 76));
			((Control)cmdGrazeClearChecks).set_Name("cmdGrazeClearChecks");
			((Control)cmdGrazeClearChecks).set_Size(new Size(97, 26));
			((Control)cmdGrazeClearChecks).set_TabIndex(43);
			((Control)cmdGrazeClearChecks).set_Text("Clear all checks");
			((ButtonBase)cmdGrazeClearChecks).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeClearChecks).add_Click((EventHandler)cmdGrazeClearChecks_Click);
			((ListControl)cmbDoubleCode).set_FormattingEnabled(true);
			cmbDoubleCode.get_Items().AddRange(new object[7] { " ", "W", "E", "N", "S", "B", "F" });
			((Control)cmbDoubleCode).set_Location(new Point(387, 42));
			((Control)cmbDoubleCode).set_Name("cmbDoubleCode");
			((Control)cmbDoubleCode).set_Size(new Size(38, 21));
			((Control)cmbDoubleCode).set_TabIndex(42);
			((Control)cmdGrazeDoublesCheck).set_Location(new Point(264, 34));
			((Control)cmdGrazeDoublesCheck).set_Name("cmdGrazeDoublesCheck");
			((Control)cmdGrazeDoublesCheck).set_Size(new Size(117, 36));
			((Control)cmdGrazeDoublesCheck).set_TabIndex(41);
			((Control)cmdGrazeDoublesCheck).set_Text("Check all events with double star code");
			((ButtonBase)cmdGrazeDoublesCheck).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeDoublesCheck).add_Click((EventHandler)cmdGrazeDoublesCheck_Click);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmdGrazeWDS);
			((Control)groupBox8).get_Controls().Add((Control)(object)cmbGrazeWDS);
			((Control)groupBox8).set_Location(new Point(28, 29));
			((Control)groupBox8).set_Name("groupBox8");
			((Control)groupBox8).set_Size(new Size(170, 130));
			((Control)groupBox8).set_TabIndex(40);
			groupBox8.set_TabStop(false);
			((Control)groupBox8).set_Text("Set WDS component letter");
			((Control)cmdGrazeWDS).set_Location(new Point(7, 72));
			((Control)cmdGrazeWDS).set_Name("cmdGrazeWDS");
			((Control)cmdGrazeWDS).set_Size(new Size(157, 39));
			((Control)cmdGrazeWDS).set_TabIndex(6);
			((Control)cmdGrazeWDS).set_Text("Apply this WDS component\r\nletter to the checked stars");
			((ButtonBase)cmdGrazeWDS).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeWDS).add_Click((EventHandler)cmdGrazeWDS_Click);
			((ListControl)cmbGrazeWDS).set_FormattingEnabled(true);
			cmbGrazeWDS.get_Items().AddRange(new object[53]
			{
				" ", "A", "B", "C", "D", "E", "F", "G", "H", "I",
				"J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
				"T", "U", "V", "W", "X", "Y", "Z", "a", "b", "c",
				"d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
				"n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
				"x", "y", "z"
			});
			((Control)cmbGrazeWDS).set_Location(new Point(68, 31));
			((Control)cmbGrazeWDS).set_Name("cmbGrazeWDS");
			((Control)cmbGrazeWDS).set_Size(new Size(35, 21));
			((Control)cmbGrazeWDS).set_TabIndex(0);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)cmdCheckFromClipboard);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)cmdPasteLocation);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)cmdPasteObserver);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)cmdGrazeUpdateSites);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)label75);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)lblCharsLeft);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)label68);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)txtPlace);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)label70);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)lblCharsLeftName);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)lblObserverName);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)label50);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)txtName);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)grpTelescopesAndNames);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)lblnumberChecked);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)label42);
			((Control)grpGrazeScopes).get_Controls().Add((Control)(object)cmdGrazeSitesCheckeSameAsFirst);
			((Control)grpGrazeScopes).set_Location(new Point(901, 49));
			((Control)grpGrazeScopes).set_Name("grpGrazeScopes");
			((Control)grpGrazeScopes).set_Size(new Size(723, 199));
			((Control)grpGrazeScopes).set_TabIndex(29);
			grpGrazeScopes.set_TabStop(false);
			((Control)grpGrazeScopes).set_Text("Telescope details, site && observer names");
			((Control)cmdCheckFromClipboard).set_Location(new Point(543, 79));
			((Control)cmdCheckFromClipboard).set_Name("cmdCheckFromClipboard");
			((Control)cmdCheckFromClipboard).set_Size(new Size(165, 21));
			((Control)cmdCheckFromClipboard).set_TabIndex(38);
			((Control)cmdCheckFromClipboard).set_Text("Soma - check copied events");
			((ButtonBase)cmdCheckFromClipboard).set_UseVisualStyleBackColor(true);
			((Control)cmdCheckFromClipboard).add_Click((EventHandler)cmdCheckFromClipboard_Click);
			((Control)cmdPasteLocation).set_Location(new Point(663, 149));
			((Control)cmdPasteLocation).set_Name("cmdPasteLocation");
			((Control)cmdPasteLocation).set_Size(new Size(57, 21));
			((Control)cmdPasteLocation).set_TabIndex(37);
			((Control)cmdPasteLocation).set_Text("Paste");
			((ButtonBase)cmdPasteLocation).set_UseVisualStyleBackColor(true);
			((Control)cmdPasteLocation).add_Click((EventHandler)cmdPasteLocation_Click);
			((Control)cmdPasteObserver).set_Location(new Point(472, 79));
			((Control)cmdPasteObserver).set_Name("cmdPasteObserver");
			((Control)cmdPasteObserver).set_Size(new Size(57, 21));
			((Control)cmdPasteObserver).set_TabIndex(36);
			((Control)cmdPasteObserver).set_Text("Paste");
			((ButtonBase)cmdPasteObserver).set_UseVisualStyleBackColor(true);
			((Control)cmdPasteObserver).add_Click((EventHandler)cmdPasteObserver_Click);
			((Control)cmdGrazeUpdateSites).set_Location(new Point(505, 23));
			((Control)cmdGrazeUpdateSites).set_Name("cmdGrazeUpdateSites");
			((Control)cmdGrazeUpdateSites).set_Size(new Size(196, 38));
			((Control)cmdGrazeUpdateSites).set_TabIndex(35);
			((Control)cmdGrazeUpdateSites).set_Text("Update telescope details, location,\r\nand observer name - for all Checked.");
			((ButtonBase)cmdGrazeUpdateSites).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeUpdateSites).add_Click((EventHandler)cmdGrazeUpdateSites_Click);
			((Control)label75).set_AutoSize(true);
			((Control)label75).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label75).set_Location(new Point(259, 135));
			((Control)label75).set_Name("label75");
			((Control)label75).set_Size(new Size(247, 13));
			((Control)label75).set_TabIndex(34);
			((Control)label75).set_Text("Name of nearby city, town or landmark, plus country");
			((Control)lblCharsLeft).set_AutoSize(true);
			((Control)lblCharsLeft).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCharsLeft).set_Location(new Point(640, 135));
			((Control)lblCharsLeft).set_Name("lblCharsLeft");
			((Control)lblCharsLeft).set_Size(new Size(19, 13));
			((Control)lblCharsLeft).set_TabIndex(32);
			((Control)lblCharsLeft).set_Text("50");
			((Control)label68).set_AutoSize(true);
			((Control)label68).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label68).set_Location(new Point(526, 135));
			((Control)label68).set_Name("label68");
			((Control)label68).set_Size(new Size(115, 13));
			((Control)label68).set_TabIndex(31);
			((Control)label68).set_Text("Limit of 50 characters : ");
			((Control)txtPlace).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtPlace).set_Location(new Point(262, 149));
			((TextBoxBase)txtPlace).set_MaxLength(50);
			((Control)txtPlace).set_Name("txtPlace");
			((Control)txtPlace).set_Size(new Size(395, 20));
			((Control)txtPlace).set_TabIndex(30);
			((Control)txtPlace).set_Text("My home town, My state, My Country");
			((Control)txtPlace).add_TextChanged((EventHandler)txtPlace_TextChanged);
			((Control)txtPlace).add_Enter((EventHandler)txtPlace_Enter);
			((Control)txtPlace).add_KeyUp(new KeyEventHandler(txtPlace_KeyUp));
			((Control)label70).set_AutoSize(true);
			((Control)label70).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label70).set_Location(new Point(269, 100));
			((Control)label70).set_Name("label70");
			((Control)label70).set_Size(new Size(163, 13));
			((Control)label70).set_TabIndex(9);
			((Control)label70).set_Text("Initial + Family name. eg 'J. Smith'");
			((Control)lblCharsLeftName).set_AutoSize(true);
			((Control)lblCharsLeftName).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCharsLeftName).set_Location(new Point(367, 62));
			((Control)lblCharsLeftName).set_Name("lblCharsLeftName");
			((Control)lblCharsLeftName).set_Size(new Size(70, 13));
			((Control)lblCharsLeftName).set_TabIndex(8);
			((Control)lblCharsLeftName).set_Text("25 characters");
			((Control)lblObserverName).set_AutoSize(true);
			((Control)lblObserverName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblObserverName).set_Location(new Point(435, 85));
			((Control)lblObserverName).set_Name("lblObserverName");
			((Control)lblObserverName).set_Size(new Size(19, 13));
			((Control)lblObserverName).set_TabIndex(6);
			((Control)lblObserverName).set_Text("25");
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label50).set_Location(new Point(260, 62));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(101, 13));
			((Control)label50).set_TabIndex(5);
			((Control)label50).set_Text("Name for Observers");
			((Control)txtName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtName).set_Location(new Point(262, 80));
			((TextBoxBase)txtName).set_MaxLength(25);
			((Control)txtName).set_Name("txtName");
			((Control)txtName).set_Size(new Size(173, 20));
			((Control)txtName).set_TabIndex(7);
			((Control)txtName).add_TextChanged((EventHandler)txtName_TextChanged);
			((Control)txtName).add_Enter((EventHandler)txtName_Enter);
			((Control)txtName).add_KeyUp(new KeyEventHandler(txtName_KeyUp));
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)label43);
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)cmbMount);
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)label44);
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)cmbDrive);
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)label45);
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)label46);
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)label47);
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)txtFocalLength);
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)label48);
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)label49);
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)cmbTelescope);
			((Control)grpTelescopesAndNames).get_Controls().Add((Control)(object)txtAperture);
			((Control)grpTelescopesAndNames).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)grpTelescopesAndNames).set_Location(new Point(14, 43));
			((Control)grpTelescopesAndNames).set_Name("grpTelescopesAndNames");
			((Control)grpTelescopesAndNames).set_Size(new Size(226, 152));
			((Control)grpTelescopesAndNames).set_TabIndex(3);
			grpTelescopesAndNames.set_TabStop(false);
			((Control)grpTelescopesAndNames).set_Text("Telescope");
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label43).set_Location(new Point(16, 101));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(59, 13));
			((Control)label43).set_TabIndex(8);
			((Control)label43).set_Text("Mounting");
			cmbMount.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbMount).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbMount).set_FormattingEnabled(true);
			cmbMount.get_Items().AddRange(new object[3] { "", "Equatorial", "Altazimuth" });
			((Control)cmbMount).set_Location(new Point(77, 97));
			((Control)cmbMount).set_Name("cmbMount");
			((Control)cmbMount).set_Size(new Size(131, 21));
			((Control)cmbMount).set_TabIndex(9);
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label44).set_Location(new Point(38, 128));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(37, 13));
			((Control)label44).set_TabIndex(10);
			((Control)label44).set_Text("Drive");
			cmbDrive.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbDrive).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbDrive).set_FormattingEnabled(true);
			cmbDrive.get_Items().AddRange(new object[3] { "", "Clock Driven", "Manual" });
			((Control)cmbDrive).set_Location(new Point(77, 124));
			((Control)cmbDrive).set_Name("cmbDrive");
			((Control)cmbDrive).set_Size(new Size(131, 21));
			((Control)cmbDrive).set_TabIndex(11);
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(32, 72));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(43, 13));
			((Control)label45).set_TabIndex(6);
			((Control)label45).set_Text("Optics");
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label46).set_Location(new Point(147, 46));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(23, 13));
			((Control)label46).set_TabIndex(5);
			((Control)label46).set_Text("cm");
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(23, 46));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(77, 13));
			((Control)label47).set_TabIndex(3);
			((Control)label47).set_Text("Focal length");
			((Control)txtFocalLength).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFocalLength).set_Location(new Point(102, 42));
			((Control)txtFocalLength).set_Name("txtFocalLength");
			((Control)txtFocalLength).set_Size(new Size(43, 20));
			((Control)txtFocalLength).set_TabIndex(4);
			((TextBoxBase)txtFocalLength).add_MouseClick(new MouseEventHandler(txtFocalLength_MouseClick));
			((Control)txtFocalLength).add_Enter((EventHandler)txtFocalLength_Enter);
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label48).set_Location(new Point(143, 20));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(23, 13));
			((Control)label48).set_TabIndex(2);
			((Control)label48).set_Text("cm");
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label49).set_Location(new Point(43, 20));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(55, 13));
			((Control)label49).set_TabIndex(0);
			((Control)label49).set_Text("Aperture");
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
			((Control)lblnumberChecked).set_Location(new Point(8, 22));
			((Control)lblnumberChecked).set_Name("lblnumberChecked");
			((Control)lblnumberChecked).set_RightToLeft((RightToLeft)0);
			((Control)lblnumberChecked).set_Size(new Size(20, 11));
			((Control)lblnumberChecked).set_TabIndex(2);
			((Control)lblnumberChecked).set_Text("0");
			lblnumberChecked.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Location(new Point(46, 22));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(390, 13));
			((Control)label42).set_TabIndex(1);
			((Control)label42).set_Text("Check all events with same Source, Site and Observer as the first checked event");
			((Control)cmdGrazeSitesCheckeSameAsFirst).set_Location(new Point(28, 20));
			((Control)cmdGrazeSitesCheckeSameAsFirst).set_Name("cmdGrazeSitesCheckeSameAsFirst");
			((Control)cmdGrazeSitesCheckeSameAsFirst).set_Size(new Size(15, 17));
			((Control)cmdGrazeSitesCheckeSameAsFirst).set_TabIndex(0);
			((Control)cmdGrazeSitesCheckeSameAsFirst).set_Text(" ");
			((ButtonBase)cmdGrazeSitesCheckeSameAsFirst).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeSitesCheckeSameAsFirst).add_Click((EventHandler)cmdGrazeSitesCheckeSameAsFirst_Click);
			((Control)chkAutoIncrement).set_AutoSize(true);
			chkAutoIncrement.set_Checked(Settings.Default.ArchiveGrazesIncrement);
			((Control)chkAutoIncrement).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ArchiveGrazesIncrement", true, (DataSourceUpdateMode)1));
			((Control)chkAutoIncrement).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAutoIncrement).set_Location(new Point(160, 114));
			((Control)chkAutoIncrement).set_Name("chkAutoIncrement");
			((Control)chkAutoIncrement).set_Size(new Size(65, 28));
			((Control)chkAutoIncrement).set_TabIndex(35);
			((Control)chkAutoIncrement).set_Text("auto \r\nincrement");
			((ButtonBase)chkAutoIncrement).set_TextAlign(ContentAlignment.MiddleCenter);
			((ButtonBase)chkAutoIncrement).set_UseVisualStyleBackColor(true);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)cmdGrazeSetDoubleCode);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)label62);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)cmbGrazeDoubles);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)label55);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)label54);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)cmdSetMidTime);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)txtSecondsForDuration);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)label53);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)label51);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)lblCurrentEventTime);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)cmdSetGrazeEventDuration);
			((Control)grpEventDuration).get_Controls().Add((Control)(object)txtEventDuration);
			((Control)grpEventDuration).set_Location(new Point(382, 45));
			((Control)grpEventDuration).set_Name("grpEventDuration");
			((Control)grpEventDuration).set_Size(new Size(521, 206));
			((Control)grpEventDuration).set_TabIndex(34);
			grpEventDuration.set_TabStop(false);
			((Control)grpEventDuration).set_Text("Set duration of current  (highlighted/selected)  event");
			((Control)cmdGrazeSetDoubleCode).set_Location(new Point(244, 110));
			((Control)cmdGrazeSetDoubleCode).set_Name("cmdGrazeSetDoubleCode");
			((Control)cmdGrazeSetDoubleCode).set_Size(new Size(86, 49));
			((Control)cmdGrazeSetDoubleCode).set_TabIndex(23);
			((Control)cmdGrazeSetDoubleCode).set_Text("Set double code for selected event");
			((ButtonBase)cmdGrazeSetDoubleCode).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeSetDoubleCode).set_Visible(false);
			((Control)cmdGrazeSetDoubleCode).add_Click((EventHandler)cmdGrazeSetDoubleCode_Click);
			((Control)label62).set_AutoSize(true);
			((Control)label62).set_Location(new Point(228, 62));
			((Control)label62).set_Name("label62");
			((Control)label62).set_Size(new Size(88, 13));
			((Control)label62).set_TabIndex(22);
			((Control)label62).set_Text("Double star code");
			((Control)label62).set_Visible(false);
			cmbGrazeDoubles.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbGrazeDoubles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbGrazeDoubles).set_FormattingEnabled(true);
			cmbGrazeDoubles.get_Items().AddRange(new object[8] { "None", "Preceding (West)", "Following (East)", "North component", "South component", "Brighter component", "Fainter component", "Unidentified star" });
			((Control)cmbGrazeDoubles).set_Location(new Point(231, 76));
			((Control)cmbGrazeDoubles).set_Name("cmbGrazeDoubles");
			((Control)cmbGrazeDoubles).set_Size(new Size(114, 21));
			((Control)cmbGrazeDoubles).set_TabIndex(21);
			((Control)cmbGrazeDoubles).set_Visible(false);
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Location(new Point(28, 174));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(317, 26));
			((Control)label55).set_TabIndex(0);
			((Control)label55).set_Text("Note: This function makes adjustments to the currently highlighted\r\nevent. The Check-box settings are not used.");
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Location(new Point(152, 65));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(49, 13));
			((Control)label54).set_TabIndex(5);
			((Control)label54).set_Text("Seconds");
			((Control)cmdSetMidTime).set_Location(new Point(133, 111));
			((Control)cmdSetMidTime).set_Name("cmdSetMidTime");
			((Control)cmdSetMidTime).set_Size(new Size(87, 47));
			((Control)cmdSetMidTime).set_TabIndex(8);
			((Control)cmdSetMidTime).set_Text("Set mid-time (secs) for selected event");
			((ButtonBase)cmdSetMidTime).set_UseVisualStyleBackColor(true);
			((Control)cmdSetMidTime).add_Click((EventHandler)cmdSetMidTime_Click);
			((Control)txtSecondsForDuration).set_Location(new Point(151, 79));
			((Control)txtSecondsForDuration).set_Name("txtSecondsForDuration");
			((Control)txtSecondsForDuration).set_Size(new Size(50, 20));
			((Control)txtSecondsForDuration).set_TabIndex(6);
			((TextBoxBase)txtSecondsForDuration).add_MouseClick(new MouseEventHandler(txtSecondsForDuration_MouseClick));
			((Control)txtSecondsForDuration).add_Enter((EventHandler)txtSecondsForDuration_Enter);
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Location(new Point(56, 64));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(47, 13));
			((Control)label53).set_TabIndex(3);
			((Control)label53).set_Text("Duration");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_Location(new Point(15, 22));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(71, 13));
			((Control)label51).set_TabIndex(0);
			((Control)label51).set_Text("Current event");
			((Control)lblCurrentEventTime).set_AutoSize(true);
			((Control)lblCurrentEventTime).set_Location(new Point(15, 39));
			((Control)lblCurrentEventTime).set_Name("lblCurrentEventTime");
			((Control)lblCurrentEventTime).set_Size(new Size(30, 13));
			((Control)lblCurrentEventTime).set_TabIndex(1);
			((Control)lblCurrentEventTime).set_Text("Time");
			((Control)cmdSetGrazeEventDuration).set_Location(new Point(42, 110));
			((Control)cmdSetGrazeEventDuration).set_Name("cmdSetGrazeEventDuration");
			((Control)cmdSetGrazeEventDuration).set_Size(new Size(72, 49));
			((Control)cmdSetGrazeEventDuration).set_TabIndex(7);
			((Control)cmdSetGrazeEventDuration).set_Text("Set duration for selected event");
			((ButtonBase)cmdSetGrazeEventDuration).set_UseVisualStyleBackColor(true);
			((Control)cmdSetGrazeEventDuration).add_Click((EventHandler)cmdSetGrazeEventDuration_Click);
			((Control)txtEventDuration).set_Location(new Point(59, 78));
			((Control)txtEventDuration).set_Name("txtEventDuration");
			((Control)txtEventDuration).set_Size(new Size(50, 20));
			((Control)txtEventDuration).set_TabIndex(4);
			((TextBoxBase)txtEventDuration).add_MouseClick(new MouseEventHandler(txtEventDuration_MouseClick));
			((Control)txtEventDuration).add_Enter((EventHandler)txtEventDuration_Enter);
			((Control)grpTelescopeCodes).get_Controls().Add((Control)(object)cmdSetPEApplication);
			((Control)grpTelescopeCodes).get_Controls().Add((Control)(object)cmdGrazeSitesAutoCorrect);
			((Control)grpTelescopeCodes).get_Controls().Add((Control)(object)cmdGrazesGetTelescopes);
			((Control)grpTelescopeCodes).set_Location(new Point(865, 45));
			((Control)grpTelescopeCodes).set_Name("grpTelescopeCodes");
			((Control)grpTelescopeCodes).set_Size(new Size(317, 156));
			((Control)grpTelescopeCodes).set_TabIndex(31);
			grpTelescopeCodes.set_TabStop(false);
			((Control)grpTelescopeCodes).set_Text("Telescope codes,   ILOC PE application codes");
			((Control)cmdSetPEApplication).set_Location(new Point(211, 38));
			((Control)cmdSetPEApplication).set_Name("cmdSetPEApplication");
			((Control)cmdSetPEApplication).set_Size(new Size(74, 50));
			((Control)cmdSetPEApplication).set_TabIndex(2);
			((Control)cmdSetPEApplication).set_Text("Set PE application for ILOC");
			((ButtonBase)cmdSetPEApplication).set_UseVisualStyleBackColor(true);
			((Control)cmdSetPEApplication).add_Click((EventHandler)cmdSetPEApplication_Click);
			((Control)cmdGrazeSitesAutoCorrect).set_Location(new Point(40, 50));
			((Control)cmdGrazeSitesAutoCorrect).set_Name("cmdGrazeSitesAutoCorrect");
			((Control)cmdGrazeSitesAutoCorrect).set_Size(new Size(72, 20));
			((Control)cmdGrazeSitesAutoCorrect).set_TabIndex(1);
			((Control)cmdGrazeSitesAutoCorrect).set_Text("AutoCorrect");
			((ButtonBase)cmdGrazeSitesAutoCorrect).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeSitesAutoCorrect).add_Click((EventHandler)cmdGrazeSitesAutoCorrect_Click);
			((Control)cmdGrazesGetTelescopes).set_Location(new Point(15, 25));
			((Control)cmdGrazesGetTelescopes).set_Name("cmdGrazesGetTelescopes");
			((Control)cmdGrazesGetTelescopes).set_Size(new Size(72, 20));
			((Control)cmdGrazesGetTelescopes).set_TabIndex(0);
			((Control)cmdGrazesGetTelescopes).set_Text("Get events");
			((ButtonBase)cmdGrazesGetTelescopes).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazesGetTelescopes).add_Click((EventHandler)cmdGrazesGetTelescopes_Click);
			((Control)grpGrazeDR).get_Controls().Add((Control)(object)cmbGrazeEvents);
			((Control)grpGrazeDR).get_Controls().Add((Control)(object)cmdGrazesSetEvent);
			((Control)grpGrazeDR).set_Location(new Point(994, 193));
			((Control)grpGrazeDR).set_Name("grpGrazeDR");
			((Control)grpGrazeDR).set_Size(new Size(260, 185));
			((Control)grpGrazeDR).set_TabIndex(30);
			grpGrazeDR.set_TabStop(false);
			((Control)grpGrazeDR).set_Text("Set Event code");
			((ListControl)cmbGrazeEvents).set_FormattingEnabled(true);
			cmbGrazeEvents.get_Items().AddRange(new object[9] { "   unspecified", "D  Disappear", "R  Reappear", "B  Blink", "F  Flash", "S  Start", "E  End", "M  Miss", "O  Other" });
			((Control)cmbGrazeEvents).set_Location(new Point(19, 70));
			((Control)cmbGrazeEvents).set_Name("cmbGrazeEvents");
			((Control)cmbGrazeEvents).set_Size(new Size(120, 21));
			((Control)cmbGrazeEvents).set_TabIndex(1);
			((Control)cmdGrazesSetEvent).set_Location(new Point(20, 25));
			((Control)cmdGrazesSetEvent).set_Name("cmdGrazesSetEvent");
			((Control)cmdGrazesSetEvent).set_Size(new Size(150, 34));
			((Control)cmdGrazesSetEvent).set_TabIndex(0);
			((Control)cmdGrazesSetEvent).set_Text("For checked events, set\r\nEvent to following");
			((ButtonBase)cmdGrazesSetEvent).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazesSetEvent).add_Click((EventHandler)cmdGrazesSetEvent_Click);
			((Control)chkUseGrazeIDonly).set_AutoSize(true);
			((Control)chkUseGrazeIDonly).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkUseGrazeIDonly).set_Location(new Point(11, 89));
			((Control)chkUseGrazeIDonly).set_Name("chkUseGrazeIDonly");
			((Control)chkUseGrazeIDonly).set_Size(new Size(132, 17));
			((Control)chkUseGrazeIDonly).set_TabIndex(28);
			((Control)chkUseGrazeIDonly).set_Text("Only load grazes by ID");
			((ButtonBase)chkUseGrazeIDonly).set_UseVisualStyleBackColor(true);
			((Control)grp2ndLimit).get_Controls().Add((Control)(object)cmdClear2ndLimit);
			((Control)grp2ndLimit).get_Controls().Add((Control)(object)cmdSet2ndLimit);
			((Control)grp2ndLimit).set_Location(new Point(980, 173));
			((Control)grp2ndLimit).set_Name("grp2ndLimit");
			((Control)grp2ndLimit).set_Size(new Size(179, 136));
			((Control)grp2ndLimit).set_TabIndex(27);
			grp2ndLimit.set_TabStop(false);
			((Control)grp2ndLimit).set_Text("Set 2nd Limit");
			((Control)grp2ndLimit).set_Visible(false);
			((Control)cmdClear2ndLimit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdClear2ndLimit).set_Location(new Point(18, 74));
			((Control)cmdClear2ndLimit).set_Name("cmdClear2ndLimit");
			((Control)cmdClear2ndLimit).set_Size(new Size(149, 35));
			((Control)cmdClear2ndLimit).set_TabIndex(9);
			((Control)cmdClear2ndLimit).set_Text("Clear 2nd-limit flag on checked");
			((ButtonBase)cmdClear2ndLimit).set_UseVisualStyleBackColor(true);
			((Control)cmdClear2ndLimit).add_Click((EventHandler)cmdClear2ndLimit_Click);
			((Control)cmdSet2ndLimit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSet2ndLimit).set_Location(new Point(18, 27));
			((Control)cmdSet2ndLimit).set_Name("cmdSet2ndLimit");
			((Control)cmdSet2ndLimit).set_Size(new Size(149, 35));
			((Control)cmdSet2ndLimit).set_TabIndex(8);
			((Control)cmdSet2ndLimit).set_Text("Set 2nd-limit flag on checked");
			((ButtonBase)cmdSet2ndLimit).set_UseVisualStyleBackColor(true);
			((Control)cmdSet2ndLimit).add_Click((EventHandler)cmdSet2ndLimit_Click);
			((Control)grpDatums).get_Controls().Add((Control)(object)cmdSetSomaVerticaltoMSL);
			((Control)grpDatums).get_Controls().Add((Control)(object)label41);
			((Control)grpDatums).get_Controls().Add((Control)(object)cmbHorizontalDatum2);
			((Control)grpDatums).get_Controls().Add((Control)(object)cmdSetSomaVerticalToEllipsoid);
			((Control)grpDatums).get_Controls().Add((Control)(object)cmdFindSomaEllipsoid);
			((Control)grpDatums).get_Controls().Add((Control)(object)cmdSetSelectedToDatum);
			((Control)grpDatums).get_Controls().Add((Control)(object)cmdFindZeroDatums);
			((Control)grpDatums).set_Location(new Point(938, 129));
			((Control)grpDatums).set_Name("grpDatums");
			((Control)grpDatums).set_Size(new Size(486, 202));
			((Control)grpDatums).set_TabIndex(26);
			grpDatums.set_TabStop(false);
			((Control)grpDatums).set_Text("Datums (generally)");
			((Control)cmdSetSomaVerticaltoMSL).set_Enabled(false);
			((Control)cmdSetSomaVerticaltoMSL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetSomaVerticaltoMSL).set_Location(new Point(326, 132));
			((Control)cmdSetSomaVerticaltoMSL).set_Name("cmdSetSomaVerticaltoMSL");
			((Control)cmdSetSomaVerticaltoMSL).set_Size(new Size(139, 39));
			((Control)cmdSetSomaVerticaltoMSL).set_TabIndex(36);
			((Control)cmdSetSomaVerticaltoMSL).set_Text("Set vertical Datum on\r\n checked to 'M - MSL'");
			((ButtonBase)cmdSetSomaVerticaltoMSL).set_UseVisualStyleBackColor(true);
			((Control)cmdSetSomaVerticaltoMSL).add_Click((EventHandler)cmdSetSomaVerticaltoMSL_Click);
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(18, 106));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(85, 13));
			((Control)label41).set_TabIndex(35);
			((Control)label41).set_Text("Horizontal datum");
			((Control)cmbHorizontalDatum2).set_Enabled(false);
			cmbHorizontalDatum2.set_FlatStyle((FlatStyle)0);
			((ListControl)cmbHorizontalDatum2).set_FormattingEnabled(true);
			cmbHorizontalDatum2.get_Items().AddRange(new object[79]
			{
				" 0  unknown (WGS84 used)", " 1  RGO code 1", " 2  RGO code 2", " 3  RGO code 3", " 4  RGO code 4", " 5  RGO code 5", "10  GoogleEarth (WGS84)", "12  Christmas Island Astro 1967", "13  Chua Astro (Brazil Geodetic)", "14  Corrego Alegre (Brazil)",
				"15  Easter Island Astro 1967", "16  European 1950  ", "17  Graciosa Island (Azores)", "18  Gizo, Provisional DOS  ", "19  Guam", "20  Heard Astro 1969", "21  Iben Astro, Navy 1947 (Truk) ", "22  Indian", "23  Isla Socorro Astro", "24  Johnston Island 1961 ",
				"25  Kusaie Astro 1962,1965", "26  Luzon 1911 (Philippines) ", "27  Midway Astro 1961", "28  New Zealand 1949 ", "29  North American 1927  ", "30 *Cape Canaveral ", "31 *White Sands", "32  Old Bavarian ", "33  Old Hawaiian ", "34  Ordnance Survey of Great Britain 1936 ",
				"35  Pico de las Nieves (Canaries) ", "36  Pitcairn Island Astro ", "37  Potsdam", "38  Provisional South American 1956", "39  Provisional South Chile 1963  ", "40  Pulkovo 1942  ", "41  South American 1969", "42  Southeast Island (Mahe)", "43  South Georgia Astro", "44  Swallow Islands (Solomons)",
				"45  Tananarive", "46  Tokyo ", "47  Tristan Astro 1968", "48  Viti Levu 1916 (Fiji) ", "49  Wake Island Astro 1952", "50  Yof Astro 1967 (Dakar)", "51  Palmer Astro 1969 (Antarctica)", "52  Efate (New Hebrides)", "53  Marcus Island 1965", "54  Canton Astro 1966  ",
				"56  Yap Island", "58  Kourou (French Guiana)", "59  Ordnance Survey of Great Britain 1970 ", "60  Qornoq (Greenland)", "61  Adindan (Ethiopia) ", "62  American Samoa 1962", "63  Arc-Cape (South Africa)", "64  Argentine  ", "65  Ascension Island 1958  ", "66  Australian Geodetic",
				"67  Bermuda 1957", "68  Berne 1898 ", "69  Betio Island 1966  ", "70  Camp Area Astro 1961-62 USGS", "71  Batavia (Java)", "72  Palestine (Israel,Jordan)", "73  Hermannskogel (Austria,Czech.,Yugoslavia)", "74  Kandawala (Ceylon)", "80  ETRS89 (European Terrestial Reference System)", "81  Amersfoort 1885 (Netherlands)",
				"82  NAD83/NAD1983 (=WGS84)", "84  WGS84", "85  JGD2000 (=WGS84)", "86  GDA94 (=WGS84)", "87  NZGD2000 (=WGS84)", "88  NGRF2000 (=WGS84)", "89  KDG2000 (=WGS84)", "90  Hartebeesthoek94 (=WGS84)", "91  TWD94 (=WGS84)"
			});
			((Control)cmbHorizontalDatum2).set_Location(new Point(12, 122));
			((Control)cmbHorizontalDatum2).set_Name("cmbHorizontalDatum2");
			((Control)cmbHorizontalDatum2).set_Size(new Size(266, 21));
			((Control)cmbHorizontalDatum2).set_TabIndex(34);
			((Control)cmdSetSomaVerticalToEllipsoid).set_Enabled(false);
			((Control)cmdSetSomaVerticalToEllipsoid).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetSomaVerticalToEllipsoid).set_Location(new Point(327, 82));
			((Control)cmdSetSomaVerticalToEllipsoid).set_Name("cmdSetSomaVerticalToEllipsoid");
			((Control)cmdSetSomaVerticalToEllipsoid).set_Size(new Size(139, 39));
			((Control)cmdSetSomaVerticalToEllipsoid).set_TabIndex(11);
			((Control)cmdSetSomaVerticalToEllipsoid).set_Text("Set vertical Datum on\r\n checked to 'E - Ellipsoid'");
			((ButtonBase)cmdSetSomaVerticalToEllipsoid).set_UseVisualStyleBackColor(true);
			((Control)cmdSetSomaVerticalToEllipsoid).add_Click((EventHandler)cmdSetSomaVerticalToEllipsoid_Click);
			((Control)cmdFindSomaEllipsoid).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFindSomaEllipsoid).set_Location(new Point(330, 30));
			((Control)cmdFindSomaEllipsoid).set_Name("cmdFindSomaEllipsoid");
			((Control)cmdFindSomaEllipsoid).set_Size(new Size(135, 40));
			((Control)cmdFindSomaEllipsoid).set_TabIndex(10);
			((Control)cmdFindSomaEllipsoid).set_Text("Find Soma events - Ellipsoid");
			((ButtonBase)cmdFindSomaEllipsoid).set_UseVisualStyleBackColor(true);
			((Control)cmdFindSomaEllipsoid).add_Click((EventHandler)cmdFindSomaEllipsoid_Click);
			((Control)cmdSetSelectedToDatum).set_Enabled(false);
			((Control)cmdSetSelectedToDatum).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetSelectedToDatum).set_Location(new Point(20, 57));
			((Control)cmdSetSelectedToDatum).set_Name("cmdSetSelectedToDatum");
			((Control)cmdSetSelectedToDatum).set_Size(new Size(118, 35));
			((Control)cmdSetSelectedToDatum).set_TabIndex(9);
			((Control)cmdSetSelectedToDatum).set_Text("Set checked to selected datum");
			((ButtonBase)cmdSetSelectedToDatum).set_UseVisualStyleBackColor(true);
			((Control)cmdSetSelectedToDatum).add_Click((EventHandler)cmdSetSelectedToDatum_Click);
			((Control)cmdFindZeroDatums).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFindZeroDatums).set_Location(new Point(20, 27));
			((Control)cmdFindZeroDatums).set_Name("cmdFindZeroDatums");
			((Control)cmdFindZeroDatums).set_Size(new Size(118, 26));
			((Control)cmdFindZeroDatums).set_TabIndex(8);
			((Control)cmdFindZeroDatums).set_Text("Find 'zero' datums");
			((ButtonBase)cmdFindZeroDatums).set_UseVisualStyleBackColor(true);
			((Control)cmdFindZeroDatums).add_Click((EventHandler)cmdFindZeroDatums_Click);
			((Control)cmdTagAllGrazeEvents).set_Location(new Point(956, 173));
			((Control)cmdTagAllGrazeEvents).set_Name("cmdTagAllGrazeEvents");
			((Control)cmdTagAllGrazeEvents).set_Size(new Size(115, 31));
			((Control)cmdTagAllGrazeEvents).set_TabIndex(25);
			((Control)cmdTagAllGrazeEvents).set_Text("Tag all graze events");
			((ButtonBase)cmdTagAllGrazeEvents).set_UseVisualStyleBackColor(true);
			((Control)cmdTagAllGrazeEvents).add_Click((EventHandler)cmdTagAllGrazeEvents_Click);
			((Control)lblGrazeFile).set_AutoSize(true);
			((Control)lblGrazeFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblGrazeFile).set_Location(new Point(1, 43));
			((Control)lblGrazeFile).set_Name("lblGrazeFile");
			((Control)lblGrazeFile).set_Size(new Size(69, 13));
			((Control)lblGrazeFile).set_TabIndex(24);
			((Control)lblGrazeFile).set_Text("Current file");
			((Control)grpGrazeRenumber).get_Controls().Add((Control)(object)cmdRenumberGrazes);
			((Control)grpGrazeRenumber).set_Location(new Point(930, 65));
			((Control)grpGrazeRenumber).set_Name("grpGrazeRenumber");
			((Control)grpGrazeRenumber).set_Size(new Size(152, 92));
			((Control)grpGrazeRenumber).set_TabIndex(23);
			grpGrazeRenumber.set_TabStop(false);
			((Control)grpGrazeRenumber).set_Text("Renumber grazes");
			((Control)cmdRenumberGrazes).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdRenumberGrazes).set_Location(new Point(15, 17));
			((Control)cmdRenumberGrazes).set_Name("cmdRenumberGrazes");
			((Control)cmdRenumberGrazes).set_Size(new Size(127, 48));
			((Control)cmdRenumberGrazes).set_TabIndex(22);
			((Control)cmdRenumberGrazes).set_Text("Re-number grazes in the file");
			((ButtonBase)cmdRenumberGrazes).set_UseVisualStyleBackColor(true);
			((Control)cmdRenumberGrazes).add_Click((EventHandler)cmdRenumberGrazes_Click);
			((Control)cmdClearDuplicatteResidualFlags).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdClearDuplicatteResidualFlags).set_Location(new Point(553, 169));
			((Control)cmdClearDuplicatteResidualFlags).set_Name("cmdClearDuplicatteResidualFlags");
			((Control)cmdClearDuplicatteResidualFlags).set_Size(new Size(127, 48));
			((Control)cmdClearDuplicatteResidualFlags).set_TabIndex(21);
			((Control)cmdClearDuplicatteResidualFlags).set_Text("Clear Duplicate and \r\nLarge Residual flags\r\nfrom checked");
			((ButtonBase)cmdClearDuplicatteResidualFlags).set_UseVisualStyleBackColor(true);
			((Control)cmdClearDuplicatteResidualFlags).add_Click((EventHandler)cmdClearDuplicatteResidualFlags_Click);
			((Control)cmdRecordGrazeAsEdited).set_Location(new Point(18, 114));
			((Control)cmdRecordGrazeAsEdited).set_Name("cmdRecordGrazeAsEdited");
			((Control)cmdRecordGrazeAsEdited).set_Size(new Size(136, 24));
			((Control)cmdRecordGrazeAsEdited).set_TabIndex(20);
			((Control)cmdRecordGrazeAsEdited).set_Text("Record current as edited");
			((ButtonBase)cmdRecordGrazeAsEdited).set_UseVisualStyleBackColor(true);
			((Control)cmdRecordGrazeAsEdited).add_Click((EventHandler)cmdRecordGrazeAsEdited_Click);
			((Control)lblSource).set_AutoSize(true);
			((Control)lblSource).set_Location(new Point(912, 30));
			((Control)lblSource).set_Name("lblSource");
			((Control)lblSource).set_Size(new Size(41, 13));
			((Control)lblSource).set_TabIndex(19);
			((Control)lblSource).set_Text("Source");
			((Control)grpDuplicateGrazeEvents).get_Controls().Add((Control)(object)cmdGrazeSetDuplicateToKeep);
			((Control)grpDuplicateGrazeEvents).get_Controls().Add((Control)(object)cmdGrazeSetDuplicateToDelete);
			((Control)grpDuplicateGrazeEvents).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpDuplicateGrazeEvents).set_Location(new Point(930, 39));
			((Control)grpDuplicateGrazeEvents).set_Name("grpDuplicateGrazeEvents");
			((Control)grpDuplicateGrazeEvents).set_Size(new Size(166, 114));
			((Control)grpDuplicateGrazeEvents).set_TabIndex(18);
			grpDuplicateGrazeEvents.set_TabStop(false);
			((Control)grpDuplicateGrazeEvents).set_Text("Duplicate events");
			((Control)cmdGrazeSetDuplicateToKeep).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGrazeSetDuplicateToKeep).set_Location(new Point(9, 71));
			((Control)cmdGrazeSetDuplicateToKeep).set_Name("cmdGrazeSetDuplicateToKeep");
			((Control)cmdGrazeSetDuplicateToKeep).set_Size(new Size(151, 35));
			((Control)cmdGrazeSetDuplicateToKeep).set_TabIndex(8);
			((Control)cmdGrazeSetDuplicateToKeep).set_Text("Set checked as Duplicates, but not to be deleted [K]");
			((ButtonBase)cmdGrazeSetDuplicateToKeep).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeSetDuplicateToKeep).add_Click((EventHandler)cmdGrazeSetDuplicateToKeep_Click);
			((Control)cmdGrazeSetDuplicateToDelete).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGrazeSetDuplicateToDelete).set_Location(new Point(8, 20));
			((Control)cmdGrazeSetDuplicateToDelete).set_Name("cmdGrazeSetDuplicateToDelete");
			((Control)cmdGrazeSetDuplicateToDelete).set_Size(new Size(151, 35));
			((Control)cmdGrazeSetDuplicateToDelete).set_TabIndex(7);
			((Control)cmdGrazeSetDuplicateToDelete).set_Text("Set checked as Duplicates that can be deleted [D]");
			((ButtonBase)cmdGrazeSetDuplicateToDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeSetDuplicateToDelete).add_Click((EventHandler)cmdGrazeSetDuplicateToDelete_Click);
			((Control)cmdGrazeEventsInEditor_Valid).set_Location(new Point(18, 157));
			((Control)cmdGrazeEventsInEditor_Valid).set_Name("cmdGrazeEventsInEditor_Valid");
			((Control)cmdGrazeEventsInEditor_Valid).set_Size(new Size(189, 21));
			((Control)cmdGrazeEventsInEditor_Valid).set_TabIndex(17);
			((Control)cmdGrazeEventsInEditor_Valid).set_Text("Place VALID graze events in Editor");
			((ButtonBase)cmdGrazeEventsInEditor_Valid).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeEventsInEditor_Valid).add_Click((EventHandler)cmdGrazeEventsInEditor_Valid_Click);
			((Control)cmdGrazeEventsInEditor_No_StartEnd).set_Location(new Point(18, 190));
			((Control)cmdGrazeEventsInEditor_No_StartEnd).set_Name("cmdGrazeEventsInEditor_No_StartEnd");
			((Control)cmdGrazeEventsInEditor_No_StartEnd).set_Size(new Size(189, 21));
			((Control)cmdGrazeEventsInEditor_No_StartEnd).set_TabIndex(16);
			((Control)cmdGrazeEventsInEditor_No_StartEnd).set_Text("Graze events in Editor, no Start/End");
			((ButtonBase)cmdGrazeEventsInEditor_No_StartEnd).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeEventsInEditor_No_StartEnd).add_Click((EventHandler)cmdGrazeEventsInEditor_No_StartEnd_Click);
			((Control)groupBox11).get_Controls().Add((Control)(object)optGrazeEventDuration);
			((Control)groupBox11).get_Controls().Add((Control)(object)optGrazeSetDoubles);
			((Control)groupBox11).get_Controls().Add((Control)(object)optGrazeSites);
			((Control)groupBox11).get_Controls().Add((Control)(object)optDatums);
			((Control)groupBox11).get_Controls().Add((Control)(object)optTelescopeCodes);
			((Control)groupBox11).get_Controls().Add((Control)(object)optGrazeEventCode);
			((Control)groupBox11).get_Controls().Add((Control)(object)optGrazeSiteDetails);
			((Control)groupBox11).get_Controls().Add((Control)(object)optMarkAs2ndLimit);
			((Control)groupBox11).get_Controls().Add((Control)(object)optTagGrazes);
			((Control)groupBox11).get_Controls().Add((Control)(object)optGraze_NullSecs);
			((Control)groupBox11).get_Controls().Add((Control)(object)optGraze_RenumberGrazes);
			((Control)groupBox11).get_Controls().Add((Control)(object)optGraze_Duplicates);
			((Control)groupBox11).get_Controls().Add((Control)(object)optGraze_LargeResiduals);
			((Control)groupBox11).get_Controls().Add((Control)(object)optGraze_Grazeflag);
			((Control)groupBox11).set_Location(new Point(230, 39));
			((Control)groupBox11).set_Name("groupBox11");
			((Control)groupBox11).set_Size(new Size(114, 229));
			((Control)groupBox11).set_TabIndex(15);
			groupBox11.set_TabStop(false);
			((Control)groupBox11).set_Text("Edit functions");
			((Control)optGrazeEventDuration).set_AutoSize(true);
			((Control)optGrazeEventDuration).set_Location(new Point(6, 143));
			((Control)optGrazeEventDuration).set_Name("optGrazeEventDuration");
			((Control)optGrazeEventDuration).set_Size(new Size(112, 17));
			((Control)optGrazeEventDuration).set_TabIndex(8);
			((Control)optGrazeEventDuration).set_Text("Set event duration");
			((ButtonBase)optGrazeEventDuration).set_UseVisualStyleBackColor(true);
			optGrazeEventDuration.add_CheckedChanged((EventHandler)optGrazeEventDuration_CheckedChanged);
			((Control)optGrazeSetDoubles).set_AutoSize(true);
			((Control)optGrazeSetDoubles).set_Location(new Point(6, 127));
			((Control)optGrazeSetDoubles).set_Name("optGrazeSetDoubles");
			((Control)optGrazeSetDoubles).set_Size(new Size(96, 17));
			((Control)optGrazeSetDoubles).set_TabIndex(7);
			((Control)optGrazeSetDoubles).set_Text("Set double info");
			((ButtonBase)optGrazeSetDoubles).set_UseVisualStyleBackColor(true);
			optGrazeSetDoubles.add_CheckedChanged((EventHandler)optGrazeSetDoubles_CheckedChanged);
			((Control)optGrazeSites).set_AutoSize(true);
			((Control)optGrazeSites).set_Location(new Point(6, 79));
			((Control)optGrazeSites).set_Name("optGrazeSites");
			((Control)optGrazeSites).set_Size(new Size(104, 17));
			((Control)optGrazeSites).set_TabIndex(4);
			((Control)optGrazeSites).set_Text("Site co-ordinates");
			((ButtonBase)optGrazeSites).set_UseVisualStyleBackColor(true);
			optGrazeSites.add_CheckedChanged((EventHandler)optGrazeSites_CheckedChanged);
			((Control)optDatums).set_AutoSize(true);
			((Control)optDatums).set_Location(new Point(6, 205));
			((Control)optDatums).set_Name("optDatums");
			((Control)optDatums).set_Size(new Size(112, 17));
			((Control)optDatums).set_TabIndex(11);
			((Control)optDatums).set_Text("Datums (generally)");
			((ButtonBase)optDatums).set_UseVisualStyleBackColor(true);
			optDatums.add_CheckedChanged((EventHandler)optDatums_CheckedChanged);
			((Control)optTelescopeCodes).set_AutoSize(true);
			((Control)optTelescopeCodes).set_Enabled(false);
			((Control)optTelescopeCodes).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optTelescopeCodes).set_Location(new Point(6, 211));
			((Control)optTelescopeCodes).set_Name("optTelescopeCodes");
			((Control)optTelescopeCodes).set_Size(new Size(92, 16));
			((Control)optTelescopeCodes).set_TabIndex(0);
			((Control)optTelescopeCodes).set_Text("Telescope codes");
			((ButtonBase)optTelescopeCodes).set_UseVisualStyleBackColor(true);
			((Control)optTelescopeCodes).set_Visible(false);
			optTelescopeCodes.add_CheckedChanged((EventHandler)optTelescopeCodes_CheckedChanged);
			((Control)optGrazeEventCode).set_AutoSize(true);
			((Control)optGrazeEventCode).set_Location(new Point(6, 95));
			((Control)optGrazeEventCode).set_Name("optGrazeEventCode");
			((Control)optGrazeEventCode).set_Size(new Size(98, 17));
			((Control)optGrazeEventCode).set_TabIndex(5);
			((Control)optGrazeEventCode).set_Text("Set event code");
			((ButtonBase)optGrazeEventCode).set_UseVisualStyleBackColor(true);
			optGrazeEventCode.add_CheckedChanged((EventHandler)optGrazeEventCode_CheckedChanged);
			((Control)optGrazeSiteDetails).set_AutoSize(true);
			((Control)optGrazeSiteDetails).set_Location(new Point(6, 63));
			((Control)optGrazeSiteDetails).set_Name("optGrazeSiteDetails");
			((Control)optGrazeSiteDetails).set_Size(new Size(95, 17));
			((Control)optGrazeSiteDetails).set_TabIndex(3);
			((Control)optGrazeSiteDetails).set_Text("Scope, Names");
			((ButtonBase)optGrazeSiteDetails).set_UseVisualStyleBackColor(true);
			optGrazeSiteDetails.add_CheckedChanged((EventHandler)optGrazeSiteDetails_CheckedChanged);
			((Control)optMarkAs2ndLimit).set_AutoSize(true);
			((Control)optMarkAs2ndLimit).set_Location(new Point(6, 46));
			((Control)optMarkAs2ndLimit).set_Name("optMarkAs2ndLimit");
			((Control)optMarkAs2ndLimit).set_Size(new Size(104, 17));
			((Control)optMarkAs2ndLimit).set_TabIndex(2);
			((Control)optMarkAs2ndLimit).set_Text("Mark as 2nd limit");
			((ButtonBase)optMarkAs2ndLimit).set_UseVisualStyleBackColor(true);
			optMarkAs2ndLimit.add_CheckedChanged((EventHandler)optMarkAs2ndLimit_CheckedChanged);
			((Control)optTagGrazes).set_AutoSize(true);
			((Control)optTagGrazes).set_Location(new Point(6, 190));
			((Control)optTagGrazes).set_Name("optTagGrazes");
			((Control)optTagGrazes).set_Size(new Size(91, 17));
			((Control)optTagGrazes).set_TabIndex(10);
			((Control)optTagGrazes).set_Text("Tag all grazes");
			((ButtonBase)optTagGrazes).set_UseVisualStyleBackColor(true);
			optTagGrazes.add_CheckedChanged((EventHandler)optTagGrazes_CheckedChanged);
			((Control)optGraze_NullSecs).set_AutoSize(true);
			((Control)optGraze_NullSecs).set_Enabled(false);
			((Control)optGraze_NullSecs).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optGraze_NullSecs).set_Location(new Point(64, 197));
			((Control)optGraze_NullSecs).set_Name("optGraze_NullSecs");
			((Control)optGraze_NullSecs).set_Size(new Size(76, 16));
			((Control)optGraze_NullSecs).set_TabIndex(13);
			((Control)optGraze_NullSecs).set_Text("Null seconds");
			((ButtonBase)optGraze_NullSecs).set_UseVisualStyleBackColor(true);
			((Control)optGraze_NullSecs).set_Visible(false);
			optGraze_NullSecs.add_CheckedChanged((EventHandler)optGraze_NullSecs_CheckedChanged);
			((Control)optGraze_RenumberGrazes).set_AutoSize(true);
			((Control)optGraze_RenumberGrazes).set_Location(new Point(6, 175));
			((Control)optGraze_RenumberGrazes).set_Name("optGraze_RenumberGrazes");
			((Control)optGraze_RenumberGrazes).set_Size(new Size(110, 17));
			((Control)optGraze_RenumberGrazes).set_TabIndex(9);
			((Control)optGraze_RenumberGrazes).set_Text("ReNumber grazes");
			((ButtonBase)optGraze_RenumberGrazes).set_UseVisualStyleBackColor(true);
			optGraze_RenumberGrazes.add_CheckedChanged((EventHandler)optGraze_RenumberGrazes_CheckedChanged);
			((Control)optGraze_Duplicates).set_AutoSize(true);
			((Control)optGraze_Duplicates).set_Location(new Point(6, 30));
			((Control)optGraze_Duplicates).set_Name("optGraze_Duplicates");
			((Control)optGraze_Duplicates).set_Size(new Size(105, 17));
			((Control)optGraze_Duplicates).set_TabIndex(1);
			((Control)optGraze_Duplicates).set_Text("Duplicate events");
			((ButtonBase)optGraze_Duplicates).set_UseVisualStyleBackColor(true);
			optGraze_Duplicates.add_CheckedChanged((EventHandler)optGraze_Duplicates_CheckedChanged);
			((Control)optGraze_LargeResiduals).set_AutoSize(true);
			optGraze_LargeResiduals.set_Checked(true);
			((Control)optGraze_LargeResiduals).set_Location(new Point(6, 14));
			((Control)optGraze_LargeResiduals).set_Name("optGraze_LargeResiduals");
			((Control)optGraze_LargeResiduals).set_Size(new Size(96, 17));
			((Control)optGraze_LargeResiduals).set_TabIndex(0);
			optGraze_LargeResiduals.set_TabStop(true);
			((Control)optGraze_LargeResiduals).set_Text("Large residuals");
			((ButtonBase)optGraze_LargeResiduals).set_UseVisualStyleBackColor(true);
			optGraze_LargeResiduals.add_CheckedChanged((EventHandler)optGraze_LargeResiduals_CheckedChanged);
			((Control)optGraze_Grazeflag).set_AutoSize(true);
			((Control)optGraze_Grazeflag).set_Location(new Point(6, 111));
			((Control)optGraze_Grazeflag).set_Name("optGraze_Grazeflag");
			((Control)optGraze_Grazeflag).set_Size(new Size(90, 17));
			((Control)optGraze_Grazeflag).set_TabIndex(6);
			((Control)optGraze_Grazeflag).set_Text("Set graze flag");
			((ButtonBase)optGraze_Grazeflag).set_UseVisualStyleBackColor(true);
			optGraze_Grazeflag.add_CheckedChanged((EventHandler)optGraze_Grazeflag_CheckedChanged);
			((Control)cmbGrazeList).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbGrazeList).set_FormattingEnabled(true);
			((Control)cmbGrazeList).set_Location(new Point(4, 67));
			((Control)cmbGrazeList).set_Name("cmbGrazeList");
			((Control)cmbGrazeList).set_Size(new Size(216, 22));
			((Control)cmbGrazeList).set_TabIndex(14);
			cmbGrazeList.add_SelectedIndexChanged((EventHandler)cmbGrazeList_SelectedIndexChanged);
			((Control)cmdSelectArchiveForGrazeEdit).set_Location(new Point(41, 8));
			((Control)cmdSelectArchiveForGrazeEdit).set_Name("cmdSelectArchiveForGrazeEdit");
			((Control)cmdSelectArchiveForGrazeEdit).set_Size(new Size(143, 32));
			((Control)cmdSelectArchiveForGrazeEdit).set_TabIndex(13);
			((Control)cmdSelectArchiveForGrazeEdit).set_Text("Select Archive file to edit");
			((ButtonBase)cmdSelectArchiveForGrazeEdit).set_UseVisualStyleBackColor(true);
			((Control)cmdSelectArchiveForGrazeEdit).add_Click((EventHandler)cmdSelectArchiveForGrazeEdit_Click);
			((Control)grpLargeResiduals).get_Controls().Add((Control)(object)cmdSetFlagLargeResiduals);
			((Control)grpLargeResiduals).get_Controls().Add((Control)(object)label39);
			((Control)grpLargeResiduals).get_Controls().Add((Control)(object)label38);
			((Control)grpLargeResiduals).get_Controls().Add((Control)(object)updnSmoothTop);
			((Control)grpLargeResiduals).get_Controls().Add((Control)(object)updnSmoothBottom);
			((Control)grpLargeResiduals).get_Controls().Add((Control)(object)label37);
			((Control)grpLargeResiduals).get_Controls().Add((Control)(object)updnLimbCorrectedResidual);
			((Control)grpLargeResiduals).get_Controls().Add((Control)(object)optSmoothResidual);
			((Control)grpLargeResiduals).get_Controls().Add((Control)(object)optLimbResidual);
			((Control)grpLargeResiduals).get_Controls().Add((Control)(object)cmdMarkLargeResiduals);
			((Control)grpLargeResiduals).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpLargeResiduals).set_Location(new Point(912, 39));
			((Control)grpLargeResiduals).set_Name("grpLargeResiduals");
			((Control)grpLargeResiduals).set_Size(new Size(192, 183));
			((Control)grpLargeResiduals).set_TabIndex(12);
			grpLargeResiduals.set_TabStop(false);
			((Control)grpLargeResiduals).set_Text("Large residuals");
			((Control)cmdSetFlagLargeResiduals).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetFlagLargeResiduals).set_Location(new Point(31, 143));
			((Control)cmdSetFlagLargeResiduals).set_Name("cmdSetFlagLargeResiduals");
			((Control)cmdSetFlagLargeResiduals).set_Size(new Size(131, 39));
			((Control)cmdSetFlagLargeResiduals).set_TabIndex(19);
			((Control)cmdSetFlagLargeResiduals).set_Text("Set checked as invalid observations [I]");
			((ButtonBase)cmdSetFlagLargeResiduals).set_UseVisualStyleBackColor(true);
			((Control)cmdSetFlagLargeResiduals).add_Click((EventHandler)cmdSetFlagLargeResiduals_Click);
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(26, 78));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(13, 13));
			((Control)label39).set_TabIndex(18);
			((Control)label39).set_Text("<");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label38).set_Location(new Point(94, 78));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(32, 13));
			((Control)label38).set_TabIndex(17);
			((Control)label38).set_Text("OR >");
			updnSmoothTop.set_DecimalPlaces(1);
			((Control)updnSmoothTop).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnSmoothTop.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnSmoothTop).set_Location(new Point(126, 75));
			updnSmoothTop.set_Maximum(new decimal(new int[4] { 35, 0, 0, 65536 }));
			updnSmoothTop.set_Minimum(new decimal(new int[4] { 25, 0, 0, -2147418112 }));
			((Control)updnSmoothTop).set_Name("updnSmoothTop");
			((Control)updnSmoothTop).set_Size(new Size(43, 20));
			((Control)updnSmoothTop).set_TabIndex(16);
			updnSmoothTop.set_Value(new decimal(new int[4] { 10, 0, 0, 65536 }));
			updnSmoothBottom.set_DecimalPlaces(1);
			((Control)updnSmoothBottom).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnSmoothBottom.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnSmoothBottom).set_Location(new Point(39, 75));
			updnSmoothBottom.set_Maximum(new decimal(new int[4] { 25, 0, 0, 65536 }));
			updnSmoothBottom.set_Minimum(new decimal(new int[4] { 35, 0, 0, -2147418112 }));
			((Control)updnSmoothBottom).set_Name("updnSmoothBottom");
			((Control)updnSmoothBottom).set_Size(new Size(43, 20));
			((Control)updnSmoothBottom).set_TabIndex(15);
			updnSmoothBottom.set_Value(new decimal(new int[4] { 10, 0, 0, -2147418112 }));
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(53, 40));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(31, 17));
			((Control)label37).set_TabIndex(14);
			((Control)label37).set_Text("OR");
			updnLimbCorrectedResidual.set_DecimalPlaces(1);
			((Control)updnLimbCorrectedResidual).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnLimbCorrectedResidual.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnLimbCorrectedResidual).set_Location(new Point(127, 19));
			updnLimbCorrectedResidual.set_Maximum(new decimal(new int[4] { 35, 0, 0, 65536 }));
			updnLimbCorrectedResidual.set_Minimum(new decimal(new int[4] { 10, 0, 0, 65536 }));
			((Control)updnLimbCorrectedResidual).set_Name("updnLimbCorrectedResidual");
			((Control)updnLimbCorrectedResidual).set_Size(new Size(43, 20));
			((Control)updnLimbCorrectedResidual).set_TabIndex(11);
			updnLimbCorrectedResidual.set_Value(new decimal(new int[4] { 10, 0, 0, 65536 }));
			((Control)optSmoothResidual).set_AutoSize(true);
			((Control)optSmoothResidual).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSmoothResidual).set_Location(new Point(6, 55));
			((Control)optSmoothResidual).set_Name("optSmoothResidual");
			((Control)optSmoothResidual).set_Size(new Size(121, 17));
			((Control)optSmoothResidual).set_TabIndex(13);
			optSmoothResidual.set_TabStop(true);
			((Control)optSmoothResidual).set_Text("Smooth limb residual");
			((ButtonBase)optSmoothResidual).set_UseVisualStyleBackColor(true);
			((Control)optLimbResidual).set_AutoSize(true);
			optLimbResidual.set_Checked(true);
			((Control)optLimbResidual).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optLimbResidual).set_Location(new Point(6, 19));
			((Control)optLimbResidual).set_Name("optLimbResidual");
			((Control)optLimbResidual).set_Size(new Size(94, 17));
			((Control)optLimbResidual).set_TabIndex(12);
			optLimbResidual.set_TabStop(true);
			((Control)optLimbResidual).set_Text("limb residual  >");
			((ButtonBase)optLimbResidual).set_UseVisualStyleBackColor(true);
			((Control)cmdMarkLargeResiduals).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMarkLargeResiduals).set_Location(new Point(31, 107));
			((Control)cmdMarkLargeResiduals).set_Name("cmdMarkLargeResiduals");
			((Control)cmdMarkLargeResiduals).set_Size(new Size(131, 25));
			((Control)cmdMarkLargeResiduals).set_TabIndex(10);
			((Control)cmdMarkLargeResiduals).set_Text("Find && check");
			((ButtonBase)cmdMarkLargeResiduals).set_UseVisualStyleBackColor(true);
			((Control)cmdMarkLargeResiduals).add_Click((EventHandler)cmdMarkLargeResiduals_Click);
			((Control)cmdGrazeEventsToEditor).set_Location(new Point(18, 222));
			((Control)cmdGrazeEventsToEditor).set_Name("cmdGrazeEventsToEditor");
			((Control)cmdGrazeEventsToEditor).set_Size(new Size(189, 21));
			((Control)cmdGrazeEventsToEditor).set_TabIndex(9);
			((Control)cmdGrazeEventsToEditor).set_Text("Place ALL graze events in Editor");
			((ButtonBase)cmdGrazeEventsToEditor).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazeEventsToEditor).add_Click((EventHandler)cmdGrazeEventsToEditor_Click);
			((Control)grpNullSeconds).get_Controls().Add((Control)(object)cmdSetZeroSecs);
			((Control)grpNullSeconds).get_Controls().Add((Control)(object)cmdFindBlankSecs);
			((Control)grpNullSeconds).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpNullSeconds).set_Location(new Point(960, 194));
			((Control)grpNullSeconds).set_Name("grpNullSeconds");
			((Control)grpNullSeconds).set_Size(new Size(136, 88));
			((Control)grpNullSeconds).set_TabIndex(8);
			grpNullSeconds.set_TabStop(false);
			((Control)grpNullSeconds).set_Text("Null seconds");
			((Control)cmdSetZeroSecs).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetZeroSecs).set_Location(new Point(8, 47));
			((Control)cmdSetZeroSecs).set_Name("cmdSetZeroSecs");
			((Control)cmdSetZeroSecs).set_Size(new Size(118, 35));
			((Control)cmdSetZeroSecs).set_TabIndex(7);
			((Control)cmdSetZeroSecs).set_Text("Set seconds on checked to ' 0.   '");
			((ButtonBase)cmdSetZeroSecs).set_UseVisualStyleBackColor(true);
			((Control)cmdSetZeroSecs).add_Click((EventHandler)cmdSetZeroSecs_Click);
			((Control)cmdFindBlankSecs).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFindBlankSecs).set_Location(new Point(8, 17));
			((Control)cmdFindBlankSecs).set_Name("cmdFindBlankSecs");
			((Control)cmdFindBlankSecs).set_Size(new Size(118, 26));
			((Control)cmdFindBlankSecs).set_TabIndex(6);
			((Control)cmdFindBlankSecs).set_Text("Find events && check");
			((ButtonBase)cmdFindBlankSecs).set_UseVisualStyleBackColor(true);
			((Control)cmdFindBlankSecs).add_Click((EventHandler)cmdFindBlankSecs_Click);
			((Control)grpGrazeFlag).get_Controls().Add((Control)(object)lblGrazeID);
			((Control)grpGrazeFlag).get_Controls().Add((Control)(object)cmdClearGrazeFlags);
			((Control)grpGrazeFlag).get_Controls().Add((Control)(object)cmdSetGrazeFlags);
			((Control)grpGrazeFlag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpGrazeFlag).set_Location(new Point(1001, 185));
			((Control)grpGrazeFlag).set_Name("grpGrazeFlag");
			((Control)grpGrazeFlag).set_Size(new Size(154, 95));
			((Control)grpGrazeFlag).set_TabIndex(7);
			grpGrazeFlag.set_TabStop(false);
			((Control)grpGrazeFlag).set_Text("Graze flag");
			((Control)lblGrazeID).set_AutoSize(true);
			((Control)lblGrazeID).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblGrazeID).set_Location(new Point(6, 78));
			((Control)lblGrazeID).set_Name("lblGrazeID");
			((Control)lblGrazeID).set_Size(new Size(27, 13));
			((Control)lblGrazeID).set_TabIndex(8);
			((Control)lblGrazeID).set_Text("ID =");
			((Control)cmdClearGrazeFlags).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdClearGrazeFlags).set_Location(new Point(3, 47));
			((Control)cmdClearGrazeFlags).set_Name("cmdClearGrazeFlags");
			((Control)cmdClearGrazeFlags).set_Size(new Size(149, 26));
			((Control)cmdClearGrazeFlags).set_TabIndex(7);
			((Control)cmdClearGrazeFlags).set_Text("Clear graze flag on checked");
			((ButtonBase)cmdClearGrazeFlags).set_UseVisualStyleBackColor(true);
			((Control)cmdClearGrazeFlags).add_Click((EventHandler)cmdClearGrazeFlags_Click);
			((Control)cmdSetGrazeFlags).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSetGrazeFlags).set_Location(new Point(3, 17));
			((Control)cmdSetGrazeFlags).set_Name("cmdSetGrazeFlags");
			((Control)cmdSetGrazeFlags).set_Size(new Size(149, 26));
			((Control)cmdSetGrazeFlags).set_TabIndex(6);
			((Control)cmdSetGrazeFlags).set_Text("Set graze flag on checked");
			((ButtonBase)cmdSetGrazeFlags).set_UseVisualStyleBackColor(true);
			((Control)cmdSetGrazeFlags).add_Click((EventHandler)cmdSetGrazeFlags_Click);
			((Control)chkAutoDisplay).set_AutoSize(true);
			chkAutoDisplay.set_Checked(Settings.Default.ArchiveGrazesAutoDisplay);
			((Control)chkAutoDisplay).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ArchiveGrazesAutoDisplay", true, (DataSourceUpdateMode)1));
			((Control)chkAutoDisplay).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAutoDisplay).set_Location(new Point(161, 92));
			((Control)chkAutoDisplay).set_Name("chkAutoDisplay");
			((Control)chkAutoDisplay).set_Size(new Size(73, 16));
			((Control)chkAutoDisplay).set_TabIndex(36);
			((Control)chkAutoDisplay).set_Text("auto display");
			((ButtonBase)chkAutoDisplay).set_UseVisualStyleBackColor(true);
			((ListControl)cmbObservationsSource).set_FormattingEnabled(true);
			cmbObservationsSource.get_Items().AddRange(new object[10] { "A    New observation reports", "H    RGO grazing occultations, OCR'd from RGO Bulletin\rI  ILOC computer records", "I    ILOC computer records", "M    Miscellaneous sources (grazes before 1960)", "N    S. Newcombe: Researches on the Motion of the Moon (APAE, 1878)", "O    grazes in the files of Occult", "P    re-processed ILOC reports after 2002", "R    RGO computer records, ordinary occultations ", "S    Graze reports collected by Mitsuru Soma - up to 2007.", "X    ILOC computer records after 2000, where no site code was allocated" });
			((Control)cmbObservationsSource).set_Location(new Point(137, 22));
			((Control)cmbObservationsSource).set_Name("cmbObservationsSource");
			((Control)cmbObservationsSource).set_Size(new Size(400, 21));
			((Control)cmbObservationsSource).set_TabIndex(68);
			((Control)panelCombine).get_Controls().Add((Control)(object)groupBox9);
			((Control)panelCombine).get_Controls().Add((Control)(object)label63);
			((Control)panelCombine).get_Controls().Add((Control)(object)label56);
			((Control)panelCombine).get_Controls().Add((Control)(object)txtEmailSignatureName);
			((Control)panelCombine).get_Controls().Add((Control)(object)label40);
			((Control)panelCombine).get_Controls().Add((Control)(object)checkBox1);
			((Control)panelCombine).get_Controls().Add((Control)(object)checkBox2);
			((Control)panelCombine).get_Controls().Add((Control)(object)cmdGenerateGrazeIndex);
			((Control)panelCombine).get_Controls().Add((Control)(object)label36);
			((Control)panelCombine).get_Controls().Add((Control)(object)cmbObservationsSource);
			((Control)panelCombine).get_Controls().Add((Control)(object)cmdProcessfromCoordinators);
			((Control)panelCombine).set_Location(new Point(170, 62));
			((Control)panelCombine).set_Name("panelCombine");
			((Control)panelCombine).set_Size(new Size(738, 439));
			((Control)panelCombine).set_TabIndex(69);
			((Control)groupBox9).get_Controls().Add((Control)(object)chkVizierLimitTo1960);
			((Control)groupBox9).get_Controls().Add((Control)(object)cmdVizier);
			((Control)groupBox9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox9).set_Location(new Point(128, 292));
			((Control)groupBox9).set_Name("groupBox9");
			((Control)groupBox9).set_Size(new Size(339, 115));
			((Control)groupBox9).set_TabIndex(78);
			groupBox9.set_TabStop(false);
			((Control)groupBox9).set_Text("Create Lunar Archive for Vizier");
			((Control)chkVizierLimitTo1960).set_AutoSize(true);
			((Control)chkVizierLimitTo1960).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkVizierLimitTo1960).set_Location(new Point(9, 83));
			((Control)chkVizierLimitTo1960).set_Name("chkVizierLimitTo1960");
			((Control)chkVizierLimitTo1960).set_Size(new Size(154, 17));
			((Control)chkVizierLimitTo1960).set_TabIndex(78);
			((Control)chkVizierLimitTo1960).set_Text("Limit to events before 1961");
			((ButtonBase)chkVizierLimitTo1960).set_UseVisualStyleBackColor(true);
			((Control)chkVizierLimitTo1960).set_Visible(false);
			((Control)cmdVizier).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdVizier).set_Location(new Point(21, 33));
			((Control)cmdVizier).set_Name("cmdVizier");
			((Control)cmdVizier).set_Size(new Size(120, 44));
			((Control)cmdVizier).set_TabIndex(77);
			((Control)cmdVizier).set_Text("Create Archive for Vizier");
			((ButtonBase)cmdVizier).set_UseVisualStyleBackColor(true);
			((Control)cmdVizier).add_Click((EventHandler)cmdVizier_Click);
			((Control)label63).set_AutoSize(true);
			((Control)label63).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label63).set_Location(new Point(247, 158));
			((Control)label63).set_Name("label63");
			((Control)label63).set_Size(new Size(126, 13));
			((Control)label63).set_TabIndex(76);
			((Control)label63).set_Text("Grazes are renumbered...");
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_Location(new Point(358, 69));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(229, 13));
			((Control)label56).set_TabIndex(75);
			((Control)label56).set_Text("Signature name to use on Email to coordinators");
			((Control)txtEmailSignatureName).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "SignatureBlockName", true, (DataSourceUpdateMode)1));
			((Control)txtEmailSignatureName).set_Location(new Point(358, 85));
			((Control)txtEmailSignatureName).set_Name("txtEmailSignatureName");
			((Control)txtEmailSignatureName).set_Size(new Size(180, 20));
			((Control)txtEmailSignatureName).set_TabIndex(74);
			((Control)txtEmailSignatureName).set_Text(Settings.Default.SignatureBlockName);
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Location(new Point(134, 55));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(75, 13));
			((Control)label40).set_TabIndex(73);
			((Control)label40).set_Text("in Reductions:");
			((Control)checkBox1).set_AutoSize(true);
			checkBox1.set_Checked(Settings.Default.ArchiveGrazesExcludeInvalid);
			checkBox1.set_CheckState((CheckState)1);
			((Control)checkBox1).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ArchiveGrazesExcludeInvalid", true, (DataSourceUpdateMode)1));
			((Control)checkBox1).set_Location(new Point(137, 71));
			((Control)checkBox1).set_Name("checkBox1");
			((Control)checkBox1).set_Size(new Size(160, 17));
			((Control)checkBox1).set_TabIndex(72);
			((Control)checkBox1).set_Text("Exclude invalid observations");
			((ButtonBase)checkBox1).set_UseVisualStyleBackColor(true);
			((Control)checkBox2).set_AutoSize(true);
			checkBox2.set_Checked(Settings.Default.ArchiveGrazesExcludeStartEnd);
			checkBox2.set_CheckState((CheckState)1);
			((Control)checkBox2).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ArchiveGrazesExcludeStartEnd", true, (DataSourceUpdateMode)1));
			((Control)checkBox2).set_Location(new Point(137, 88));
			((Control)checkBox2).set_Name("checkBox2");
			((Control)checkBox2).set_Size(new Size(177, 17));
			((Control)checkBox2).set_TabIndex(71);
			((Control)checkBox2).set_Text("Exclude Start/End graze events");
			((ButtonBase)checkBox2).set_UseVisualStyleBackColor(true);
			((Control)cmdGenerateGrazeIndex).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGenerateGrazeIndex).set_Location(new Point(248, 183));
			((Control)cmdGenerateGrazeIndex).set_Name("cmdGenerateGrazeIndex");
			((Control)cmdGenerateGrazeIndex).set_Size(new Size(124, 41));
			((Control)cmdGenerateGrazeIndex).set_TabIndex(70);
			((Control)cmdGenerateGrazeIndex).set_Text("Create graze index file\r\nfrom main files");
			((ButtonBase)cmdGenerateGrazeIndex).set_UseVisualStyleBackColor(true);
			((Control)cmdGenerateGrazeIndex).add_Click((EventHandler)cmdGenerateGrazeIndex_Click);
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Location(new Point(141, 7));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(145, 13));
			((Control)label36).set_TabIndex(69);
			((Control)label36).set_Text("select source of observations");
			((Control)panelResiduals).get_Controls().Add((Control)(object)label34);
			((Control)panelResiduals).get_Controls().Add((Control)(object)lblResidual2);
			((Control)panelResiduals).set_Location(new Point(160, 122));
			((Control)panelResiduals).set_Name("panelResiduals");
			((Control)panelResiduals).set_Size(new Size(840, 37));
			((Control)panelResiduals).set_TabIndex(70);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1128, 682));
			((Control)this).get_Controls().Add((Control)(object)panelHeader);
			((Control)this).get_Controls().Add((Control)(object)panelGeneralEdit);
			((Control)this).get_Controls().Add((Control)(object)panelSiteEdit);
			((Control)this).get_Controls().Add((Control)(object)panelCombine);
			((Control)this).get_Controls().Add((Control)(object)panelResiduals);
			((Control)this).get_Controls().Add((Control)(object)panelGraze);
			((Control)this).get_Controls().Add((Control)(object)optCombineRegions);
			((Control)this).get_Controls().Add((Control)(object)optGrazeEditor);
			((Control)this).get_Controls().Add((Control)(object)lstCheckedEdit);
			((Control)this).get_Controls().Add((Control)(object)optDoublesEditor);
			((Control)this).get_Controls().Add((Control)(object)optSiteEditor);
			((Control)this).get_Controls().Add((Control)(object)optGeneral);
			((Control)this).get_Controls().Add((Control)(object)lstEdit);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)panelDoubles);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarArchiveEditor", true, (DataSourceUpdateMode)1));
			((Form)this).set_Location(Settings.Default.LocationLunarArchiveEditor);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(1144, 500));
			((Control)this).set_Name("ArchiveEditor");
			((Form)this).set_StartPosition((FormStartPosition)1);
			((Control)this).set_Text("Archive editor");
			((Form)this).add_Load((EventHandler)ArchiveEditor_Load);
			((Control)this).add_Resize((EventHandler)ArchiveEditor_Resize);
			((Control)pnlPre1750).ResumeLayout(false);
			((Control)pnlPre1750).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((ISupportInitialize)updnAcc).EndInit();
			((ISupportInitialize)updnSecond).EndInit();
			((ISupportInitialize)updnPE).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnHour).EndInit();
			((ISupportInitialize)updnMinute).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnYear).EndInit();
			((ISupportInitialize)updnLongSec).EndInit();
			((ISupportInitialize)updnLongD).EndInit();
			((ISupportInitialize)updnLongM).EndInit();
			((ISupportInitialize)updnLatS).EndInit();
			((ISupportInitialize)updnLatD).EndInit();
			((ISupportInitialize)updnLatM).EndInit();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((Control)groupBox5).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panelGeneralEdit).ResumeLayout(false);
			((Control)panelGeneralEdit).PerformLayout();
			((Control)panelEventTypes).ResumeLayout(false);
			((Control)panelEventTypes).PerformLayout();
			((Control)panelSiteEdit).ResumeLayout(false);
			((Control)panelSiteEdit).PerformLayout();
			((Control)groupBox6).ResumeLayout(false);
			((Control)groupBox6).PerformLayout();
			((Control)panelILOC).ResumeLayout(false);
			((Control)panelILOC).PerformLayout();
			((Control)panelRGO).ResumeLayout(false);
			((Control)panelRGO).PerformLayout();
			((Control)PanelILOCsiteReplace).ResumeLayout(false);
			((Control)PanelILOCsiteReplace).PerformLayout();
			((Control)panelHeader).ResumeLayout(false);
			((Control)panelHeader).PerformLayout();
			((Control)panelDoubles).ResumeLayout(false);
			((Control)panelDoubles).PerformLayout();
			((Control)groupBox7).ResumeLayout(false);
			((Control)groupBox7).PerformLayout();
			((Control)grpSetWDS).ResumeLayout(false);
			((Control)panelGraze).ResumeLayout(false);
			((Control)panelGraze).PerformLayout();
			((Control)grpGrazeSites).ResumeLayout(false);
			((Control)grpGrazeSites).PerformLayout();
			((Control)grpGrazeDoubles).ResumeLayout(false);
			((Control)groupBox8).ResumeLayout(false);
			((Control)grpGrazeScopes).ResumeLayout(false);
			((Control)grpGrazeScopes).PerformLayout();
			((Control)grpTelescopesAndNames).ResumeLayout(false);
			((Control)grpTelescopesAndNames).PerformLayout();
			((Control)grpEventDuration).ResumeLayout(false);
			((Control)grpEventDuration).PerformLayout();
			((Control)grpTelescopeCodes).ResumeLayout(false);
			((Control)grpGrazeDR).ResumeLayout(false);
			((Control)grp2ndLimit).ResumeLayout(false);
			((Control)grpDatums).ResumeLayout(false);
			((Control)grpDatums).PerformLayout();
			((Control)grpGrazeRenumber).ResumeLayout(false);
			((Control)grpDuplicateGrazeEvents).ResumeLayout(false);
			((Control)groupBox11).ResumeLayout(false);
			((Control)groupBox11).PerformLayout();
			((Control)grpLargeResiduals).ResumeLayout(false);
			((Control)grpLargeResiduals).PerformLayout();
			((ISupportInitialize)updnSmoothTop).EndInit();
			((ISupportInitialize)updnSmoothBottom).EndInit();
			((ISupportInitialize)updnLimbCorrectedResidual).EndInit();
			((Control)grpNullSeconds).ResumeLayout(false);
			((Control)grpGrazeFlag).ResumeLayout(false);
			((Control)grpGrazeFlag).PerformLayout();
			((Control)panelCombine).ResumeLayout(false);
			((Control)panelCombine).PerformLayout();
			((Control)groupBox9).ResumeLayout(false);
			((Control)groupBox9).PerformLayout();
			((Control)panelResiduals).ResumeLayout(false);
			((Control)panelResiduals).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
