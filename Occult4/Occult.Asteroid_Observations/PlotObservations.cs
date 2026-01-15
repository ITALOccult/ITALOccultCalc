using System;
using System.ComponentModel;
using System.Configuration;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Ephemerides;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class PlotObservations : Form
	{
		internal bool Updating;

		internal bool GettingTag;

		internal bool GettingSlope;

		internal bool PreventUpdatingChanges;

		internal bool InitialiseForm;

		private float OldX;

		private float OldY;

		private float OldArg0;

		private static int Xrev0;

		private static int Yrev0;

		private static int Xrev1;

		private static int Yrev1;

		private static int CurrentPlotRecord = -1;

		internal int CurrentlySelectedComponent;

		private static double NormalMotion = 0.0;

		private const double Radian = 180.0 / Math.PI;

		private bool GetToolTip = true;

		private bool UnsavedShapeChanges;

		private bool IsResizing;

		private bool SatFieldsChanging;

		internal Button[] cmdTransferImage = (Button[])(object)new Button[14];

		internal Label[] lblISAMmatch = (Label[])(object)new Label[6];

		internal CheckBox[] chkReviewList = (CheckBox[])(object)new CheckBox[4];

		private int xPos_Shape;

		private int yPos_Shape;

		private int xPos_Satellites;

		private int yPos_Satellites;

		private int xPos_SatellitesIcon;

		private int yPos_SatellitesIcon;

		private int xPos_Doubles;

		private int yPos_Doubles;

		private int xPos_DoublesIcon;

		private int yPos_DoublesIcon;

		private int xPos_Grid;

		private int yPos_Grid;

		internal string[] PlotMenuDefaults = new string[32];

		private bool XYABplacesChanging;

		internal static bool SetDecimalPlaces = true;

		private bool LightDropChanging;

		private bool IncludeAxisOfRotation_ShapeModel = true;

		private bool DrawDamitID_ShapeModel = true;

		private bool DrawDAMITMean_ShapeModel = true;

		private bool ShowModelFaceEdges_ShapeModel;

		private bool BlackWhiteShapeModel;

		private bool DarkShapeModel;

		internal NumericUpDown[] updn_DoublePA = (NumericUpDown[])(object)new NumericUpDown[4];

		internal NumericUpDown[] updn_DoubleSep = (NumericUpDown[])(object)new NumericUpDown[4];

		internal Label[] lbl_DoubleValues = (Label[])(object)new Label[4];

		internal NumericUpDown[] updn_SatPA = (NumericUpDown[])(object)new NumericUpDown[4];

		internal NumericUpDown[] updn_SatSep = (NumericUpDown[])(object)new NumericUpDown[4];

		internal NumericUpDown[] updn_SatPAUncert = (NumericUpDown[])(object)new NumericUpDown[4];

		internal NumericUpDown[] updn_SatSepUncert = (NumericUpDown[])(object)new NumericUpDown[4];

		internal NumericUpDown[] updn_ASat = (NumericUpDown[])(object)new NumericUpDown[4];

		internal NumericUpDown[] updn_BSat = (NumericUpDown[])(object)new NumericUpDown[4];

		internal NumericUpDown[] updn_PASat = (NumericUpDown[])(object)new NumericUpDown[4];

		internal NumericUpDown[] updn_NumChords = (NumericUpDown[])(object)new NumericUpDown[4];

		internal ComboBox[] cmb_SatFitQuality = (ComboBox[])(object)new ComboBox[4];

		internal TextBox[] txtSatID = (TextBox[])(object)new TextBox[4];

		internal TextBox[] txtSat_dRA = (TextBox[])(object)new TextBox[4];

		internal TextBox[] txtSat_d2RA = (TextBox[])(object)new TextBox[4];

		internal TextBox[] txtSat_dDec = (TextBox[])(object)new TextBox[4];

		internal TextBox[] txtSat_d2Dec = (TextBox[])(object)new TextBox[4];

		internal TextBox[] txtCBET = (TextBox[])(object)new TextBox[4];

		internal Label[] lbl_SatXYoffset = (Label[])(object)new Label[4];

		internal CheckBox[] chkReview = (CheckBox[])(object)new CheckBox[4];

		internal string chkReviewZero_Label = " items are tagged for review";

		internal RadioButton[] optCompanion = (RadioButton[])(object)new RadioButton[4];

		internal string[] MiriadeAsteroids;

		private Timer FlashTimer;

		private Timer FlashTimerDouble;

		private static readonly int ColorCount = 4;

		private static readonly int ColorCountDouble = 4;

		private static Color[] C = new Color[ColorCount];

		private static Color[] CDouble = new Color[ColorCount];

		private static int CurrentColor = 0;

		private static int CurrentColorDouble = 0;

		private static string[] QualityExplanation = new string[8] { "The SM fit has not been considered", "SM mismatch because the observation is unreliable", "SM is clearly inconsistent with the chords", "Chord(s) matched to longest path across the SM", "Chords have major differences from SM, but an approx dia possible", "Chords: poorly spaced across / significant differences from, SM", "Many chords without major differences from SM", "Chords do not provide any constraint on the SM size" };

		private IContainer components;

		private MenuStrip menuStrip1;

		private Label label8;

		private Label label7;

		private Label label6;

		private Label label5;

		private Label label4;

		private Label label3;

		private Label label1;

		private Label label9;

		public NumericUpDown updnY;

		public NumericUpDown updnA;

		public NumericUpDown updnB;

		public NumericUpDown updnPA;

		public NumericUpDown updnX;

		public CheckBox chkMiss;

		public ComboBox cmbQuality;

		public CheckBox chkCircle;

		public CheckBox chkY;

		public CheckBox chkA;

		public CheckBox chkB;

		public CheckBox chkPA;

		public CheckBox chkCompanionSep;

		public CheckBox chkCompanionPA;

		public CheckBox chkX;

		private Label label10;

		private Button cmdFit;

		private ToolStripMenuItem withPlotToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem plotOptionsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem copyObserverListToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator3;

		public ToolStripMenuItem mnuBlackBackground;

		public ToolStripMenuItem mnuShowMarkers;

		public ToolStripMenuItem mnuShowID;

		public ToolStripMenuItem mnuShowVideo;

		public ToolStripMenuItem mnuShowErrors;

		public ToolStripMenuItem mnuShowPathsMiss;

		public ToolStripMenuItem mnuShowZeroWeighted;

		public ToolStripMenuItem mnuShowPredicted;

		public ToolStripMenuItem mnuShowCloud;

		public ToolStripMenuItem mnuShowEllipse;

		public ToolStripMenuItem mnuShowAxes;

		public ToolStripMenuItem mnuAlign;

		public ToolStripMenuItem mnuShowSolution;

		public ToolStripMenuItem mnuIncludeTimeShift;

		public Panel panelDouble;

		public ListBox lstObservers;

		public PictureBox picPlot;

		public VScrollBar Vbar;

		public HScrollBar Hbar;

		public RadioButton optSecondary;

		public RadioButton optPrimary;

		public RadioButton optBoth;

		public TrackBar SliderScale;

		private Label lblTag;

		public Label lblRMS;

		private ToolStripMenuItem saveObserverListToolStripMenuItem;

		private Button cmdMissTimes;

		private Button cmdShowEditor;

		private ToolStripMenuItem helpToolStripMenuItem;

		internal Label lblAxisRatio;

		internal ToolStripMenuItem mnuShowPlotScalekm;

		internal ToolStripMenuItem mnuPathsInColour;

		internal PictureBox picLegend;

		internal Panel PanelLegend;

		internal ToolStripMenuItem mnuShowEllipseInOutline;

		internal ToolStripMenuItem mnuThickLines_Paths;

		private ToolStripSeparator toolStripSeparator2;

		private ToolStripSeparator toolStripSeparator4;

		internal ToolStripMenuItem mnuLargeLabels;

		private ToolStripMenuItem drawAsteroidProfileToolStripMenuItem;

		private ToolStripMenuItem saveEntireFormAsAnImageToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator5;

		public ToolStripMenuItem addImageFromClipboardToolStripMenuItem;

		private ToolTip toolTip;

		private ToolStripMenuItem showStarDiameterAnalyserToolStripMenuItem;

		internal Label lblMotion;

		public Label lblDoubleStar;

		public ToolStripMenuItem addShapeImageFromDAMIT_ISAMToolStripMenuItem;

		internal ToolStripMenuItem mnuShowScaleGrid;

		private ToolStripMenuItem kmToolStripMenuItem_1;

		private ToolStripMenuItem kmToolStripMenuItem_2;

		private ToolStripMenuItem kmToolStripMenuItem_5;

		private ToolStripMenuItem kmToolStripMenuItem_10;

		private ToolStripMenuItem kmToolStripMenuItem_20;

		private ToolStripMenuItem kmToolStripMenuItem_50;

		private ToolStripMenuItem kmToolStripMenuItem_100;

		private ToolStripMenuItem kmToolStripMenuItem_200;

		private ToolStripMenuItem kmToolStripMenuItem_500;

		private Panel panelGridAngles;

		private Button cmdCloseGridAngles;

		private ToolStripMenuItem setOrientationToolStripMenuItem;

		private Label label17;

		internal ComboBox dropAngles;

		private ToolStripSeparator toolStripSeparator7;

		private Button cmdCloseShapeModelControl;

		private ToolStripSeparator toolStripSeparator8;

		internal ToolStripMenuItem mnuShowMeanDiameterDAMIT;

		internal TrackBar tbarScaleAdjust;

		private ToolStripMenuItem keepOnTopToolStripMenuItem;

		private Label label18;

		internal TrackBar trackOpacity;

		internal Panel pnlDoubleStarSolution;

		internal Label lblSatelliteQuality;

		internal Label lblNumberOfSolutions;

		internal Button cmdSetOffsets;

		internal Label lbl4;

		internal Label lbl3;

		internal Label lbl2;

		internal Label lbl1;

		internal CheckBox chkUseAssumedDiameter;

		private Label label12;

		private Label label11;

		private Label label23;

		private Label label21;

		private Label label20;

		private Label label19;

		private Label label24;

		private Label label22;

		private Label label25;

		internal TextBox txtModelPhase;

		internal Button cmdGetLightCurves;

		private Label label26;

		private Label lblCurrentMaxDia;

		private Label lblCurrentMinDia;

		private Label lblCurrentPhase;

		internal Button cmdSetModelMax;

		internal Button cmdSetModelMin;

		internal TextBox txtModelMaxDia;

		internal TextBox txtModelMinDia;

		private Label label27;

		internal TextBox txtSurface_Volume_Ratio;

		internal ToolStripMenuItem showShapeModelControlsOnPlotToolStripMenuItem;

		private Panel panel2;

		internal Panel panelShapeModelControl;

		internal GroupBox grpBestFit;

		internal CheckBox chkShapeModelCentered;

		private Button cmdHelpOnCompanionID;

		private Label label30;

		internal Panel PanelSatellites;

		internal Label lblCurrentFit;

		private Label label33;

		private Label label32;

		private Label label31;

		private Label label29;

		private Button cmdGetSatelliteMotions;

		private Label label34;

		private Panel PanelMiriade;

		private Label label35;

		internal ComboBox cmbModelFitQuality;

		internal TextBox txtModelSource;

		internal TextBox txtVersion;

		internal TextBox txtModelID;

		private ToolStripSeparator toolStripSeparator6;

		private Label lblSatNum;

		private Label label28;

		private Label label38;

		private Label label37;

		private Label label36;

		private Label label2;

		private Label label14;

		internal RadioButton optyPlotNormal;

		internal RadioButton optPlotx5;

		internal RadioButton optPlotx2;

		private Panel panel4;

		private Label label15;

		internal Label lblSatelliteNumbers;

		internal NumericUpDown updnSatNum;

		private Label label40;

		private Label label39;

		private Label label41;

		private Label label42;

		private Button cmdHelpDoubles;

		private Button cmdSet3Checks;

		private Panel pnlNames;

		private Label label43;

		internal Label lblSat4;

		internal Label lblSat1;

		internal Label lblSat3;

		internal Label lblSat2;

		public ToolStripMenuItem showASTROMETRYLocation;

		private Label label45;

		private Label label44;

		public NumericUpDown updnCenterOfMass_Y;

		public NumericUpDown updnCenterOfMass_X;

		private Label label46;

		internal Panel panelLimbFit;

		internal TrackBar trackLimbFit;

		private Label lblLimbFit;

		private Panel panel5;

		private Label label16;

		internal CheckBox chkScoll;

		private ToolStripSeparator toolStripSeparator9;

		private ToolStripMenuItem saveCurrentSettingsAsDefaultToolStripMenuItem;

		private ToolStripMenuItem applyDEFAULTSettingsToolStripMenuItem;

		private ToolStripMenuItem restoreOccultDefaultValuesToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator10;

		private ToolStripSeparator toolStripSeparator11;

		internal NumericUpDown updnBrightRatio;

		internal NumericUpDown updnBrightnessUncertPerCent;

		private Label label47;

		private Label label48;

		public Label lblOffset;

		private Button cmdShapeHelp;

		internal ToolStripMenuItem showRINGEventPathsToolStripMenuItem;

		public ToolStripMenuItem mnuShowPathsOcculted;

		public ToolStripMenuItem mnuShowPathsVisible;

		internal ToolStripMenuItem showPLOTSCALEmasToolStripMenuItem;

		internal ToolStripMenuItem mnuGrayBackground;

		internal ToolStripMenuItem applyWatermark;

		internal ToolStripMenuItem showFresnelDiffractionPeakToolStripMenuItem;

		internal ToolStripMenuItem ErrorBarsForFittingToShapeModels_menu;

		private Button cmdForDiameters;

		private Label label50;

		private Panel panel6;

		internal Panel panelSolve;

		private Label lblSolveSolnNum;

		internal RadioButton optSingle;

		internal RadioButton opt4;

		internal RadioButton optTwo;

		private TextBox textBox1;

		private Label label51;

		private Button cmdFit_Doubles;

		private Panel panel7;

		private Label label55;

		private Label label54;

		private Label label53;

		private Label label56;

		private TextBox txtMag2nd;

		private TextBox txtMagMain;

		internal TextBox txtMainDrop;

		internal TextBox txt2ndDrop;

		private Label label49;

		private Label label13;

		private Panel panel9;

		private Panel panel8;

		private Label label52;

		private Label label57;

		private Label lblSelectSth;

		private Label lblSelectNth;

		private Panel lblSelectSep;

		private Panel panel10;

		private Label label58;

		internal Panel pnlDouble;

		private Panel pnlOffset;

		private ToolStripMenuItem seeShapeModelDisplayFormForMorePlotOptionsToolStripMenuItem;

		private ToolStripMenuItem kmToolStripMenuItem1_200m;

		private ToolStripMenuItem kmToolStripMenuItem_500m;

		private ToolStripMenuItem kmToolStripMenuItem_100m;

		private ToolStripMenuItem saveImageToolStripMenuItem;

		private ToolStripMenuItem setScalecurrently100ToMatchMonitorScaleToolStripMenuItem;

		internal ToolStripMenuItem mnuShowTitle;

		private ToolStripMenuItem setPlotOptionsForPublicationPlotToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator12;

		private Button cmdEstimateOrbit;

		internal ToolStripMenuItem applyProvisionalMarkToolStripMenuItem;

		private ToolStripMenuItem shapeModelsToolStripMenuItem;

		private ToolStripMenuItem legendToolStripMenuItem;

		private ToolStripMenuItem plotColorsLineWidthsWatermarksToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator13;

		private Label label59;

		private Label label60;

		internal TextBox txtJDSO_Vol_Num_Pg;

		internal TextBox txtJDSOSubmitDate;

		private Panel panelJDSO;

		private Label label62;

		private Label label61;

		internal CheckBox chkEditJDSO;

		internal RadioButton opt1Component;

		internal ToolStripMenuItem showAsteroidEllipseInColorToolStripMenuItem;

		internal ToolStripMenuItem showEventInColorToolStripMenuItem;

		internal ToolStripMenuItem mnuThickLines_EllipseEvents;

		private Button cmdGridMinus;

		private Button cmdGridPlus;

		private ToolStripMenuItem setTHICKLineWidthInUserSettingsGroup8ToolStripMenuItem;

		private ToolStripMenuItem lARGEFontSizeSetInUserSettingsGroup8ToolStripMenuItem;

		internal ToolStripMenuItem showStarWithADiameterOfToolStripMenuItem;

		internal ToolStripTextBox toolStripStarDia_mas;

		internal ToolStripMenuItem showMISSEventsAsDashedLinesToolStripMenuItem;

		private Panel pnlCBET;

		internal CheckBox chkEditCBET;

		private Label label66;

		private PictureBox picDrag;

		private PictureBox picDragDouble;

		internal ToolStripMenuItem addThisTextToTheTitleToolStripMenuItem;

		internal ToolStripTextBox toolStrip_txtExtra;

		private ToolStripSeparator toolStripSeparator14;

		private ToolStripSeparator toolStripSeparator15;

		private Label lblDiaEquiv;

		internal ToolStripMenuItem showSatelliteOrbitsifAnyToolStripMenuItem;

		internal TextBox txtDoublePairID;

		private Label label64;

		private Panel panel3;

		private Panel panel1;

		internal Button cmdSaveModelInfo;

		private ToolStripMenuItem mnuWhiteBackground;

		private Panel pnlReview;

		private Label label63;

		private Label lblQualityExplanation;

		private ToolStripMenuItem showPolarAxesToolStripMenuItem;

		private ToolStripMenuItem includeAsterodIDAndModelIDToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator17;

		private ToolStripSeparator toolStripSeparator16;

		private ToolStripMenuItem showModelMeanDiameterLinesToolStripMenuItem;

		private ToolStripMenuItem showModelFacesToolStripMenuItem;

		private ToolStripMenuItem plotModelDarkToolStripMenuItem;

		private ToolStripMenuItem plotModelInGreyScaleToolStripMenuItem;

		internal ToolStripMenuItem drawStarImageAtPlotCenterToolStripMenuItem;

		public PlotObservations()
		{
			//IL_0169: Unknown result type (might be due to invalid IL or missing references)
			//IL_0173: Expected O, but got Unknown
			//IL_019b: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a5: Expected O, but got Unknown
			//IL_035b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0360: Unknown result type (might be due to invalid IL or missing references)
			//IL_036b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0373: Unknown result type (might be due to invalid IL or missing references)
			//IL_037a: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d4: Expected O, but got Unknown
			//IL_0558: Unknown result type (might be due to invalid IL or missing references)
			//IL_055e: Expected O, but got Unknown
			//IL_06d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_06da: Expected O, but got Unknown
			//IL_07c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_07cf: Expected O, but got Unknown
			//IL_0855: Unknown result type (might be due to invalid IL or missing references)
			//IL_085b: Expected O, but got Unknown
			//IL_0966: Unknown result type (might be due to invalid IL or missing references)
			//IL_096c: Expected O, but got Unknown
			//IL_0aab: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ab1: Expected O, but got Unknown
			//IL_0c09: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c0f: Expected O, but got Unknown
			//IL_0d67: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d6d: Expected O, but got Unknown
			//IL_0eb6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ebc: Expected O, but got Unknown
			//IL_1014: Unknown result type (might be due to invalid IL or missing references)
			//IL_101a: Expected O, but got Unknown
			//IL_1109: Unknown result type (might be due to invalid IL or missing references)
			//IL_110f: Expected O, but got Unknown
			//IL_1269: Unknown result type (might be due to invalid IL or missing references)
			//IL_126f: Expected O, but got Unknown
			//IL_13c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_13cf: Expected O, but got Unknown
			//IL_1518: Unknown result type (might be due to invalid IL or missing references)
			//IL_151e: Expected O, but got Unknown
			//IL_1673: Unknown result type (might be due to invalid IL or missing references)
			//IL_1679: Expected O, but got Unknown
			//IL_17bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_17c1: Expected O, but got Unknown
			//IL_18a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_18ab: Expected O, but got Unknown
			//IL_198f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1995: Expected O, but got Unknown
			//IL_1a7c: Unknown result type (might be due to invalid IL or missing references)
			//IL_1a82: Expected O, but got Unknown
			Updating = true;
			InitializeComponent();
			FlashTimer = new Timer();
			FlashTimer.set_Interval(500);
			FlashTimer.add_Tick((EventHandler)FlashTimer_Tick);
			FlashTimerDouble = new Timer();
			FlashTimerDouble.set_Interval(300);
			FlashTimerDouble.add_Tick((EventHandler)FlashTimerDouble_Tick);
			((ListControl)cmbQuality).set_SelectedIndex(0);
			C[0] = Color.LightGreen;
			C[1] = Color.Yellow;
			C[2] = Color.Cyan;
			C[3] = Color.Coral;
			CDouble[0] = Color.Chartreuse;
			CDouble[1] = Color.Yellow;
			CDouble[2] = Color.LightBlue;
			CDouble[3] = Color.Plum;
			((ToolStripItem)mnuLargeLabels).set_Text("use LARGE font for labels  (" + Settings.Default.AsterPlot_LargeFontSize + " pt)");
			Color[] array = new Color[8]
			{
				Color.LightCyan,
				Color.FromArgb(255, 200, 255, 200),
				Color.MistyRose,
				Color.SandyBrown,
				Color.FromArgb(255, 255, 255, 200),
				Color.LightGreen,
				Color.LightBlue,
				Color.LightCoral
			};
			((Control)lbl1).set_BackColor(array[0]);
			((Control)lbl2).set_BackColor(array[1]);
			((Control)lbl3).set_BackColor(array[2]);
			((Control)lbl4).set_BackColor(array[3]);
			ToolTip val = new ToolTip();
			val.set_AutoPopDelay(5000);
			val.set_InitialDelay(100);
			val.set_IsBalloon(true);
			val.set_ReshowDelay(10);
			val.set_ShowAlways(true);
			((Control)cmdHelpOnCompanionID).set_TabIndex(0);
			((Control)PanelMiriade).set_TabIndex(100);
			((Control)updnSatNum).set_TabIndex(1);
			((Control)cmdGetSatelliteMotions).set_TabIndex(14);
			string text = "A satellite identification must be either the permID or provID. Designations are not allowed.\r\nTo set the name, double-click a name in the box below\r\nIf a name is not listed, the IAU Designation field is left blank\r\n\r\nDesignation \t\t|permID\t\t|provID \r\n_____________________________________________________________________________\r\nJupiter XIII \t\t| Jupiter 13\t|\r\n(45) Eugenia I \t\t| (45) 1\t\t| \r\nS/2001 U 9\t\t|\t\t| S/2001 U 9 \r\nS/2001 S 31\t\t|\t\t| S/2001 S 31 \r\nS/2008 (41)1\t\t|\t\t| S/2008 (41) 1 \r\nS/2000 (1998 WW31) 1\t|\t\t| S/2000 (1998 WW31) 1";
			for (int i = 0; i < 4; i++)
			{
				updn_DoubleSep[i] = new NumericUpDown();
				((Control)updn_DoubleSep[i]).BringToFront();
				((Control)updn_DoubleSep[i]).set_Visible(i == 0);
				((Control)updn_DoubleSep[i]).set_Size(new Size(58, 20));
				((Control)updn_DoubleSep[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)updn_DoubleSep[i]).set_Location(new Point(10, 31));
				((Control)updn_DoubleSep[i]).set_BackColor(array[i]);
				updn_DoubleSep[i].set_Maximum(40000m);
				updn_DoubleSep[i].set_Minimum(0m);
				updn_DoubleSep[i].set_DecimalPlaces(1);
				updn_DoubleSep[i].set_Value(0m);
				((Control)updn_DoubleSep[i]).set_Tag((object)i);
				((Control)updn_DoubleSep[i]).set_TabStop(true);
				((Control)updn_DoubleSep[i]).add_Click((EventHandler)SEPchange);
				((Control)panelSolve).get_Controls().Add((Control)(object)updn_DoubleSep[i]);
				((Control)updn_DoubleSep[i]).set_TabIndex(20 * i + 1);
				if (i == 0)
				{
					((Control)chkCompanionSep).set_Left(((Control)updn_DoubleSep[i]).get_Right() + 10);
					((Control)label4).set_Left(((Control)updn_DoubleSep[i]).get_Left() - (((Control)label4).get_Width() - ((Control)updn_DoubleSep[i]).get_Width()) / 2);
				}
				updn_DoublePA[i] = new NumericUpDown();
				((Control)updn_DoublePA[i]).BringToFront();
				((Control)updn_DoublePA[i]).set_Visible(i == 0);
				((Control)updn_DoublePA[i]).set_Size(new Size(58, 20));
				((Control)updn_DoublePA[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)updn_DoublePA[i]).set_Location(new Point(((Control)panelSolve).get_Width() / 2 + 10, 31));
				((Control)updn_DoublePA[i]).set_BackColor(array[i]);
				updn_DoublePA[i].set_Maximum(362m);
				updn_DoublePA[i].set_Minimum(-360m);
				updn_DoublePA[i].set_DecimalPlaces(1);
				updn_DoublePA[i].set_Value(0m);
				((Control)updn_DoublePA[i]).set_Tag((object)i);
				((Control)panelSolve).get_Controls().Add((Control)(object)updn_DoublePA[i]);
				((Control)updn_DoublePA[i]).add_Click((EventHandler)PAchange);
				if (i == 0)
				{
					((Control)chkCompanionPA).set_Left(((Control)updn_DoublePA[i]).get_Right() + 10);
					((Control)label3).set_Left(((Control)updn_DoublePA[i]).get_Left() - (((Control)label3).get_Width() - ((Control)updn_DoublePA[i]).get_Width()) / 2);
				}
				optCompanion[i] = new RadioButton();
				((Control)optCompanion[i]).BringToFront();
				optCompanion[i].set_Checked(i == 0);
				((Control)optCompanion[i]).set_Size(new Size(37, 17));
				((Control)optCompanion[i]).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)optCompanion[i]).set_Location(new Point(50 + 65 * i, 1));
				((Control)optCompanion[i]).set_Tag((object)i);
				((Control)optCompanion[i]).set_BackColor(array[i]);
				((Control)optCompanion[i]).set_Text("#" + (i + 1));
				((Control)pnlDoubleStarSolution).get_Controls().Add((Control)(object)optCompanion[i]);
				((Control)optCompanion[i]).add_Click((EventHandler)SelectCompanion);
				lbl_DoubleValues[i] = new Label();
				((Control)lbl_DoubleValues[i]).BringToFront();
				((Control)lbl_DoubleValues[i]).set_Text("--, --°");
				((Control)lbl_DoubleValues[i]).set_AutoSize(true);
				((Control)lbl_DoubleValues[i]).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)lbl_DoubleValues[i]).set_Location(new Point(42 + 65 * i, 20));
				((Control)pnlDoubleStarSolution).get_Controls().Add((Control)(object)lbl_DoubleValues[i]);
				txtSatID[i] = new TextBox();
				((Control)txtSatID[i]).BringToFront();
				((Control)txtSatID[i]).set_Visible(i == 0);
				((Control)txtSatID[i]).set_Size(new Size(141, 20));
				((Control)txtSatID[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)txtSatID[i]).set_Location(new Point(96, 58));
				((Control)txtSatID[i]).set_BackColor(array[i + 4]);
				((Control)txtSatID[i]).set_Text("");
				((Control)txtSatID[i]).set_Tag((object)i);
				((TextBoxBase)txtSatID[i]).set_ReadOnly(true);
				((Control)txtSatID[i]).set_TabStop(true);
				((Control)PanelSatellites).get_Controls().Add((Control)(object)txtSatID[i]);
				((Control)txtSatID[i]).set_TabIndex(15 * (i + 1) + 1);
				toolTip.SetToolTip((Control)(object)txtSatID[i], text);
				txtCBET[i] = new TextBox();
				((Control)txtCBET[i]).BringToFront();
				((Control)txtCBET[i]).set_Visible(i == 0);
				((Control)txtCBET[i]).set_Size(new Size(46, 20));
				((Control)txtCBET[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
				((Control)txtCBET[i]).set_Location(new Point(205, 1));
				((Control)txtCBET[i]).set_BackColor(Color.LightCyan);
				((Control)txtCBET[i]).set_ForeColor(Color.Maroon);
				((Control)txtCBET[i]).set_Text("");
				txtCBET[i].set_TextAlign((HorizontalAlignment)2);
				((Control)txtCBET[i]).set_Tag((object)i);
				((TextBoxBase)txtCBET[i]).set_ReadOnly(true);
				((Control)txtCBET[i]).set_TabStop(true);
				((Control)txtCBET[i]).add_TextChanged((EventHandler)CBET_change);
				((Control)pnlCBET).get_Controls().Add((Control)(object)txtCBET[i]);
				((Control)txtCBET[i]).set_TabIndex(15 * (i + 1) + 1);
				toolTip.SetToolTip((Control)(object)txtCBET[i], text);
				updn_SatSep[i] = new NumericUpDown();
				((Control)updn_SatSep[i]).BringToFront();
				((Control)updn_SatSep[i]).set_Visible(i == 0);
				((Control)updn_SatSep[i]).set_Size(new Size(58, 20));
				((Control)updn_SatSep[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)updn_SatSep[i]).set_Location(new Point(12, ((Control)label14).get_Bottom() + 3));
				((Control)updn_SatSep[i]).set_BackColor(array[i + 4]);
				updn_SatSep[i].set_Maximum(10000m);
				updn_SatSep[i].set_Minimum(0m);
				updn_SatSep[i].set_Increment(0.5m);
				updn_SatSep[i].set_DecimalPlaces(1);
				updn_SatSep[i].set_Value(0m);
				((Control)updn_SatSep[i]).set_Tag((object)i);
				((Control)updn_SatSep[i]).set_TabStop(true);
				((Control)updn_SatSep[i]).add_Click((EventHandler)SAT_SEPchange);
				((Control)PanelSatellites).get_Controls().Add((Control)(object)updn_SatSep[i]);
				((Control)updn_SatSep[i]).set_TabIndex(15 * (i + 1) + 2);
				updn_SatSepUncert[i] = new NumericUpDown();
				((Control)updn_SatSepUncert[i]).BringToFront();
				((Control)updn_SatSepUncert[i]).set_Visible(i == 0);
				((Control)updn_SatSepUncert[i]).set_Size(new Size(45, 20));
				((Control)updn_SatSepUncert[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)updn_SatSepUncert[i]).set_Location(new Point(76, ((Control)label14).get_Bottom() + 3));
				((Control)updn_SatSepUncert[i]).set_BackColor(array[i + 4]);
				updn_SatSepUncert[i].set_Maximum(300m);
				updn_SatSepUncert[i].set_Minimum(0m);
				updn_SatSepUncert[i].set_Increment(0.2m);
				updn_SatSepUncert[i].set_DecimalPlaces(1);
				updn_SatSepUncert[i].set_Value(0m);
				((Control)updn_SatSepUncert[i]).set_Tag((object)i);
				((Control)updn_SatSepUncert[i]).set_TabStop(true);
				((Control)updn_SatSepUncert[i]).add_Click((EventHandler)SAT_SEPUncertchange);
				((Control)PanelSatellites).get_Controls().Add((Control)(object)updn_SatSepUncert[i]);
				((Control)updn_SatSepUncert[i]).set_TabIndex(15 * (i + 1) + 3);
				updn_SatPA[i] = new NumericUpDown();
				((Control)updn_SatPA[i]).BringToFront();
				((Control)updn_SatPA[i]).set_Visible(i == 0);
				((Control)updn_SatPA[i]).set_Size(new Size(58, 20));
				((Control)updn_SatPA[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)updn_SatPA[i]).set_Location(new Point(137, ((Control)label14).get_Bottom() + 3));
				((Control)updn_SatPA[i]).set_BackColor(array[i + 4]);
				updn_SatPA[i].set_Maximum(362m);
				updn_SatPA[i].set_Minimum(-360m);
				updn_SatPA[i].set_DecimalPlaces(2);
				updn_SatPA[i].set_Value(0m);
				((Control)updn_SatPA[i]).set_Tag((object)i);
				((Control)updn_SatPA[i]).set_TabStop(true);
				((Control)updn_SatPA[i]).add_Click((EventHandler)SAT_PAchange);
				((Control)PanelSatellites).get_Controls().Add((Control)(object)updn_SatPA[i]);
				((Control)updn_SatPA[i]).set_TabIndex(15 * (i + 1) + 4);
				updn_SatPAUncert[i] = new NumericUpDown();
				((Control)updn_SatPAUncert[i]).BringToFront();
				((Control)updn_SatPAUncert[i]).set_Visible(i == 0);
				((Control)updn_SatPAUncert[i]).set_Size(new Size(52, 20));
				((Control)updn_SatPAUncert[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)updn_SatPAUncert[i]).set_Location(new Point(202, ((Control)label14).get_Bottom() + 3));
				((Control)updn_SatPAUncert[i]).set_BackColor(array[i + 4]);
				updn_SatPAUncert[i].set_Maximum(45m);
				updn_SatPAUncert[i].set_Minimum(0m);
				updn_SatPAUncert[i].set_Increment(0.1m);
				updn_SatPAUncert[i].set_DecimalPlaces(1);
				updn_SatPAUncert[i].set_Value(0m);
				((Control)updn_SatPAUncert[i]).set_Tag((object)i);
				((Control)updn_SatPAUncert[i]).set_TabStop(true);
				((Control)updn_SatPAUncert[i]).add_Click((EventHandler)SAT_PAUncertchange);
				((Control)PanelSatellites).get_Controls().Add((Control)(object)updn_SatPAUncert[i]);
				((Control)updn_SatPAUncert[i]).set_TabIndex(15 * (i + 1) + 5);
				lbl_SatXYoffset[i] = new Label();
				((Control)lbl_SatXYoffset[i]).BringToFront();
				((Control)lbl_SatXYoffset[i]).set_Visible(i == 0);
				((Control)lbl_SatXYoffset[i]).set_AutoSize(false);
				((Control)lbl_SatXYoffset[i]).set_Width(((Control)PanelSatellites).get_Width() - 3);
				((Control)lbl_SatXYoffset[i]).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)lbl_SatXYoffset[i]).set_Location(new Point(0, 218));
				((Control)lbl_SatXYoffset[i]).set_ForeColor(Color.DarkRed);
				((Control)lbl_SatXYoffset[i]).set_Text("Offset: dX =    dY =");
				lbl_SatXYoffset[i].set_TextAlign(ContentAlignment.TopCenter);
				((Control)lbl_SatXYoffset[i]).set_Tag((object)i);
				lbl_SatXYoffset[i].set_TabStop(false);
				((Control)PanelSatellites).get_Controls().Add((Control)(object)lbl_SatXYoffset[i]);
				updn_ASat[i] = new NumericUpDown();
				((Control)updn_ASat[i]).BringToFront();
				((Control)updn_ASat[i]).set_Visible(i == 0);
				((Control)updn_ASat[i]).set_Size(new Size(58, 20));
				((Control)updn_ASat[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)updn_ASat[i]).set_Location(new Point(12, ((Control)label36).get_Bottom() + 3));
				((Control)updn_ASat[i]).set_BackColor(array[i + 4]);
				updn_ASat[i].set_Maximum(3000m);
				updn_ASat[i].set_Minimum(0m);
				updn_ASat[i].set_Increment(0.1m);
				updn_ASat[i].set_DecimalPlaces(1);
				updn_ASat[i].set_Value(10m);
				((Control)updn_ASat[i]).set_Tag((object)i);
				((Control)updn_ASat[i]).set_TabStop(true);
				((Control)updn_ASat[i]).add_Click((EventHandler)SAT_A_change);
				((Control)PanelSatellites).get_Controls().Add((Control)(object)updn_ASat[i]);
				((Control)updn_ASat[i]).set_TabIndex(15 * (i + 1) + 6);
				updn_BSat[i] = new NumericUpDown();
				((Control)updn_BSat[i]).BringToFront();
				((Control)updn_BSat[i]).set_Visible(i == 0);
				((Control)updn_BSat[i]).set_Size(new Size(58, 20));
				((Control)updn_BSat[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)updn_BSat[i]).set_Location(new Point(89, ((Control)label36).get_Bottom() + 3));
				((Control)updn_BSat[i]).set_BackColor(array[i + 4]);
				updn_BSat[i].set_Maximum(2000m);
				updn_BSat[i].set_Minimum(0m);
				updn_BSat[i].set_Increment(0.1m);
				updn_BSat[i].set_DecimalPlaces(1);
				updn_BSat[i].set_Value(10m);
				((Control)updn_BSat[i]).set_Tag((object)i);
				((Control)updn_BSat[i]).set_TabStop(true);
				((Control)updn_BSat[i]).add_Click((EventHandler)SAT_B_change);
				((Control)PanelSatellites).get_Controls().Add((Control)(object)updn_BSat[i]);
				((Control)updn_BSat[i]).set_TabIndex(15 * (i + 1) + 7);
				updn_PASat[i] = new NumericUpDown();
				((Control)updn_PASat[i]).BringToFront();
				((Control)updn_PASat[i]).set_Visible(i == 0);
				((Control)updn_PASat[i]).set_Size(new Size(58, 20));
				((Control)updn_PASat[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)updn_PASat[i]).set_Location(new Point(166, ((Control)label36).get_Bottom() + 3));
				((Control)updn_PASat[i]).set_BackColor(array[i + 4]);
				updn_PASat[i].set_Maximum(365m);
				updn_PASat[i].set_Minimum(-360m);
				updn_PASat[i].set_DecimalPlaces(1);
				updn_PASat[i].set_Value(0m);
				((Control)updn_PASat[i]).set_Tag((object)i);
				((Control)updn_PASat[i]).set_TabStop(true);
				((Control)updn_PASat[i]).add_Click((EventHandler)SAT_PA_change);
				((Control)PanelSatellites).get_Controls().Add((Control)(object)updn_PASat[i]);
				((Control)updn_PASat[i]).set_TabIndex(15 * (i + 1) + 8);
				cmb_SatFitQuality[i] = new ComboBox();
				((Control)cmb_SatFitQuality[i]).BringToFront();
				((Control)cmb_SatFitQuality[i]).set_Visible(i == 0);
				((Control)cmb_SatFitQuality[i]).set_Size(new Size(100, 21));
				((Control)cmb_SatFitQuality[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)cmb_SatFitQuality[i]).set_Location(new Point(((Control)lblSatelliteQuality).get_Right() + 1, ((Control)lblSatelliteQuality).get_Top() - 4));
				((Control)cmb_SatFitQuality[i]).set_BackColor(array[i + 4]);
				string[] array2 = new string[5] { "None", "Is it a satellite ?", "Approx offset", "Offset + size", "Offset + shape" };
				for (int j = 0; j < array2.Length; j++)
				{
					cmb_SatFitQuality[i].get_Items().Insert(j, (object)array2[j]);
				}
				((ListControl)cmb_SatFitQuality[i]).set_SelectedIndex(0);
				((Control)cmb_SatFitQuality[i]).set_Tag((object)i);
				((Control)cmb_SatFitQuality[i]).set_TabStop(true);
				((Control)PanelSatellites).get_Controls().Add((Control)(object)cmb_SatFitQuality[i]);
				((Control)cmb_SatFitQuality[i]).set_TabIndex(15 * (i + 1) + 9);
				updn_NumChords[i] = new NumericUpDown();
				((Control)updn_NumChords[i]).BringToFront();
				((Control)updn_NumChords[i]).set_Visible(i == 0);
				((Control)updn_NumChords[i]).set_Size(new Size(38, 20));
				((Control)updn_NumChords[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)updn_NumChords[i]).set_Location(new Point(220, ((Control)lblSatelliteQuality).get_Top() - 4));
				((Control)updn_NumChords[i]).set_BackColor(array[i + 4]);
				updn_NumChords[i].set_Maximum(100m);
				updn_NumChords[i].set_Minimum(1m);
				updn_NumChords[i].set_DecimalPlaces(0);
				updn_NumChords[i].set_Value(1m);
				((Control)updn_NumChords[i]).set_Tag((object)i);
				((Control)updn_NumChords[i]).set_TabStop(true);
				((Control)updn_NumChords[i]).add_Click((EventHandler)ChordsChange);
				((Control)PanelSatellites).get_Controls().Add((Control)(object)updn_NumChords[i]);
				((Control)updn_NumChords[i]).set_TabIndex(15 * (i + 1) + 10);
				txtSat_dRA[i] = new TextBox();
				((Control)txtSat_dRA[i]).BringToFront();
				((Control)txtSat_dRA[i]).set_Visible(i == 0);
				((Control)txtSat_dRA[i]).set_Size(new Size(51, 20));
				((Control)txtSat_dRA[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)txtSat_dRA[i]).set_Location(new Point(44, 28));
				((Control)txtSat_dRA[i]).set_BackColor(array[i + 4]);
				((Control)txtSat_dRA[i]).set_Text("");
				((Control)txtSat_dRA[i]).set_Tag((object)i);
				((Control)txtSat_dRA[i]).set_TabStop(true);
				((Control)PanelMiriade).get_Controls().Add((Control)(object)txtSat_dRA[i]);
				((Control)txtSat_dRA[i]).set_TabIndex(4 * i + 70);
				txtSat_dDec[i] = new TextBox();
				((Control)txtSat_dDec[i]).BringToFront();
				((Control)txtSat_dDec[i]).set_Visible(i == 0);
				((Control)txtSat_dDec[i]).set_Size(new Size(51, 20));
				((Control)txtSat_dDec[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)txtSat_dDec[i]).set_Location(new Point(44, 51));
				((Control)txtSat_dDec[i]).set_BackColor(array[i + 4]);
				((Control)txtSat_dDec[i]).set_Text("");
				((Control)txtSat_dDec[i]).set_Tag((object)i);
				((Control)txtSat_dDec[i]).set_TabStop(true);
				((Control)PanelMiriade).get_Controls().Add((Control)(object)txtSat_dDec[i]);
				((Control)txtSat_dDec[i]).set_TabIndex(4 * i + 71);
				txtSat_d2RA[i] = new TextBox();
				((Control)txtSat_d2RA[i]).BringToFront();
				((Control)txtSat_d2RA[i]).set_Visible(i == 0);
				((Control)txtSat_d2RA[i]).set_Size(new Size(51, 20));
				((Control)txtSat_d2RA[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)txtSat_d2RA[i]).set_Location(new Point(137, 28));
				((Control)txtSat_d2RA[i]).set_BackColor(array[i + 4]);
				((Control)txtSat_d2RA[i]).set_Text("");
				((Control)txtSat_d2RA[i]).set_Tag((object)i);
				((Control)txtSat_d2RA[i]).set_TabStop(true);
				((Control)PanelMiriade).get_Controls().Add((Control)(object)txtSat_d2RA[i]);
				((Control)txtSat_d2RA[i]).set_TabIndex(4 * i + 72);
				txtSat_d2Dec[i] = new TextBox();
				((Control)txtSat_d2Dec[i]).BringToFront();
				((Control)txtSat_d2Dec[i]).set_Visible(i == 0);
				((Control)txtSat_d2Dec[i]).set_Size(new Size(51, 20));
				((Control)txtSat_d2Dec[i]).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
				((Control)txtSat_d2Dec[i]).set_Location(new Point(137, 51));
				((Control)txtSat_d2Dec[i]).set_BackColor(array[i + 4]);
				((Control)txtSat_d2Dec[i]).set_Text("");
				((Control)txtSat_d2Dec[i]).set_Tag((object)i);
				((Control)txtSat_d2Dec[i]).set_TabStop(true);
				((Control)PanelMiriade).get_Controls().Add((Control)(object)txtSat_d2Dec[i]);
				((Control)txtSat_d2Dec[i]).set_TabIndex(4 * i + 73);
			}
			CurrentlySelectedComponent = 0;
			SetDoubleSolutionType(4, Initialising: true);
			Updating = false;
		}

		private void PlotObservations_Load(object sender, EventArgs e)
		{
			//IL_0695: Unknown result type (might be due to invalid IL or missing references)
			//IL_069b: Expected O, but got Unknown
			//IL_0805: Unknown result type (might be due to invalid IL or missing references)
			//IL_080b: Expected O, but got Unknown
			//IL_08f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_08fe: Expected O, but got Unknown
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion);
			if ((((Control)this).get_Left() + ((Control)this).get_Width() <= Screen.GetWorkingArea((Control)(object)this).Left) | (((Control)this).get_Left() >= Screen.GetWorkingArea((Control)(object)this).Right) | (((Control)this).get_Bottom() <= 0) | (((Control)this).get_Top() >= Screen.GetWorkingArea((Control)(object)this).Bottom))
			{
				((Control)this).set_Left((Screen.GetWorkingArea((Control)(object)this).Width - ((Control)this).get_Width()) / 2);
				((Control)this).set_Top((Screen.GetWorkingArea((Control)(object)this).Height - ((Control)this).get_Height()) / 2);
			}
			else
			{
				((Form)this).set_Location(Settings.Default.LocationAsterPlotObs);
			}
			if ((((Control)this).get_Width() < 100) | (((Control)this).get_Height() < 100))
			{
				((Control)this).set_Width(818);
				((Control)this).set_Height(554);
				Settings.Default.SizeAsterPlot = (Point)((Form)this).get_Size();
			}
			else
			{
				((Form)this).set_Size((Size)Settings.Default.SizeAsterPlot);
			}
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).set_Text("Set scale (currently " + Settings.Default.MonitorScale + "%) to match Monitor scale");
			mnuBlackBackground.set_Checked(Settings.Default.ShapeModelBackColor == 0);
			mnuWhiteBackground.set_Checked(Settings.Default.ShapeModelBackColor == 1);
			mnuGrayBackground.set_Checked(Settings.Default.ShapeModelBackColor == 2);
			mnuPathsInColour.set_Checked(Settings.Default.AsterPlot_ChordsIncolour);
			showAsteroidEllipseInColorToolStripMenuItem.set_Checked(Settings.Default.AsterPlot_EllipseIncolour);
			showEventInColorToolStripMenuItem.set_Checked(Settings.Default.AsterPlot_EventsIncolour);
			showMISSEventsAsDashedLinesToolStripMenuItem.set_Checked(Settings.Default.AsterPlot_MissDashed);
			mnuThickLines_Paths.set_Checked(Settings.Default.AsterPlot_ThickLines_Paths);
			mnuLargeLabels.set_Checked(Settings.Default.AsterPlot_LargeFont);
			applyWatermark.set_Checked(Settings.Default.AsterPlotWatermark);
			applyProvisionalMarkToolStripMenuItem.set_Checked(Settings.Default.AsterPlotProvisional);
			mnuShowMeanDiameterDAMIT.set_Checked(Settings.Default.ShapeModel_ShowMean);
			mnuShowScaleGrid.set_Checked(Settings.Default.Asterplot_ShowScaleGrid);
			SetGridScale(Settings.Default.Asterplot_GridScale, Plot: false);
			showPLOTSCALEmasToolStripMenuItem.set_Checked(Settings.Default.AsterPlot_ShowScaleMAS);
			mnuShowPlotScalekm.set_Checked(Settings.Default.AsterPlot_ShowScaleKM);
			mnuShowSolution.set_Checked(Settings.Default.AsterPlot_ShowSolution);
			mnuShowTitle.set_Checked(Settings.Default.AsterPlot_ShowTitle);
			mnuShowEllipse.set_Checked(Settings.Default.AsterPlot_ShowEllipse);
			showFresnelDiffractionPeakToolStripMenuItem.set_Checked(Settings.Default.Asterplot_ShowFresnel);
			mnuShowEllipseInOutline.set_Checked(Settings.Default.AsterPlot_ShowEllipseInOutline);
			ToolStripMenuItem obj = drawAsteroidProfileToolStripMenuItem;
			bool asterPlotDrawLimb;
			((Control)panelLimbFit).set_Visible(asterPlotDrawLimb = Settings.Default.AsterPlotDrawLimb);
			obj.set_Checked(asterPlotDrawLimb);
			mnuShowAxes.set_Checked(Settings.Default.AsterPlot_ShowAxes);
			showASTROMETRYLocation.set_Checked(Settings.Default.AsterPlot_ShowAstrometry);
			showSatelliteOrbitsifAnyToolStripMenuItem.set_Checked(true);
			mnuShowMarkers.set_Checked(Settings.Default.AsterPlot_ShowMarkers);
			mnuShowID.set_Checked(Settings.Default.AsterPlot_ShowNumbers);
			mnuShowPredicted.set_Checked(Settings.Default.AsterPlot_ShowPredicted);
			mnuAlign.set_Checked(Settings.Default.AsterPlot_AlignDoubles);
			mnuShowErrors.set_Checked(Settings.Default.AsterPlot_ShowErrors);
			ErrorBarsForFittingToShapeModels_menu.set_Checked(true);
			Set_ColorFor_cmdForDiameters();
			mnuShowPathsOcculted.set_Checked(Settings.Default.AsterPlot_ShowOcculted);
			mnuShowPathsVisible.set_Checked(Settings.Default.AsterPlot_ShowVisible);
			showRINGEventPathsToolStripMenuItem.set_Checked(Settings.Default.AsterPlot_ShowRingPaths);
			mnuIncludeTimeShift.set_Checked(Settings.Default.AsterPlot_IncludeTimeShift);
			mnuShowPathsMiss.set_Checked(Settings.Default.AsterPlot_ShowMiss);
			mnuShowVideo.set_Checked(Settings.Default.AsterPlot_VideoOnly);
			mnuShowZeroWeighted.set_Checked(Settings.Default.AsterPlot_ShowZeroWeighted);
			mnuShowCloud.set_Checked(Settings.Default.AsterPlot_ShowClouded);
			showAsteroidEllipseInColorToolStripMenuItem.set_Checked(Settings.Default.AsterPlot_EllipseIncolour);
			showEventInColorToolStripMenuItem.set_Checked(Settings.Default.AsterPlot_EventsIncolour);
			mnuThickLines_EllipseEvents.set_Checked(Settings.Default.Asterplot_ThickLinesOther);
			((Control)PanelLegend).set_Top(((Control)lstObservers).get_Top());
			((Control)PanelLegend).set_Left(((Control)lstObservers).get_Left());
			((Control)PanelLegend).set_Width(((Control)lstObservers).get_Width());
			PictureBox obj2 = picLegend;
			int top;
			((Control)picLegend).set_Left(top = 0);
			((Control)obj2).set_Top(top);
			((Control)PanelLegend).set_Visible(mnuPathsInColour.get_Checked());
			for (int i = 0; i < 91; i++)
			{
				dropAngles.get_Items().Add((object)i);
			}
			((ListControl)dropAngles).set_SelectedIndex(0);
			((Control)panelShapeModelControl).set_Visible(showShapeModelControlsOnPlotToolStripMenuItem.get_Checked());
			Panel panelSatellites = PanelSatellites;
			((Control)panelDouble).set_Left(top = ((Control)picPlot).get_Right() - 10);
			((Control)panelSatellites).set_Left(top);
			((Control)cmdSet3Checks).set_Visible(Settings.Default.Administrator);
			((Control)chkEditJDSO).set_Enabled(Settings.Default.Administrator);
			showPolarAxesToolStripMenuItem.set_Checked(IncludeAxisOfRotation_ShapeModel = Settings.Default.IncludeAxisOfRotation);
			includeAsterodIDAndModelIDToolStripMenuItem.set_Checked(DrawDamitID_ShapeModel = Settings.Default.DrawDamitID);
			showModelMeanDiameterLinesToolStripMenuItem.set_Checked(DrawDAMITMean_ShapeModel = Settings.Default.ShapeModel_DrawMean);
			showModelFacesToolStripMenuItem.set_Checked(ShowModelFaceEdges_ShapeModel = Settings.Default.ModelFaceEdges);
			plotModelDarkToolStripMenuItem.set_Checked(DarkShapeModel = Settings.Default.ModelDark);
			plotModelInGreyScaleToolStripMenuItem.set_Checked(BlackWhiteShapeModel = Settings.Default.ModelinBlackWhite);
			for (int j = 0; j < 12; j++)
			{
				cmdTransferImage[j] = new Button();
				((Control)cmdTransferImage[j]).BringToFront();
				((Control)cmdTransferImage[j]).set_Width(18);
				((Control)cmdTransferImage[j]).set_Height(21);
				((Control)cmdTransferImage[j]).set_Top(1);
				if (j < 6)
				{
					((Control)cmdTransferImage[j]).set_Left(44 + 18 * j);
				}
				else
				{
					((Control)cmdTransferImage[j]).set_Left(268 + 24 * (j - 6));
				}
				((Control)cmdTransferImage[j]).set_Visible(true);
				((Control)cmdTransferImage[j]).set_Enabled(false);
				((Control)cmdTransferImage[j]).set_BackColor(Color.LightYellow);
				((Control)cmdTransferImage[j]).set_Font(new Font("Microsoft Sans Serif", 7f));
				if (j < 6)
				{
					((Control)cmdTransferImage[j]).set_Text((j + 1).ToString());
				}
				else
				{
					((Control)cmdTransferImage[j]).set_Text((j - 5).ToString());
				}
				((Control)cmdTransferImage[j]).set_Tag((object)(j + 1).ToString());
				((Control)panelShapeModelControl).get_Controls().Add((Control)(object)cmdTransferImage[j]);
				((Control)cmdTransferImage[j]).add_Click((EventHandler)TransferModels);
			}
			for (int k = 0; k < 6; k++)
			{
				lblISAMmatch[k] = new Label();
				((Control)lblISAMmatch[k]).BringToFront();
				((Control)lblISAMmatch[k]).set_Left(268 + 24 * k + 15);
				((Control)lblISAMmatch[k]).set_Top(10);
				((Control)lblISAMmatch[k]).set_Text((k + 1).ToString());
				((Control)lblISAMmatch[k]).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Bold));
				((Control)lblISAMmatch[k]).set_ForeColor(Color.Brown);
				((Control)lblISAMmatch[k]).set_BackColor(Color.White);
				((Control)lblISAMmatch[k]).set_Visible(true);
				((Control)lblISAMmatch[k]).set_AutoSize(true);
				((Control)panelShapeModelControl).get_Controls().Add((Control)(object)lblISAMmatch[k]);
			}
			for (int l = 0; l < Tags.chkReview_Labels.Length; l++)
			{
				chkReviewList[l] = new CheckBox();
				((Control)chkReviewList[l]).set_Left(3);
				((Control)chkReviewList[l]).set_Top(19 * l - 3);
				((Control)chkReviewList[l]).set_Width(((Control)pnlReview).get_Width());
				((Control)chkReviewList[l]).set_Text(Tags.chkReview_Labels[l]);
				((ButtonBase)chkReviewList[l]).set_TextAlign(ContentAlignment.MiddleLeft);
				((Control)chkReviewList[l]).set_Font(new Font("Microsoft Sans Serif", 8.25f));
				chkReviewList[l].set_AutoCheck(false);
				if (l == 0)
				{
					((Control)chkReviewList[l]).set_Text("0" + chkReviewZero_Label);
					((Control)chkReviewList[l]).set_BackColor(Color.LightCyan);
					((Control)chkReviewList[l]).set_Font(new Font("Microsoft Sans Serif", 9f));
				}
				((Control)pnlReview).get_Controls().Add((Control)(object)chkReviewList[l]);
				((Control)chkReviewList[l]).add_Click((EventHandler)CheckReview);
			}
		}

		private void SelectCompanion(object sender, EventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			SetCompanion(CurrentlySelectedComponent = int.Parse(((Control)(RadioButton)sender).get_Tag().ToString()));
			RePlotAfterChanges();
		}

		private void SetCompanion(int x)
		{
			for (int i = 0; i < 4; i++)
			{
				((Control)updn_DoublePA[i]).set_Visible(i == x);
				((Control)updn_DoubleSep[i]).set_Visible(i == x);
			}
			((Control)lblSolveSolnNum).set_Text("S o l v e   s o l u t i o n   #" + (x + 1));
		}

		private void SEPchange(object sender, EventArgs e)
		{
			if (opt4.get_Checked() && !DoubleStarOffsets_HaveBeenSet())
			{
				NoOffsets_Message();
				return;
			}
			SetPAIncrement();
			RePlotAfterChanges();
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			SetLabelValues();
		}

		internal void SetPAIncrement()
		{
			if (updn_DoubleSep[CurrentlySelectedComponent].get_Value() < 0m)
			{
				updn_DoubleSep[CurrentlySelectedComponent].set_Value(0.1m);
			}
			else if (updn_DoubleSep[CurrentlySelectedComponent].get_Value() > 100m)
			{
				updn_DoublePA[CurrentlySelectedComponent].set_Increment(0.2m);
			}
			else if (updn_DoubleSep[CurrentlySelectedComponent].get_Value() > 50m)
			{
				updn_DoublePA[CurrentlySelectedComponent].set_Increment(0.5m);
			}
			else if (updn_DoubleSep[CurrentlySelectedComponent].get_Value() > 20m)
			{
				updn_DoublePA[CurrentlySelectedComponent].set_Increment(1m);
			}
			else
			{
				updn_DoublePA[CurrentlySelectedComponent].set_Increment(2m);
			}
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void PAchange(object sender, EventArgs e)
		{
			if (EventDetails.NumberOfSatellites == 0 && opt4.get_Checked() && !DoubleStarOffsets_HaveBeenSet())
			{
				NoOffsets_Message();
				return;
			}
			if (EventDetails.NumberOfSatellites > 0)
			{
				SetSatelliteFields();
			}
			if (EventDetails.NumberOfSatellites > 0)
			{
				if (updn_DoublePA[CurrentlySelectedComponent].get_Value() > 360m)
				{
					NumericUpDown obj = updn_DoublePA[CurrentlySelectedComponent];
					obj.set_Value(obj.get_Value() - 360m);
				}
				if (updn_DoublePA[CurrentlySelectedComponent].get_Value() < 0m)
				{
					NumericUpDown obj2 = updn_DoublePA[CurrentlySelectedComponent];
					obj2.set_Value(obj2.get_Value() + 360m);
				}
			}
			else
			{
				if (updn_DoublePA[CurrentlySelectedComponent].get_Value() > 360m)
				{
					NumericUpDown obj3 = updn_DoublePA[CurrentlySelectedComponent];
					obj3.set_Value(obj3.get_Value() - 360m);
				}
				if (updn_DoublePA[CurrentlySelectedComponent].get_Value() < 0m)
				{
					NumericUpDown obj4 = updn_DoublePA[CurrentlySelectedComponent];
					obj4.set_Value(obj4.get_Value() + 360m);
				}
			}
			SetPAIncrement();
			RePlotAfterChanges();
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			SetLabelValues();
		}

		private void SetLabelValues()
		{
			try
			{
				((Control)Data_and_Plots.PlotForm.txtDoublePairID).set_Text(EventDetails.KnownPair_ID);
				((Control)Data_and_Plots.PlotForm.txtJDSOSubmitDate).set_Text(EventDetails.JDSO_SubmitDate);
				((Control)Data_and_Plots.PlotForm.txtJDSO_Vol_Num_Pg).set_Text(EventDetails.JDSO_Vol_Num_Pg);
			}
			catch
			{
			}
			for (int i = 0; i < 4; i++)
			{
				if (updn_DoubleSep[i].get_Value() == 0m)
				{
					((Control)lbl_DoubleValues[i]).set_Text("--, --°");
				}
				else
				{
					((Control)lbl_DoubleValues[i]).set_Text(string.Format("{0,1:f1}, {1,1:f0}°", updn_DoubleSep[i].get_Value(), updn_DoublePA[i].get_Value()));
				}
			}
		}

		private void CBET_change(object sender, EventArgs e)
		{
			if (!SatFieldsChanging)
			{
				SatFieldsChanging = true;
				EventDetails.Satellites[Data_and_Plots.SelectedSatellite].CBET_Discovery = ((Control)txtCBET[Data_and_Plots.SelectedSatellite]).get_Text();
				SatFieldsChanging = false;
				((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			}
		}

		private void SAT_SEPchange(object sender, EventArgs e)
		{
			if (!SatFieldsChanging)
			{
				SatFieldsChanging = true;
				SetPASatIncrement();
				SetSepSatIncrement();
				SetSatelliteFields();
				RePlotAfterChanges();
				SatFieldsChanging = false;
				((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			}
		}

		private void SAT_SEPUncertchange(object sender, EventArgs e)
		{
			if (!SatFieldsChanging)
			{
				SatFieldsChanging = true;
				SetSepSatIncrement();
				SetSatelliteFields();
				RePlotAfterChanges();
				SatFieldsChanging = false;
				((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			}
		}

		internal void SetPASatIncrement()
		{
			double num = (double)updn_SatSep[Data_and_Plots.SelectedSatellite].get_Value();
			if (num < 0.0)
			{
				updn_SatSep[Data_and_Plots.SelectedSatellite].set_Value(1m);
			}
			if (num < 5.0)
			{
				NumericUpDown obj = updn_SatPA[Data_and_Plots.SelectedSatellite];
				NumericUpDown obj2 = updn_SatPAUncert[Data_and_Plots.SelectedSatellite];
				decimal increment = 0.5m;
				obj2.set_Increment(increment);
				obj.set_Increment(increment);
				NumericUpDown obj3 = updn_SatPA[Data_and_Plots.SelectedSatellite];
				int decimalPlaces;
				updn_SatPAUncert[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(decimalPlaces = 1);
				obj3.set_DecimalPlaces(decimalPlaces);
			}
			else if (num < 10.0)
			{
				NumericUpDown obj4 = updn_SatPA[Data_and_Plots.SelectedSatellite];
				NumericUpDown obj5 = updn_SatPAUncert[Data_and_Plots.SelectedSatellite];
				decimal increment = 0.4m;
				obj5.set_Increment(increment);
				obj4.set_Increment(increment);
				NumericUpDown obj6 = updn_SatPA[Data_and_Plots.SelectedSatellite];
				int decimalPlaces;
				updn_SatPAUncert[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(decimalPlaces = 1);
				obj6.set_DecimalPlaces(decimalPlaces);
			}
			else if (num < 25.0)
			{
				NumericUpDown obj7 = updn_SatPA[Data_and_Plots.SelectedSatellite];
				NumericUpDown obj8 = updn_SatPAUncert[Data_and_Plots.SelectedSatellite];
				decimal increment = 0.2m;
				obj8.set_Increment(increment);
				obj7.set_Increment(increment);
				NumericUpDown obj9 = updn_SatPA[Data_and_Plots.SelectedSatellite];
				int decimalPlaces;
				updn_SatPAUncert[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(decimalPlaces = 1);
				obj9.set_DecimalPlaces(decimalPlaces);
			}
			else if (num < 50.0)
			{
				NumericUpDown obj10 = updn_SatPA[Data_and_Plots.SelectedSatellite];
				NumericUpDown obj11 = updn_SatPAUncert[Data_and_Plots.SelectedSatellite];
				decimal increment = 0.1m;
				obj11.set_Increment(increment);
				obj10.set_Increment(increment);
				NumericUpDown obj12 = updn_SatPA[Data_and_Plots.SelectedSatellite];
				int decimalPlaces;
				updn_SatPAUncert[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(decimalPlaces = 1);
				obj12.set_DecimalPlaces(decimalPlaces);
			}
			else
			{
				NumericUpDown obj13 = updn_SatPA[Data_and_Plots.SelectedSatellite];
				NumericUpDown obj14 = updn_SatPAUncert[Data_and_Plots.SelectedSatellite];
				decimal increment = 0.05m;
				obj14.set_Increment(increment);
				obj13.set_Increment(increment);
				NumericUpDown obj15 = updn_SatPA[Data_and_Plots.SelectedSatellite];
				int decimalPlaces;
				updn_SatPAUncert[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(decimalPlaces = 2);
				obj15.set_DecimalPlaces(decimalPlaces);
			}
		}

		internal void SetSepSatIncrement()
		{
			double num = (double)updn_SatSep[Data_and_Plots.SelectedSatellite].get_Value();
			if (num < 0.0)
			{
				updn_SatSep[Data_and_Plots.SelectedSatellite].set_Value(1m);
			}
			else if (num < 5.0)
			{
				NumericUpDown obj = updn_SatSep[Data_and_Plots.SelectedSatellite];
				NumericUpDown obj2 = updn_SatSepUncert[Data_and_Plots.SelectedSatellite];
				decimal increment = 0.02m;
				obj2.set_Increment(increment);
				obj.set_Increment(increment);
				NumericUpDown obj3 = updn_SatSep[Data_and_Plots.SelectedSatellite];
				int decimalPlaces;
				updn_SatSepUncert[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(decimalPlaces = 2);
				obj3.set_DecimalPlaces(decimalPlaces);
			}
			else if (num < 20.0)
			{
				NumericUpDown obj4 = updn_SatSep[Data_and_Plots.SelectedSatellite];
				NumericUpDown obj5 = updn_SatSepUncert[Data_and_Plots.SelectedSatellite];
				decimal increment = 0.1m;
				obj5.set_Increment(increment);
				obj4.set_Increment(increment);
				NumericUpDown obj6 = updn_SatSep[Data_and_Plots.SelectedSatellite];
				int decimalPlaces;
				updn_SatSepUncert[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(decimalPlaces = 1);
				obj6.set_DecimalPlaces(decimalPlaces);
			}
			else if (num < 50.0)
			{
				NumericUpDown obj7 = updn_SatSep[Data_and_Plots.SelectedSatellite];
				decimal increment;
				updn_SatSepUncert[Data_and_Plots.SelectedSatellite].set_Increment(increment = 1m);
				obj7.set_Increment(increment);
				NumericUpDown obj8 = updn_SatSep[Data_and_Plots.SelectedSatellite];
				int decimalPlaces;
				updn_SatSepUncert[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(decimalPlaces = 1);
				obj8.set_DecimalPlaces(decimalPlaces);
			}
			else
			{
				NumericUpDown obj9 = updn_SatSep[Data_and_Plots.SelectedSatellite];
				NumericUpDown obj10 = updn_SatSepUncert[Data_and_Plots.SelectedSatellite];
				decimal increment = 2m;
				obj10.set_Increment(increment);
				obj9.set_Increment(increment);
				NumericUpDown obj11 = updn_SatSep[Data_and_Plots.SelectedSatellite];
				int decimalPlaces;
				updn_SatSepUncert[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(decimalPlaces = 1);
				obj11.set_DecimalPlaces(decimalPlaces);
			}
		}

		private void SAT_PAchange(object sender, EventArgs e)
		{
			if (!SatFieldsChanging)
			{
				SatFieldsChanging = true;
				SetPASatIncrement();
				SetSatelliteFields();
				RePlotAfterChanges();
				SatFieldsChanging = false;
				((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			}
		}

		private void SAT_PAUncertchange(object sender, EventArgs e)
		{
			if (!SatFieldsChanging)
			{
				SatFieldsChanging = true;
				SetSatelliteFields();
				RePlotAfterChanges();
				SatFieldsChanging = false;
				((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			}
		}

		private void SAT_A_change(object sender, EventArgs e)
		{
			if (!SatFieldsChanging)
			{
				SatFieldsChanging = true;
				Set_Sat_AaxisPrecision();
				SatFieldsChanging = false;
				((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			}
		}

		private void Set_Sat_AaxisPrecision()
		{
			double num = (double)updn_ASat[Data_and_Plots.SelectedSatellite].get_Value();
			if (num < 1.0)
			{
				updn_ASat[Data_and_Plots.SelectedSatellite].set_Increment(0.001m);
				updn_ASat[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(3);
			}
			else if (num < 2.0)
			{
				updn_ASat[Data_and_Plots.SelectedSatellite].set_Increment(0.01m);
				updn_ASat[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(2);
			}
			else if (num < 5.0)
			{
				updn_ASat[Data_and_Plots.SelectedSatellite].set_Increment(0.05m);
				updn_ASat[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(2);
			}
			else
			{
				updn_ASat[Data_and_Plots.SelectedSatellite].set_Increment(1m);
				updn_ASat[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(1);
			}
			SetSatelliteFields();
			RePlotAfterChanges();
		}

		private void SAT_B_change(object sender, EventArgs e)
		{
			if (!SatFieldsChanging)
			{
				SatFieldsChanging = true;
				Set_Sat_BaxisPrecision();
				SatFieldsChanging = false;
				((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			}
		}

		private void Set_Sat_BaxisPrecision()
		{
			double num = (double)updn_BSat[Data_and_Plots.SelectedSatellite].get_Value();
			if (num < 1.0)
			{
				updn_BSat[Data_and_Plots.SelectedSatellite].set_Increment(0.001m);
				updn_BSat[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(3);
			}
			else if (num < 2.0)
			{
				updn_BSat[Data_and_Plots.SelectedSatellite].set_Increment(0.01m);
				updn_BSat[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(2);
			}
			else if (num < 5.0)
			{
				updn_BSat[Data_and_Plots.SelectedSatellite].set_Increment(0.05m);
				updn_BSat[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(2);
			}
			else
			{
				updn_BSat[Data_and_Plots.SelectedSatellite].set_Increment(1m);
				updn_BSat[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(1);
			}
			SetSatelliteFields();
			RePlotAfterChanges();
		}

		private void SAT_PA_change(object sender, EventArgs e)
		{
			if (!SatFieldsChanging)
			{
				SatFieldsChanging = true;
				double num = (double)updn_ASat[Data_and_Plots.SelectedSatellite].get_Value();
				if (num < 0.5)
				{
					updn_SatPA[Data_and_Plots.SelectedSatellite].set_Increment(2m);
					updn_SatPA[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(0);
				}
				else if (num < 5.0)
				{
					updn_SatPA[Data_and_Plots.SelectedSatellite].set_Increment(1m);
					updn_SatPA[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(0);
				}
				else
				{
					updn_SatPA[Data_and_Plots.SelectedSatellite].set_Increment(0.5m);
					updn_SatPA[Data_and_Plots.SelectedSatellite].set_DecimalPlaces(1);
				}
				SetSatelliteFields();
				RePlotAfterChanges();
				SatFieldsChanging = false;
				((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			}
		}

		private void ChordsChange(object sender, EventArgs e)
		{
			if (!SatFieldsChanging)
			{
				SatFieldsChanging = true;
				SetSatelliteFields();
				RePlotAfterChanges();
				SatFieldsChanging = false;
				((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			}
		}

		internal void SetSatelliteFields()
		{
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].SatelliteSeparation = (double)updn_SatSep[Data_and_Plots.SelectedSatellite].get_Value();
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].SatellitePA_Apparent = (double)updn_SatPA[Data_and_Plots.SelectedSatellite].get_Value();
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].Sat_Sep_Uncertainty = (double)updn_SatSepUncert[Data_and_Plots.SelectedSatellite].get_Value();
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].Sat_PA_Uncertainty = (double)updn_SatPAUncert[Data_and_Plots.SelectedSatellite].get_Value();
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].SatellitePA_2000 = EventDetails.Satellites[Data_and_Plots.SelectedSatellite].SatellitePA_Apparent + Utilities.PrecessionalRotationToJ2000_deg(EventDetails.JD_EventDate, EventDetails.RA_Star_Apparent, EventDetails.Dec_Star_Apparent);
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].MajorAxisSatellite = (double)updn_ASat[Data_and_Plots.SelectedSatellite].get_Value();
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].MinorAxisSatellite = (double)updn_BSat[Data_and_Plots.SelectedSatellite].get_Value();
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].PAAxisSatellite = (double)updn_PASat[Data_and_Plots.SelectedSatellite].get_Value();
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].SatelliteQuality = ((ListControl)cmb_SatFitQuality[Data_and_Plots.SelectedSatellite]).get_SelectedIndex();
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].NumberOfChords = (int)updn_NumChords[Data_and_Plots.SelectedSatellite].get_Value();
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].CompanionIAUname = ((Control)txtSatID[Data_and_Plots.SelectedSatellite]).get_Text();
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].CBET_Discovery = ((Control)txtCBET[Data_and_Plots.SelectedSatellite]).get_Text();
			double.TryParse(((Control)txtSat_dRA[Data_and_Plots.SelectedSatellite]).get_Text(), out var result);
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].Sat_dRA_mas = result;
			double.TryParse(((Control)txtSat_dDec[Data_and_Plots.SelectedSatellite]).get_Text(), out result);
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].Sat_dDec_mas = result;
			double.TryParse(((Control)txtSat_d2RA[Data_and_Plots.SelectedSatellite]).get_Text(), out result);
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].Sat_d2RA_mas = result;
			double.TryParse(((Control)txtSat_d2Dec[Data_and_Plots.SelectedSatellite]).get_Text(), out result);
			EventDetails.Satellites[Data_and_Plots.SelectedSatellite].Sat_d2Dec_mas = result;
			double num = (double)updn_SatSep[Data_and_Plots.SelectedSatellite].get_Value() / 1000.0 / EventDetails.Parallax * 6378.137;
			double num2 = (double)updn_SatPA[Data_and_Plots.SelectedSatellite].get_Value();
			double num3 = num * Math.Sin(num2 / (180.0 / Math.PI));
			double num4 = num * Math.Cos(num2 / (180.0 / Math.PI));
			double num5 = Math.Sqrt(num3 * num3 + num4 * num4);
			string text = ".##";
			if (num5 >= 100.0)
			{
				text = "";
			}
			else if (num5 >= 10.0)
			{
				text = ".#";
			}
			string format = "Offsets  dX = {0:+0" + text + ";-0" + text + "} km, dY = {1:+0" + text + ";-0" + text + "} km, dR = {2:0" + text + "} km";
			((Control)lbl_SatXYoffset[Data_and_Plots.SelectedSatellite]).set_Text(string.Format(format, num3, num4, num5));
		}

		internal void SetSatelliteControls()
		{
			for (int i = 0; i < 4; i++)
			{
				updn_SatSep[i].set_Value((decimal)EventDetails.Satellites[i].SatelliteSeparation);
				updn_SatPA[i].set_Value((decimal)EventDetails.Satellites[i].SatellitePA_Apparent);
				updn_SatSepUncert[i].set_Value((decimal)EventDetails.Satellites[i].Sat_Sep_Uncertainty);
				updn_SatPAUncert[i].set_Value((decimal)EventDetails.Satellites[i].Sat_PA_Uncertainty);
				updn_ASat[i].set_Value((decimal)EventDetails.Satellites[i].MajorAxisSatellite);
				updn_BSat[i].set_Value((decimal)EventDetails.Satellites[i].MinorAxisSatellite);
				updn_PASat[i].set_Value((decimal)EventDetails.Satellites[i].PAAxisSatellite);
				((ListControl)cmb_SatFitQuality[i]).set_SelectedIndex(EventDetails.Satellites[i].SatelliteQuality);
				((Control)txtSatID[i]).set_Text(EventDetails.Satellites[i].CompanionIAUname);
				chkEditCBET.set_Checked(false);
				((Control)txtCBET[i]).set_Text(EventDetails.Satellites[i].CBET_Discovery);
				updn_NumChords[i].set_Value((decimal)EventDetails.Satellites[i].NumberOfChords);
				((Control)txtSat_dRA[i]).set_Text(string.Format("{0,1:f1}", EventDetails.Satellites[i].Sat_dRA_mas));
				((Control)txtSat_dDec[i]).set_Text(string.Format("{0,1:f1}", EventDetails.Satellites[i].Sat_dDec_mas));
				((Control)txtSat_d2RA[i]).set_Text(string.Format("{0,1:f1}", EventDetails.Satellites[i].Sat_d2RA_mas));
				((Control)txtSat_d2Dec[i]).set_Text(string.Format("{0,1:f1}", EventDetails.Satellites[i].Sat_d2Dec_mas));
				chkCompanionPA.set_Checked(EventDetails.Solve_CompanionPA);
				chkCompanionSep.set_Checked(EventDetails.Solve_CompanionSep);
				Set_Sat_AaxisPrecision();
				Set_Sat_BaxisPrecision();
			}
			((Control)lblSatelliteNumbers).set_Text("of " + EventDetails.NumberOfSatellites);
			if (EventDetails.NumberOfSatellites == 0)
			{
				AddNewSatellite();
			}
		}

		internal void SetSatelliteControls_Visible(int SatNum)
		{
			for (int i = 0; i < 4; i++)
			{
				NumericUpDown obj = updn_SatPA[i];
				NumericUpDown obj2 = updn_SatSep[i];
				NumericUpDown obj3 = updn_SatPAUncert[i];
				NumericUpDown obj4 = updn_SatSepUncert[i];
				NumericUpDown obj5 = updn_ASat[i];
				NumericUpDown obj6 = updn_BSat[i];
				NumericUpDown obj7 = updn_PASat[i];
				ComboBox obj8 = cmb_SatFitQuality[i];
				NumericUpDown obj9 = updn_NumChords[i];
				TextBox obj10 = txtSatID[i];
				TextBox obj11 = txtSat_dRA[i];
				TextBox obj12 = txtSat_dDec[i];
				TextBox obj13 = txtSat_d2RA[i];
				TextBox obj14 = txtSat_d2Dec[i];
				Label obj15 = lbl_SatXYoffset[i];
				bool flag;
				((Control)txtCBET[i]).set_Visible(flag = i == SatNum);
				bool flag2;
				((Control)obj15).set_Visible(flag2 = flag);
				bool flag3;
				((Control)obj14).set_Visible(flag3 = flag2);
				bool flag4;
				((Control)obj13).set_Visible(flag4 = flag3);
				bool flag5;
				((Control)obj12).set_Visible(flag5 = flag4);
				bool flag6;
				((Control)obj11).set_Visible(flag6 = flag5);
				bool flag7;
				((Control)obj10).set_Visible(flag7 = flag6);
				bool flag8;
				((Control)obj9).set_Visible(flag8 = flag7);
				bool flag9;
				((Control)obj8).set_Visible(flag9 = flag8);
				bool flag10;
				((Control)obj7).set_Visible(flag10 = flag9);
				bool flag11;
				((Control)obj6).set_Visible(flag11 = flag10);
				bool flag12;
				((Control)obj5).set_Visible(flag12 = flag11);
				bool flag13;
				((Control)obj4).set_Visible(flag13 = flag12);
				bool flag14;
				((Control)obj3).set_Visible(flag14 = flag13);
				bool visible;
				((Control)obj2).set_Visible(visible = flag14);
				((Control)obj).set_Visible(visible);
			}
		}

		private void CheckReview(object sender, EventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			if (((Control)(CheckBox)sender).get_Text().Contains(chkReviewZero_Label))
			{
				ExpandReviewList();
				return;
			}
			((CheckBox)sender).set_Checked(!((CheckBox)sender).get_Checked());
			EventDetails.FlagForReview = RetrieveValueOf_ReviewList();
			SetInitialReviewTextColor();
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void SetInitialReviewTextColor()
		{
			try
			{
				chkReviewList[0].set_Checked(EventDetails.FlagForReview > 0);
				int num = 0;
				for (int i = 1; i < chkReviewList.Length; i++)
				{
					if (chkReviewList[i].get_Checked())
					{
						num++;
					}
				}
				((Control)chkReviewList[0]).set_Text(num + chkReviewZero_Label);
				if (!chkReviewList[0].get_Checked())
				{
					((Control)chkReviewList[0]).set_BackColor(Color.LightCyan);
				}
				else
				{
					((Control)chkReviewList[0]).set_BackColor(Color.Thistle);
				}
			}
			catch
			{
			}
		}

		internal void Populate_ReviewList(int value)
		{
			try
			{
				for (int i = 1; i < chkReviewList.Length; i++)
				{
					int num = (int)Math.Pow(2.0, i - 1);
					if ((value & num) == num)
					{
						chkReviewList[i].set_Checked(true);
					}
					else
					{
						chkReviewList[i].set_Checked(false);
					}
				}
			}
			catch
			{
			}
			SetInitialReviewTextColor();
		}

		internal int RetrieveValueOf_ReviewList()
		{
			int num = 0;
			for (int i = 1; i < Tags.chkReview_Labels.Length; i++)
			{
				if (chkReviewList[i].get_Checked())
				{
					num += (int)Math.Pow(2.0, i - 1);
				}
			}
			return num;
		}

		internal void ExpandReviewList()
		{
			((Control)pnlReview).set_Height(19 * Tags.chkReview_Labels.Length + 3);
		}

		internal void CollapseReviewList()
		{
			((Control)pnlReview).set_Height(21);
		}

		private void pnlReview_Leave(object sender, EventArgs e)
		{
			CollapseReviewList();
		}

		private void TransferModels(object sender, EventArgs e)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			if (Asteroid_Observations_Reports.Display_ShapeModels != null)
			{
				TransferImages(((Control)(Button)sender).get_Tag().ToString());
			}
		}

		internal void TransferImages(string Tag)
		{
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d8: Invalid comparison between Unknown and I4
			int num = int.Parse(((Control)Asteroid_Observations_Reports.Display_ShapeModels.txtPhaseOffset).get_Text().Replace("°", "").Trim());
			int num2 = 12;
			int transferButtons = 0;
			if (((Control)Asteroid_Observations_Reports.Display_ShapeModels.pnlPhaseChange).get_Visible())
			{
				num2 = Asteroid_Observations_Reports.Display_ShapeModels.NumPhaseImages;
			}
			for (int i = 0; i < num2; i++)
			{
				((Control)cmdTransferImage[i]).set_Font(new Font("Microsoft Sans Serif", 7f));
			}
			for (int j = 0; j < num2; j++)
			{
				if (!(Tag == (j + 1).ToString()))
				{
					continue;
				}
				((Control)cmdTransferImage[j]).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold));
				transferButtons = j;
				if (UnsavedShapeChanges && (int)MessageBox.Show("You have made changes to this Model fit that have not been saved or updated. Changing the shape model will lose the fit values.\r\n\r\nDo you want to continue changing the model?", "Unsaved changes", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
				if (((Control)Asteroid_Observations_Reports.Display_ShapeModels.pnlPhaseChange).get_Visible())
				{
					if (((Control)Asteroid_Observations_Reports.Display_ShapeModels.lblPhaseDownloadStatus[j]).get_Text() != "Available")
					{
						return;
					}
					Asteroid_Observations_Reports.Display_ShapeModels.TransferPhaseImage(j);
					num = int.Parse(((Control)Asteroid_Observations_Reports.Display_ShapeModels.cmdPhaseModel[j]).get_Text().Replace("Ph:", "").Replace("°", "")
						.Trim());
					((Control)cmdSetModelMin).set_Enabled(true);
					break;
				}
				DisplayShapeModels.Currently_Displayed_Model = j;
				Button obj = cmdGetLightCurves;
				bool enabled;
				((Control)cmdSetModelMax).set_Enabled(enabled = true);
				((Control)obj).set_Enabled(enabled);
				if (j > 5)
				{
					TextBox obj2 = txtModelMaxDia;
					string text;
					((Control)txtModelMinDia).set_Text(text = "");
					((Control)obj2).set_Text(text);
				}
				((Control)cmdGetLightCurves).set_Enabled(j < 6);
				if (((Control)cmdGetLightCurves).get_Enabled())
				{
					((Control)cmdGetLightCurves).set_BackColor(Color.PaleTurquoise);
				}
				else
				{
					((Control)cmdGetLightCurves).set_BackColor(Color.LightGray);
				}
				if (j > 11 || (j < 6 && ((Control)Asteroid_Observations_Reports.Display_ShapeModels.lblDownloadStatus[j, 0]).get_Text() != "Yes") || (j > 5 && ((Control)Asteroid_Observations_Reports.Display_ShapeModels.lblDownloadStatus[j - 6, 1]).get_Text() != "Yes"))
				{
					break;
				}
				if (DisplayShapeModels.Currently_Displayed_Model < 6)
				{
					((Control)Data_and_Plots.PlotForm.txtModelSource).set_Text("DAMIT");
				}
				else
				{
					((Control)Data_and_Plots.PlotForm.txtModelSource).set_Text("ISAM");
				}
				((Control)Data_and_Plots.PlotForm.txtModelID).set_Text(DisplayShapeModels.ModelParameters[DisplayShapeModels.Currently_Displayed_Model].ModelNumber_String);
				((Control)Data_and_Plots.PlotForm.txtVersion).set_Text(DisplayShapeModels.ModelParameters[DisplayShapeModels.Currently_Displayed_Model].VersionComments);
				((Control)Data_and_Plots.PlotForm.txtModelPhase).set_Text("0°");
				((Control)Data_and_Plots.PlotForm.txtSurface_Volume_Ratio).set_Text(string.Format("{0,1:f3}", DisplayShapeModels.ModelParameters[DisplayShapeModels.Currently_Displayed_Model].SurfaceToVolumeRatio));
				if (j < Asteroid_Observations_Reports.Display_ShapeModels.NumDAMITImages)
				{
					if (((Control)Asteroid_Observations_Reports.Display_ShapeModels.cmdGetModel[j, 0]).get_Enabled())
					{
						((Control)Asteroid_Observations_Reports.Display_ShapeModels).set_Enabled(true);
						Asteroid_Observations_Reports.Display_ShapeModels.TransferImage(j);
						DisplayExistingSolutionForCurrentModel(((Control)txtModelSource).get_Text(), ((Control)txtModelID).get_Text());
						((Control)cmdSaveModelInfo).set_Text("Save/update\r\nmodel " + ((Control)txtModelID).get_Text());
						if (Asteroid_Observations_Reports.Display_ShapeModels.ISAMEquivalent[j] >= 0)
						{
							Button obj3 = cmdSaveModelInfo;
							((Control)obj3).set_Text(((Control)obj3).get_Text() + "\r\n&& " + DisplayShapeModels.ModelParameters[Asteroid_Observations_Reports.Display_ShapeModels.ISAMEquivalent[j] + 6].ModelNumber_String);
						}
						break;
					}
					((Control)Asteroid_Observations_Reports.Display_ShapeModels).set_Enabled(false);
				}
				else
				{
					if (((j - 6 + Asteroid_Observations_Reports.Display_ShapeModels.NumDAMITImages >= 0) & (j - 6 + Asteroid_Observations_Reports.Display_ShapeModels.NumDAMITImages < DisplayShapeModels.Images.Count)) && ((Control)Asteroid_Observations_Reports.Display_ShapeModels.cmdGetModel[j - 6, 1]).get_Enabled())
					{
						int imageNumber = j - 6 + Asteroid_Observations_Reports.Display_ShapeModels.NumDAMITImages;
						Asteroid_Observations_Reports.Display_ShapeModels.TransferImage(imageNumber);
						DisplayExistingSolutionForCurrentModel(((Control)txtModelSource).get_Text(), ((Control)txtModelID).get_Text());
						((Control)cmdSaveModelInfo).set_Text("Save/update\r\nmodel " + ((Control)txtModelID).get_Text());
						break;
					}
					((Control)Asteroid_Observations_Reports.Display_ShapeModels).set_Enabled(false);
				}
			}
			((Control)txtModelPhase).set_Text(num + "°");
			UnsavedShapeChanges = false;
			((Control)cmdSaveModelInfo).set_Enabled(false);
			((Control)cmdSaveModelInfo).set_BackColor(Color.Pink);
			SetTransferButtons(transferButtons);
		}

		internal void SetTransferButtons(int SelectedImage)
		{
			bool flag = false;
			try
			{
				flag = ((Control)Asteroid_Observations_Reports.Display_ShapeModels.pnlPhaseChange).get_Visible();
			}
			catch
			{
			}
			if (Asteroid_Observations_Reports.Display_ShapeModels != null)
			{
				for (int i = Asteroid_Observations_Reports.Display_ShapeModels.NumPhaseImages; i < 12; i++)
				{
					((Control)cmdTransferImage[i]).set_Visible(!((Control)Asteroid_Observations_Reports.Display_ShapeModels.pnlPhaseChange).get_Visible());
				}
				if (flag)
				{
					((Control)label11).set_Text("Phase");
					((Control)label12).set_Visible(false);
					for (int j = 0; j < Asteroid_Observations_Reports.Display_ShapeModels.NumPhaseImages; j++)
					{
						((Control)cmdTransferImage[j]).set_Left(44 + 20 * j);
						((Control)cmdTransferImage[j]).set_Top(1);
						((Control)cmdTransferImage[j]).set_Text(j.ToString());
						((Control)cmdTransferImage[j]).set_Enabled(true);
						((Control)cmdTransferImage[j]).set_BackColor(((Control)Asteroid_Observations_Reports.Display_ShapeModels.cmdPhaseModel[j]).get_BackColor());
						if (((Control)Asteroid_Observations_Reports.Display_ShapeModels.lblPhaseDownloadStatus[j]).get_Text() == "Failed")
						{
							((Control)cmdTransferImage[j]).set_ForeColor(Color.Red);
						}
						else
						{
							((Control)cmdTransferImage[j]).set_ForeColor(Control.get_DefaultForeColor());
						}
						((Control)cmdGetLightCurves).set_Visible(false);
					}
				}
				else
				{
					((Control)label11).set_Text("DAMIT");
					((Control)label12).set_Visible(true);
					((Control)cmdGetLightCurves).set_Visible(true);
					for (int k = 0; k < 12; k++)
					{
						((Control)cmdTransferImage[k]).set_Top(1);
						if (k < 6)
						{
							((Control)cmdTransferImage[k]).set_Left(44 + 18 * k);
							((Control)cmdTransferImage[k]).set_Text((k + 1).ToString());
						}
						else
						{
							((Control)cmdTransferImage[k]).set_Left(268 + 24 * (k - 6));
							((Control)cmdTransferImage[k]).set_Text((k - 5).ToString());
						}
						((Control)cmdTransferImage[k]).set_Visible(true);
						((Control)cmdTransferImage[k]).set_Enabled(true);
					}
					for (int l = 0; l < 6; l++)
					{
						((Control)cmdTransferImage[l]).set_BackColor(((Control)Asteroid_Observations_Reports.Display_ShapeModels.cmdGetModel[l, 0]).get_BackColor());
						((Control)cmdTransferImage[l + 6]).set_BackColor(((Control)Asteroid_Observations_Reports.Display_ShapeModels.cmdGetModel[l, 1]).get_BackColor());
						((Control)cmdTransferImage[l]).set_Enabled(((Control)Asteroid_Observations_Reports.Display_ShapeModels.cmdGetModel[l, 0]).get_Enabled());
						((Control)cmdTransferImage[l + 6]).set_Enabled(((Control)Asteroid_Observations_Reports.Display_ShapeModels.cmdGetModel[l, 1]).get_Enabled());
						((Control)lblISAMmatch[l]).set_Visible(((Control)cmdTransferImage[l + 6]).get_Enabled());
						if (Asteroid_Observations_Reports.Display_ShapeModels.DamitEquivalent[l] >= 0)
						{
							((Control)lblISAMmatch[l]).set_Text((Asteroid_Observations_Reports.Display_ShapeModels.DamitEquivalent[l] + 1).ToString());
						}
						else
						{
							((Control)lblISAMmatch[l]).set_Text("");
						}
						if (((Control)Asteroid_Observations_Reports.Display_ShapeModels.lblDownloadStatus[l, 0]).get_Text() == "Failed")
						{
							((Control)cmdTransferImage[l]).set_ForeColor(Color.Red);
						}
						else
						{
							((Control)cmdTransferImage[l]).set_ForeColor(Control.get_DefaultForeColor());
						}
						if (((Control)Asteroid_Observations_Reports.Display_ShapeModels.lblDownloadStatus[l, 1]).get_Text() == "Failed")
						{
							((Control)cmdTransferImage[l + 4]).set_ForeColor(Color.Red);
						}
						else
						{
							((Control)cmdTransferImage[l + 6]).set_ForeColor(Control.get_DefaultForeColor());
						}
					}
				}
				SetTransferButtonColors(SelectedImage);
			}
			SetScale();
		}

		internal void SetTransferButtonColors(int SelectedImage)
		{
			if (((Control)Asteroid_Observations_Reports.Display_ShapeModels.pnlPhaseChange).get_Visible())
			{
				for (int i = 0; i < 12; i++)
				{
					((Control)cmdTransferImage[i]).set_BackColor(Color.LightPink);
					if (i == SelectedImage)
					{
						((Control)cmdTransferImage[i]).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold));
					}
					else
					{
						((Control)cmdTransferImage[i]).set_Font(new Font("Microsoft Sans Serif", 7f));
					}
				}
				return;
			}
			for (int j = 0; j < 6; j++)
			{
				((Control)cmdTransferImage[j]).set_BackColor(Color.LightYellow);
				((Control)cmdTransferImage[j + 6]).set_BackColor(Color.LightYellow);
			}
			for (int k = 0; k < 12; k++)
			{
				((Control)cmdTransferImage[k]).set_Font(new Font("Microsoft Sans Serif", 7f));
				bool flag = false;
				for (int l = 0; l < EventDetails.ShapeData.Count; l++)
				{
					if (((Control)cmdTransferImage[k]).get_Enabled() && EventDetails.ShapeData[l].ID == DisplayShapeModels.ModelParameters[k].ModelNumber_String)
					{
						flag = true;
						break;
					}
				}
				if (flag)
				{
					if (k == SelectedImage)
					{
						((Control)cmdTransferImage[k]).set_BackColor(Color.Lime);
						((Control)cmdTransferImage[k]).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold));
					}
					else
					{
						((Control)cmdTransferImage[k]).set_BackColor(Color.MediumAquamarine);
					}
				}
				else if (((Control)cmdTransferImage[k]).get_Enabled())
				{
					if (k == SelectedImage)
					{
						((Control)cmdTransferImage[k]).set_BackColor(Color.LightPink);
						((Control)cmdTransferImage[k]).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold));
					}
					else
					{
						((Control)cmdTransferImage[k]).set_BackColor(Color.Plum);
					}
				}
				else
				{
					((Control)cmdTransferImage[k]).set_BackColor(Control.get_DefaultBackColor());
				}
			}
		}

		private void PlotObservations_Resize(object sender, EventArgs e)
		{
			if (IsResizing)
			{
				return;
			}
			IsResizing = true;
			((Control)this).SuspendLayout();
			if (!((((Control)this).get_Width() < 300) | (((Control)this).get_Height() < 120)))
			{
				((Control)picPlot).set_Height(((Control)this).get_Height() - 78);
				((Control)picPlot).set_Width(((Control)picPlot).get_Height());
				((Control)Vbar).set_Top(((Control)picPlot).get_Top());
				((Control)Vbar).set_Left(((Control)picPlot).get_Width() + ((Control)picPlot).get_Left());
				((Control)Vbar).set_Height(((Control)picPlot).get_Height());
				((Control)Hbar).set_Left(((Control)picPlot).get_Left());
				((Control)Hbar).set_Top(((Control)picPlot).get_Height() + ((Control)picPlot).get_Top());
				((Control)Hbar).set_Width(((Control)picPlot).get_Width());
				((Control)grpBestFit).set_Left(((Control)Vbar).get_Left() + 20);
				((Control)grpBestFit).set_Height(((Control)picPlot).get_Height() + ((Control)picPlot).get_Top() - ((Control)grpBestFit).get_Top());
				((Control)lstObservers).set_Height(((Control)picPlot).get_Height() + ((Control)picPlot).get_Top() - ((Control)lstObservers).get_Top() - 40);
				((Control)PanelLegend).set_Height(((Control)lstObservers).get_Height());
				((Control)PanelLegend).set_Width(((Control)lstObservers).get_Width());
				if (((Control)panelDouble).get_Left() > ((Control)this).get_Width() - 40)
				{
					((Control)panelDouble).set_Left(((Control)this).get_Width() - 40);
				}
				if (((Control)panelDouble).get_Top() > ((Control)this).get_Height() - 90)
				{
					((Control)panelDouble).set_Top(((Control)this).get_Height() - 90);
				}
				if (((Control)PanelSatellites).get_Left() > ((Control)this).get_Width() - 40)
				{
					((Control)PanelSatellites).set_Left(((Control)this).get_Width() - 40);
				}
				if (((Control)PanelSatellites).get_Top() > ((Control)this).get_Height() - 70)
				{
					((Control)PanelSatellites).set_Top(((Control)this).get_Height() - 70);
				}
				Settings.Default.SizeAsterPlot = (Point)((Form)this).get_Size();
				if (!Updating)
				{
					Data_and_Plots.PlotEventOnScreen();
				}
				IsResizing = false;
				((Control)this).ResumeLayout();
			}
		}

		private void PlotObservations_ResizeBegin(object sender, EventArgs e)
		{
			((Control)this).SuspendLayout();
		}

		private void PlotObservations_ResizeEnd(object sender, EventArgs e)
		{
			((Control)this).ResumeLayout();
		}

		internal void ResizeToWidthOfImage(int ImageWidth)
		{
			((Control)picPlot).set_Height(ImageWidth);
			((Control)picPlot).set_Width(ImageWidth);
		}

		private void updnX_ValueChanged(object sender, EventArgs e)
		{
			SetDiameterDecimalPlaces();
			RePlotAfterChanges();
		}

		private void updnY_ValueChanged(object sender, EventArgs e)
		{
			SetDiameterDecimalPlaces();
			RePlotAfterChanges();
		}

		private void updnA_ValueChanged(object sender, EventArgs e)
		{
			SetDiameterDecimalPlaces();
			SetEquivalentDiameter();
			RePlotAfterChanges();
		}

		private void updnB_ValueChanged(object sender, EventArgs e)
		{
			SetDiameterDecimalPlaces();
			SetEquivalentDiameter();
			RePlotAfterChanges();
		}

		private void SetEquivalentDiameter()
		{
			double num = Math.Sqrt((double)updnA.get_Value() * (double)updnB.get_Value());
			int num2 = AllEvents.DecimalPlaces_Int_InSolution_Output((double)updnB.get_Value());
			if (updnA.get_Value() > 200m)
			{
				num2 = 0;
			}
			((Control)lblDiaEquiv).set_Text(string.Format("dia = {0,1:f" + num2 + "} km", num));
		}

		internal void SetDiameterDecimalPlaces()
		{
			if (SetDecimalPlaces && !XYABplacesChanging)
			{
				XYABplacesChanging = true;
				int num = AllEvents.DecimalPlaces_Int_InSolution_Output((double)updnB.get_Value());
				decimal num2 = (decimal)Math.Pow(10.0, 1 - num);
				if (updnA.get_Increment() < num2)
				{
					updnA.set_Increment(num2);
				}
				if (updnB.get_Increment() < num2)
				{
					updnB.set_Increment(num2);
				}
				if (updnX.get_Increment() < num2)
				{
					updnX.set_Increment(num2);
				}
				if (updnY.get_Increment() < num2)
				{
					updnY.set_Increment(num2);
				}
				NumericUpDown obj = updnA;
				NumericUpDown obj2 = updnB;
				NumericUpDown obj3 = updnX;
				int num3;
				updnY.set_DecimalPlaces(num3 = num);
				int num4;
				obj3.set_DecimalPlaces(num4 = num3);
				int decimalPlaces;
				obj2.set_DecimalPlaces(decimalPlaces = num4);
				obj.set_DecimalPlaces(decimalPlaces);
				updnA.set_Value(Math.Round(updnA.get_Value(), updnA.get_DecimalPlaces()));
				updnB.set_Value(Math.Round(updnB.get_Value(), updnB.get_DecimalPlaces()));
				updnX.set_Value(Math.Round(updnX.get_Value(), updnX.get_DecimalPlaces()));
				updnY.set_Value(Math.Round(updnY.get_Value(), updnY.get_DecimalPlaces()));
				XYABplacesChanging = false;
			}
		}

		private void updnPA_ValueChanged(object sender, EventArgs e)
		{
			if (updnPA.get_Value() > 360m)
			{
				NumericUpDown obj = updnPA;
				obj.set_Value(obj.get_Value() - 360m);
			}
			if (updnPA.get_Value() < 0m)
			{
				NumericUpDown obj2 = updnPA;
				obj2.set_Value(obj2.get_Value() + 360m);
			}
			RePlotAfterChanges();
		}

		internal void SetLabelsForDoubleOffsets()
		{
			((Control)lbl1).set_Text(string.Format("#1 ({0,1:f1},{1,1:f1})", Data_and_Plots.DoublesOffset_X[1], Data_and_Plots.DoublesOffset_Y[1]));
			((Control)lbl2).set_Text(string.Format("#2 ({0,1:f1},{1,1:f1})", Data_and_Plots.DoublesOffset_X[2], Data_and_Plots.DoublesOffset_Y[2]));
			((Control)lbl3).set_Text(string.Format("#3 ({0,1:f1},{1,1:f1})", Data_and_Plots.DoublesOffset_X[3], Data_and_Plots.DoublesOffset_Y[3]));
			((Control)lbl4).set_Text(string.Format("#4 ({0,1:f1},{1,1:f1})", Data_and_Plots.DoublesOffset_X[4], Data_and_Plots.DoublesOffset_Y[4]));
			if (EventDetails.Doubles.Count > 0)
			{
				double num3 = (EventDetails.Doubles[0].Offset_X = (EventDetails.Doubles[1].Offset_X = Data_and_Plots.DoublesOffset_X[1]));
				num3 = (EventDetails.Doubles[0].Offset_Y = (EventDetails.Doubles[1].Offset_Y = Data_and_Plots.DoublesOffset_Y[1]));
				num3 = (EventDetails.Doubles[2].Offset_X = (EventDetails.Doubles[3].Offset_X = Data_and_Plots.DoublesOffset_X[3]));
				num3 = (EventDetails.Doubles[2].Offset_Y = (EventDetails.Doubles[3].Offset_Y = Data_and_Plots.DoublesOffset_Y[3]));
				for (int i = 0; i < 4; i++)
				{
					EventDetails.Doubles[i].Centre_X = (double)updnX.get_Value();
					EventDetails.Doubles[i].Centre_Y = (double)updnY.get_Value();
				}
			}
		}

		internal void SetDoubleStarOffsetValues(decimal MidX, decimal MidY, decimal OffsetX, decimal OffsetY)
		{
			Data_and_Plots.PlotForm.updnX.set_Value(MidX);
			Data_and_Plots.PlotForm.updnX.set_Value(MidX);
			Data_and_Plots.PlotForm.updnY.set_Value(MidY);
			Data_and_Plots.DoublesOffset_X[1] = (Data_and_Plots.DoublesOffset_X[2] = (double)OffsetX);
			Data_and_Plots.DoublesOffset_Y[1] = (Data_and_Plots.DoublesOffset_Y[2] = (double)OffsetY);
			Data_and_Plots.DoublesOffset_X[3] = (Data_and_Plots.DoublesOffset_X[4] = 0.0 - (double)OffsetX);
			Data_and_Plots.DoublesOffset_Y[3] = (Data_and_Plots.DoublesOffset_Y[4] = 0.0 - (double)OffsetY);
			Data_and_Plots.PlotForm.SetLabelsForDoubleOffsets();
			CheckBox obj = Data_and_Plots.PlotForm.chkX;
			CheckBox obj2 = Data_and_Plots.PlotForm.chkY;
			CheckBox obj3 = Data_and_Plots.PlotForm.chkA;
			bool flag;
			Data_and_Plots.PlotForm.chkB.set_Checked(flag = false);
			bool flag2;
			obj3.set_Checked(flag2 = flag);
			bool @checked;
			obj2.set_Checked(@checked = flag2);
			obj.set_Checked(@checked);
			CheckBox obj4 = Data_and_Plots.PlotForm.chkCompanionPA;
			Data_and_Plots.PlotForm.chkCompanionSep.set_Checked(@checked = true);
			obj4.set_Checked(@checked);
		}

		internal bool DoubleStarOffsets_HaveBeenSet()
		{
			return (Data_and_Plots.DoublesOffset_X[1] != 0.0) & (Data_and_Plots.DoublesOffset_Y[1] != 0.0);
		}

		internal void NoOffsets_Message()
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show("You cannot proceed with the analysis of a 4-solution\r\nevent without first setting the offsets.", "No offsets set", (MessageBoxButtons)0, (MessageBoxIcon)16);
		}

		private void RePlotAfterChanges()
		{
			if (!InitialiseForm && !PreventUpdatingChanges && !Updating)
			{
				Updating = true;
				if (chkCircle.get_Checked())
				{
					updnB.set_Value(updnA.get_Value());
				}
				Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
				Data_and_Plots.GetVariations();
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.PlotEventOnScreen();
				SetRatioAndMag();
				Updating = false;
			}
		}

		internal void SetRatioAndMag()
		{
			if (updnB.get_Value() <= 0m)
			{
				double num = 1.0;
				((Control)lblAxisRatio).set_Text("a/b= NaN");
			}
			else
			{
				double num = (double)updnA.get_Value() / (double)updnB.get_Value();
				((Control)lblAxisRatio).set_Text(string.Format("a/b: {0,2:F2}, dMag: {1,2:f2}", num, 2.5 * Math.Log10(num)));
			}
		}

		private void cmbQuality_SelectedIndexChanged(object sender, EventArgs e)
		{
			EventDetails.Quality_From_cmbQuality = ((ListControl)cmbQuality).get_SelectedIndex();
			if (((ListControl)cmbQuality).get_SelectedIndex() == 0)
			{
				FlashTimer.Start();
			}
			else
			{
				FlashTimer.Stop();
				((Control)cmbQuality).set_BackColor(SystemColors.Window);
			}
			try
			{
				if (!((Control)Data_and_Plots.Observations_Editor.cmdUpdateHistorical).get_Visible())
				{
					if (((ListControl)cmbQuality).get_SelectedIndex() < 2)
					{
						((Control)Data_and_Plots.Observations_Editor.lblMPC).set_Text("Will not be reported to the MPC");
					}
					else
					{
						((Control)Data_and_Plots.Observations_Editor.lblMPC).set_Text("Not yet reported to the MPC");
					}
				}
			}
			catch
			{
			}
			string text = "";
			text = EventDetails.Quality switch
			{
				0 => "No reliable position or size\r\n\r\nUse when the observations are unreliable", 
				1 => "Astrometry only. No reliable size\r\n\r\nUse when the chords are insufficient to\r\nlocate the center of the asteroid, even when using the Assumed diameter.\r\n\r\nThe reported astrometry has large uncertainties related to the size of the asteroid", 
				2 => "Limits on size, but no shape\r\n\r\nUsed when the chords are sufficient to locate the center of \r\nthe asteroid - including when the diameter is the Assumed diameter", 
				3 => "Reliable size. Can fit to shape models\r\n\r\nUsed when there are sufficient chords to define the \r\nprofile of the asteroid. A shape model does not have to be available", 
				4 => "Resolution better than shape models\r\n\r\nUsed when there are many chords that define the \r\nprofile of the asteroid in some detail. A shape model \r\ndoes not have to be available", 
				5 => "Single frame event. No astrometry\r\n\r\nUsed when there is only one +'ve chord, and that chord is \r\nbased on a single-frame light drop. No astrometry will be reported", 
				6 => "Only 1 star of a double. No astrometry\r\n\r\nUsed when only one star of a double star detected.\r\nThat detection is based on a light drop being significantly\r\nless than expected, and not explainable by variation\r\nin the asteroid's brightness, or the presence of\r\nanother star in the measurement aperture.", 
				_ => "", 
			};
			toolTip.SetToolTip((Control)(object)cmbQuality, text);
		}

		private void FlashTimer_Tick(object sender, EventArgs e)
		{
			CurrentColor++;
			if (CurrentColor >= ColorCount)
			{
				CurrentColor = 0;
			}
			((Control)cmbQuality).set_BackColor(C[CurrentColor]);
		}

		private void FlashTimerDouble_Tick(object sender, EventArgs e)
		{
			CurrentColorDouble++;
			if (CurrentColorDouble >= ColorCountDouble)
			{
				CurrentColorDouble = 0;
			}
			((Control)cmdSetOffsets).set_BackColor(CDouble[CurrentColorDouble]);
		}

		internal void FlashTimerDouble_StartEnd()
		{
			if (!DoubleStarOffsets_HaveBeenSet())
			{
				FlashTimerDouble.Start();
				return;
			}
			FlashTimerDouble.Stop();
			((Control)cmdSetOffsets).set_BackColor(Color.Chartreuse);
		}

		private void chkShapeModelCentered_CheckedChanged(object sender, EventArgs e)
		{
			if (chkShapeModelCentered.get_Checked())
			{
				CheckBox obj = chkX;
				bool @checked;
				chkY.set_Checked(@checked = true);
				obj.set_Checked(@checked);
				CheckBox obj2 = chkCircle;
				chkUseAssumedDiameter.set_Checked(@checked = false);
				obj2.set_Checked(@checked);
				CheckBox obj3 = chkX;
				CheckBox obj4 = chkY;
				CheckBox obj5 = chkCircle;
				bool flag;
				((Control)chkUseAssumedDiameter).set_Enabled(flag = false);
				bool flag2;
				((Control)obj5).set_Enabled(flag2 = flag);
				((Control)obj4).set_Enabled(@checked = flag2);
				((Control)obj3).set_Enabled(@checked);
			}
			else
			{
				CheckBox obj6 = chkX;
				CheckBox obj7 = chkY;
				CheckBox obj8 = chkCircle;
				bool flag;
				((Control)chkUseAssumedDiameter).set_Enabled(flag = true);
				bool flag2;
				((Control)obj8).set_Enabled(flag2 = flag);
				bool @checked;
				((Control)obj7).set_Enabled(@checked = flag2);
				((Control)obj6).set_Enabled(@checked);
				((Control)chkShapeModelCentered).set_Enabled(false);
			}
		}

		private void chkX_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.GetVariations();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void chkY_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.GetVariations();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void chkA_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.GetVariations();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void chkB_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.GetVariations();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void chkPA_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.GetVariations();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void chkSep_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.GetVariations();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void chkPA2nd_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.GetVariations();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void Vbar_Scroll(object sender, ScrollEventArgs e)
		{
			if (!Updating)
			{
				Updating = true;
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.PlotEventOnScreen();
				Updating = false;
			}
		}

		private void Hbar_Scroll(object sender, ScrollEventArgs e)
		{
			if (!Updating)
			{
				Updating = true;
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.PlotEventOnScreen();
				Updating = false;
			}
		}

		private void SliderScale_ValueChanged(object sender, EventArgs e)
		{
			if (!Updating)
			{
				Updating = true;
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.PlotEventOnScreen();
				Updating = false;
			}
		}

		private void SliderScale_Scroll(object sender, EventArgs e)
		{
			if (!Updating)
			{
				Updating = true;
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.PlotEventOnScreen();
				Updating = false;
			}
		}

		private void chkDecreaseScale_CheckedChanged(object sender, EventArgs e)
		{
			if (!Updating)
			{
				Updating = true;
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.PlotEventOnScreen();
				Updating = false;
			}
		}

		private void PlotObservations_Enter(object sender, EventArgs e)
		{
		}

		private void PlotObservations_Activated(object sender, EventArgs e)
		{
			if (!((Control)panelDouble).get_ContainsFocus() && !((Control)panelShapeModelControl).get_ContainsFocus())
			{
				showPolarAxesToolStripMenuItem.set_Checked(IncludeAxisOfRotation_ShapeModel = Settings.Default.IncludeAxisOfRotation);
				includeAsterodIDAndModelIDToolStripMenuItem.set_Checked(DrawDamitID_ShapeModel = Settings.Default.DrawDamitID);
				showModelFacesToolStripMenuItem.set_Checked(ShowModelFaceEdges_ShapeModel = Settings.Default.ModelFaceEdges);
				plotModelDarkToolStripMenuItem.set_Checked(DarkShapeModel = Settings.Default.ModelDark);
				plotModelInGreyScaleToolStripMenuItem.set_Checked(BlackWhiteShapeModel = Settings.Default.ModelinBlackWhite);
				try
				{
					((ToolStripItem)shapeModelsToolStripMenuItem).set_Enabled(((Control)Data_and_Plots.Observations_Editor.lblInDAMIT).get_Visible() | ((Control)Data_and_Plots.Observations_Editor.lblInDAMIT).get_Visible());
				}
				catch
				{
				}
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.Observations_InitialisePlot_Using_EventDetails();
				Data_and_Plots.PlotEventOnScreen();
				SetDiameterDecimalPlaces();
				((Control)this).Focus();
			}
		}

		private void PlotObservations_Deactivate(object sender, EventArgs e)
		{
			Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
			Settings.Default.LocationAsterPlotObs = ((Form)this).get_Location();
		}

		private void mnuWhiteBackground_Click(object sender, EventArgs e)
		{
			mnuWhiteBackground.set_Checked(true);
			ToolStripMenuItem obj = mnuBlackBackground;
			bool @checked;
			mnuGrayBackground.set_Checked(@checked = false);
			obj.set_Checked(@checked);
			Settings.Default.ShapeModelBackColor = 1;
			if (Asteroid_Observations_Reports.Display_ShapeModels != null)
			{
				Asteroid_Observations_Reports.Display_ShapeModels.SetBackgroundChecks(1);
				Asteroid_Observations_Reports.Display_ShapeModels.SetBackground_White_Black(1);
			}
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuBlackBackground_Click(object sender, EventArgs e)
		{
			mnuBlackBackground.set_Checked(true);
			ToolStripMenuItem obj = mnuWhiteBackground;
			bool @checked;
			mnuGrayBackground.set_Checked(@checked = false);
			obj.set_Checked(@checked);
			Settings.Default.ShapeModelBackColor = 0;
			if (Asteroid_Observations_Reports.Display_ShapeModels != null)
			{
				Asteroid_Observations_Reports.Display_ShapeModels.SetBackgroundChecks(0);
				Asteroid_Observations_Reports.Display_ShapeModels.SetBackground_White_Black(0);
			}
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuGrayBackground_Click(object sender, EventArgs e)
		{
			mnuGrayBackground.set_Checked(true);
			ToolStripMenuItem obj = mnuWhiteBackground;
			bool @checked;
			mnuBlackBackground.set_Checked(@checked = false);
			obj.set_Checked(@checked);
			Settings.Default.ShapeModelBackColor = 2;
			if (Asteroid_Observations_Reports.Display_ShapeModels != null)
			{
				Asteroid_Observations_Reports.Display_ShapeModels.SetBackgroundChecks(2);
				Asteroid_Observations_Reports.Display_ShapeModels.SetBackground_White_Black(2);
			}
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuPathsInColour_Click(object sender, EventArgs e)
		{
			mnuPathsInColour.set_Checked(!mnuPathsInColour.get_Checked());
			Settings.Default.AsterPlot_ChordsIncolour = mnuPathsInColour.get_Checked();
			((Control)PanelLegend).set_Visible(mnuPathsInColour.get_Checked());
			((Control)lstObservers).set_Visible(!mnuPathsInColour.get_Checked());
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showAsteroidEllipseInColorToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showAsteroidEllipseInColorToolStripMenuItem.set_Checked(!showAsteroidEllipseInColorToolStripMenuItem.get_Checked());
			Settings.Default.AsterPlot_EllipseIncolour = showAsteroidEllipseInColorToolStripMenuItem.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showEventToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showEventInColorToolStripMenuItem.set_Checked(!showEventInColorToolStripMenuItem.get_Checked());
			Settings.Default.AsterPlot_EventsIncolour = showEventInColorToolStripMenuItem.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void applyWatermark_Click(object sender, EventArgs e)
		{
			applyWatermark.set_Checked(!applyWatermark.get_Checked());
			Settings.Default.AsterPlotWatermark = applyWatermark.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void applyProvisionalMarkToolStripMenuItem_Click(object sender, EventArgs e)
		{
			applyProvisionalMarkToolStripMenuItem.set_Checked(!applyProvisionalMarkToolStripMenuItem.get_Checked());
			Settings.Default.AsterPlotProvisional = applyProvisionalMarkToolStripMenuItem.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuThickLines_Click(object sender, EventArgs e)
		{
			mnuThickLines_Paths.set_Checked(!mnuThickLines_Paths.get_Checked());
			Settings.Default.AsterPlot_ThickLines_Paths = mnuThickLines_Paths.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuThickLines_EllipseEvents_Click(object sender, EventArgs e)
		{
			mnuThickLines_EllipseEvents.set_Checked(!mnuThickLines_EllipseEvents.get_Checked());
			Settings.Default.Asterplot_ThickLinesOther = mnuThickLines_EllipseEvents.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuLargeLabels_Click(object sender, EventArgs e)
		{
			mnuLargeLabels.set_Checked(!mnuLargeLabels.get_Checked());
			Settings.Default.AsterPlot_LargeFont = mnuLargeLabels.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void drawAsteroidProfileToolStripMenuItem_CheckedChanged(object sender, EventArgs e)
		{
			((Control)panelLimbFit).set_Visible(drawAsteroidProfileToolStripMenuItem.get_Checked());
			Settings.Default.AsterPlotDrawLimb = drawAsteroidProfileToolStripMenuItem.get_Checked();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void addShapeImageFromDAMIT_ISAMToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((Control)Data_and_Plots.Observations_Editor.lblInDAMIT).get_Visible() | ((Control)Data_and_Plots.Observations_Editor.lblInDAMIT).get_Visible())
			{
				if (addShapeImageFromDAMIT_ISAMToolStripMenuItem.get_Checked())
				{
					ToolStripMenuItem obj = showShapeModelControlsOnPlotToolStripMenuItem;
					Panel obj2 = panelShapeModelControl;
					bool flag;
					ErrorBarsForFittingToShapeModels_menu.set_Checked(flag = true);
					bool @checked;
					((Control)obj2).set_Visible(@checked = flag);
					obj.set_Checked(@checked);
				}
				else
				{
					ToolStripMenuItem obj3 = showShapeModelControlsOnPlotToolStripMenuItem;
					Panel obj4 = panelShapeModelControl;
					bool flag;
					ErrorBarsForFittingToShapeModels_menu.set_Checked(flag = false);
					bool @checked;
					((Control)obj4).set_Visible(@checked = flag);
					obj3.set_Checked(@checked);
				}
			}
			else
			{
				ToolStripMenuItem obj5 = addShapeImageFromDAMIT_ISAMToolStripMenuItem;
				ToolStripMenuItem obj6 = showShapeModelControlsOnPlotToolStripMenuItem;
				Panel obj7 = panelShapeModelControl;
				bool flag2;
				ErrorBarsForFittingToShapeModels_menu.set_Checked(flag2 = false);
				bool flag;
				((Control)obj7).set_Visible(flag = flag2);
				bool @checked;
				obj6.set_Checked(@checked = flag);
				obj5.set_Checked(@checked);
			}
			Set_ColorFor_cmdForDiameters();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
			SetTransferButtons(0);
		}

		private void showMeanDiameterDAMITToolStripMenuItem_Click(object sender, EventArgs e)
		{
			mnuShowMeanDiameterDAMIT.set_Checked(!mnuShowMeanDiameterDAMIT.get_Checked());
			Settings.Default.ShapeModel_ShowMean = mnuShowMeanDiameterDAMIT.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void setOrientationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Control)panelGridAngles).set_Visible(true);
		}

		private void kmToolStripMenuItem__100m_Click(object sender, EventArgs e)
		{
			SetGridScale(0.1, Plot: true);
		}

		private void kmToolStripMenuItem1_200m_Click(object sender, EventArgs e)
		{
			SetGridScale(0.2, Plot: true);
		}

		private void kmToolStripMenuItem_500m_Click(object sender, EventArgs e)
		{
			SetGridScale(0.5, Plot: true);
		}

		private void kmToolStripMenuItem_1_Click(object sender, EventArgs e)
		{
			SetGridScale(1.0, Plot: true);
		}

		private void kmToolStripMenuItem_2_Click(object sender, EventArgs e)
		{
			SetGridScale(2.0, Plot: true);
		}

		private void kmToolStripMenuItem_5_Click(object sender, EventArgs e)
		{
			SetGridScale(5.0, Plot: true);
		}

		private void kmToolStripMenuItem_10_Click(object sender, EventArgs e)
		{
			SetGridScale(10.0, Plot: true);
		}

		private void kmToolStripMenuItem_20_Click(object sender, EventArgs e)
		{
			SetGridScale(20.0, Plot: true);
		}

		private void kmToolStripMenuItem_50_Click(object sender, EventArgs e)
		{
			SetGridScale(50.0, Plot: true);
		}

		private void kmToolStripMenuItem_100_Click(object sender, EventArgs e)
		{
			SetGridScale(100.0, Plot: true);
		}

		private void kmToolStripMenuItem_200_Click(object sender, EventArgs e)
		{
			SetGridScale(200.0, Plot: true);
		}

		private void kmToolStripMenuItem_500_Click(object sender, EventArgs e)
		{
			SetGridScale(500.0, Plot: true);
		}

		private void SetGridScale(double Scale, bool Plot)
		{
			Settings.Default.Asterplot_GridScale = Scale;
			kmToolStripMenuItem_100m.set_Checked(Scale == 0.1);
			kmToolStripMenuItem_200.set_Checked(Scale == 0.2);
			kmToolStripMenuItem_500.set_Checked(Scale == 0.5);
			kmToolStripMenuItem_1.set_Checked(Scale == 1.0);
			kmToolStripMenuItem_2.set_Checked(Scale == 2.0);
			kmToolStripMenuItem_5.set_Checked(Scale == 5.0);
			kmToolStripMenuItem_10.set_Checked(Scale == 10.0);
			kmToolStripMenuItem_20.set_Checked(Scale == 20.0);
			kmToolStripMenuItem_50.set_Checked(Scale == 50.0);
			kmToolStripMenuItem_100.set_Checked(Scale == 100.0);
			kmToolStripMenuItem_200.set_Checked(Scale == 200.0);
			kmToolStripMenuItem_500.set_Checked(Scale == 500.0);
			if (Plot)
			{
				Settings @default = Settings.Default;
				ToolStripMenuItem obj = mnuShowScaleGrid;
				ToolStripMenuItem obj2 = setOrientationToolStripMenuItem;
				bool flag;
				((Control)panelGridAngles).set_Visible(flag = true);
				bool flag2;
				obj2.set_Checked(flag2 = flag);
				bool asterplot_ShowScaleGrid;
				obj.set_Checked(asterplot_ShowScaleGrid = flag2);
				@default.Asterplot_ShowScaleGrid = asterplot_ShowScaleGrid;
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.PlotEventOnScreen();
			}
		}

		private void mnuShowScaleGrid_Click(object sender, EventArgs e)
		{
			mnuShowScaleGrid.set_Checked(!mnuShowScaleGrid.get_Checked());
			Settings.Default.Asterplot_ShowScaleGrid = mnuShowScaleGrid.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showPLOTSCALEmasToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showPLOTSCALEmasToolStripMenuItem.set_Checked(!showPLOTSCALEmasToolStripMenuItem.get_Checked());
			Settings.Default.AsterPlot_ShowScaleMAS = showPLOTSCALEmasToolStripMenuItem.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowPlotScalekm_Click(object sender, EventArgs e)
		{
			mnuShowPlotScalekm.set_Checked(!mnuShowPlotScalekm.get_Checked());
			Settings.Default.AsterPlot_ShowScaleKM = mnuShowPlotScalekm.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowSolution_Click(object sender, EventArgs e)
		{
			mnuShowSolution.set_Checked(!mnuShowSolution.get_Checked());
			Settings.Default.AsterPlot_ShowSolution = mnuShowSolution.get_Checked();
			if (mnuShowTitle.get_Checked() & mnuShowSolution.get_Checked())
			{
				Settings @default = Settings.Default;
				bool asterPlot_ShowTitle;
				mnuShowTitle.set_Checked(asterPlot_ShowTitle = false);
				@default.AsterPlot_ShowTitle = asterPlot_ShowTitle;
			}
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowTitle_Click(object sender, EventArgs e)
		{
			mnuShowTitle.set_Checked(!mnuShowTitle.get_Checked());
			Settings.Default.AsterPlot_ShowTitle = mnuShowTitle.get_Checked();
			if (mnuShowTitle.get_Checked() & mnuShowSolution.get_Checked())
			{
				Settings @default = Settings.Default;
				bool asterPlot_ShowSolution;
				mnuShowSolution.set_Checked(asterPlot_ShowSolution = false);
				@default.AsterPlot_ShowSolution = asterPlot_ShowSolution;
			}
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowEllipse_Click(object sender, EventArgs e)
		{
			mnuShowEllipse.set_Checked(!mnuShowEllipse.get_Checked());
			Settings.Default.AsterPlot_ShowEllipse = mnuShowEllipse.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showFresnelDiffractionPeakToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showFresnelDiffractionPeakToolStripMenuItem.set_Checked(!showFresnelDiffractionPeakToolStripMenuItem.get_Checked());
			Settings.Default.Asterplot_ShowFresnel = showFresnelDiffractionPeakToolStripMenuItem.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowEllipseInOutline_Click(object sender, EventArgs e)
		{
			mnuShowEllipseInOutline.set_Checked(!mnuShowEllipseInOutline.get_Checked());
			Settings.Default.AsterPlot_ShowEllipseInOutline = mnuShowEllipseInOutline.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowAxes_Click(object sender, EventArgs e)
		{
			mnuShowAxes.set_Checked(!mnuShowAxes.get_Checked());
			Settings.Default.AsterPlot_ShowAxes = mnuShowAxes.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showSatelliteOrbitsifAnyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings @default = Settings.Default;
			bool asterplot_ShowSatellites;
			showSatelliteOrbitsifAnyToolStripMenuItem.set_Checked(asterplot_ShowSatellites = !showSatelliteOrbitsifAnyToolStripMenuItem.get_Checked());
			@default.Asterplot_ShowSatellites = asterplot_ShowSatellites;
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showASTROMETRYLocation_Click(object sender, EventArgs e)
		{
			showASTROMETRYLocation.set_Checked(!showASTROMETRYLocation.get_Checked());
			Settings.Default.AsterPlot_ShowAstrometry = showASTROMETRYLocation.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowMarkers_Click(object sender, EventArgs e)
		{
			mnuShowMarkers.set_Checked(!mnuShowMarkers.get_Checked());
			Settings.Default.AsterPlot_ShowMarkers = mnuShowMarkers.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowID_Click(object sender, EventArgs e)
		{
			mnuShowID.set_Checked(!mnuShowID.get_Checked());
			Settings.Default.AsterPlot_ShowNumbers = mnuShowID.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowPredicted_Click(object sender, EventArgs e)
		{
			mnuShowPredicted.set_Checked(!mnuShowPredicted.get_Checked());
			Settings.Default.AsterPlot_ShowPredicted = mnuShowPredicted.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuAlign_Click(object sender, EventArgs e)
		{
			mnuAlign.set_Checked(!mnuAlign.get_Checked());
			Settings.Default.AsterPlot_AlignDoubles = mnuAlign.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowErrors_Click(object sender, EventArgs e)
		{
			mnuShowErrors.set_Checked(!mnuShowErrors.get_Checked());
			Settings.Default.AsterPlot_ShowErrors = mnuShowErrors.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void ErrorBarsForFittingToShapeModels_menu_Click(object sender, EventArgs e)
		{
			ErrorBarsForFittingToShapeModels_menu.set_Checked(!ErrorBarsForFittingToShapeModels_menu.get_Checked());
			Set_ColorFor_cmdForDiameters();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowPathsOcculted_Click(object sender, EventArgs e)
		{
			mnuShowPathsOcculted.set_Checked(!mnuShowPathsOcculted.get_Checked());
			if (mnuShowPathsOcculted.get_Checked())
			{
				mnuShowPathsVisible.set_Checked(false);
			}
			Settings.Default.AsterPlot_ShowVisible = mnuShowPathsVisible.get_Checked();
			Settings.Default.AsterPlot_ShowOcculted = mnuShowPathsOcculted.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowPathsVisible_Click(object sender, EventArgs e)
		{
			mnuShowPathsVisible.set_Checked(!mnuShowPathsVisible.get_Checked());
			if (mnuShowPathsVisible.get_Checked())
			{
				mnuShowPathsOcculted.set_Checked(false);
			}
			Settings.Default.AsterPlot_ShowVisible = mnuShowPathsVisible.get_Checked();
			Settings.Default.AsterPlot_ShowOcculted = mnuShowPathsOcculted.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showRINGEventPathsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showRINGEventPathsToolStripMenuItem.set_Checked(!showRINGEventPathsToolStripMenuItem.get_Checked());
			Settings.Default.AsterPlot_ShowRingPaths = showRINGEventPathsToolStripMenuItem.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuIncludeTimeShift_Click(object sender, EventArgs e)
		{
			mnuIncludeTimeShift.set_Checked(!mnuIncludeTimeShift.get_Checked());
			Settings.Default.AsterPlot_IncludeTimeShift = mnuIncludeTimeShift.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.Observations_InitialisePlot_Using_EventDetails();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowPathsMiss_Click(object sender, EventArgs e)
		{
			mnuShowPathsMiss.set_Checked(!mnuShowPathsMiss.get_Checked());
			Settings.Default.AsterPlot_ShowMiss = mnuShowPathsMiss.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowVideo_Click(object sender, EventArgs e)
		{
			mnuShowVideo.set_Checked(!mnuShowVideo.get_Checked());
			Settings.Default.AsterPlot_VideoOnly = mnuShowVideo.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowZeroWeighted_Click(object sender, EventArgs e)
		{
			mnuShowZeroWeighted.set_Checked(!mnuShowZeroWeighted.get_Checked());
			Settings.Default.AsterPlot_ShowZeroWeighted = mnuShowZeroWeighted.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void mnuShowCloud_Click(object sender, EventArgs e)
		{
			mnuShowCloud.set_Checked(!mnuShowCloud.get_Checked());
			Settings.Default.AsterPlot_ShowClouded = mnuShowCloud.get_Checked();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void saveCurrentSettingsAsDefaultToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to save the current Plot Options settings as default settings", "Save settings as defaults", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				for (int i = 0; i < 32; i++)
				{
					PlotMenuDefaults[i] = "0";
				}
				if (mnuBlackBackground.get_Checked())
				{
					PlotMenuDefaults[0] = "1";
				}
				if (mnuPathsInColour.get_Checked())
				{
					PlotMenuDefaults[1] = "1";
				}
				if (mnuThickLines_Paths.get_Checked())
				{
					PlotMenuDefaults[2] = "1";
				}
				if (mnuLargeLabels.get_Checked())
				{
					PlotMenuDefaults[3] = "1";
				}
				if (mnuShowMeanDiameterDAMIT.get_Checked())
				{
					PlotMenuDefaults[4] = "1";
				}
				if (mnuShowScaleGrid.get_Checked())
				{
					PlotMenuDefaults[5] = "1";
				}
				if (mnuShowPlotScalekm.get_Checked())
				{
					PlotMenuDefaults[7] = "1";
				}
				if (mnuShowSolution.get_Checked())
				{
					PlotMenuDefaults[8] = "1";
				}
				if (mnuShowEllipse.get_Checked())
				{
					PlotMenuDefaults[9] = "1";
				}
				if (mnuShowEllipseInOutline.get_Checked())
				{
					PlotMenuDefaults[10] = "1";
				}
				if (drawAsteroidProfileToolStripMenuItem.get_Checked())
				{
					PlotMenuDefaults[11] = "1";
				}
				if (((Control)panelLimbFit).get_Visible())
				{
					PlotMenuDefaults[12] = "1";
				}
				if (mnuShowAxes.get_Checked())
				{
					PlotMenuDefaults[13] = "1";
				}
				if (showASTROMETRYLocation.get_Checked())
				{
					PlotMenuDefaults[14] = "1";
				}
				if (mnuShowMarkers.get_Checked())
				{
					PlotMenuDefaults[15] = "1";
				}
				if (mnuShowID.get_Checked())
				{
					PlotMenuDefaults[16] = "1";
				}
				if (mnuShowPredicted.get_Checked())
				{
					PlotMenuDefaults[17] = "1";
				}
				if (mnuAlign.get_Checked())
				{
					PlotMenuDefaults[18] = "1";
				}
				if (mnuShowErrors.get_Checked())
				{
					PlotMenuDefaults[19] = "1";
				}
				if (mnuShowPathsOcculted.get_Checked())
				{
					PlotMenuDefaults[20] = "1";
				}
				if (mnuShowPathsVisible.get_Checked())
				{
					PlotMenuDefaults[21] = "1";
				}
				if (mnuIncludeTimeShift.get_Checked())
				{
					PlotMenuDefaults[22] = "1";
				}
				if (mnuShowPathsMiss.get_Checked())
				{
					PlotMenuDefaults[23] = "1";
				}
				if (mnuShowVideo.get_Checked())
				{
					PlotMenuDefaults[24] = "1";
				}
				if (mnuShowZeroWeighted.get_Checked())
				{
					PlotMenuDefaults[25] = "1";
				}
				if (mnuShowCloud.get_Checked())
				{
					PlotMenuDefaults[26] = "1";
				}
				if (mnuShowTitle.get_Checked())
				{
					PlotMenuDefaults[27] = "1";
				}
				if (showAsteroidEllipseInColorToolStripMenuItem.get_Checked())
				{
					PlotMenuDefaults[28] = "1";
				}
				if (showEventInColorToolStripMenuItem.get_Checked())
				{
					PlotMenuDefaults[29] = "1";
				}
				if (mnuThickLines_EllipseEvents.get_Checked())
				{
					PlotMenuDefaults[30] = "1";
				}
				if (showMISSEventsAsDashedLinesToolStripMenuItem.get_Checked())
				{
					PlotMenuDefaults[31] = "1";
				}
				string text = "";
				for (int j = 0; j < 32; j++)
				{
					text += PlotMenuDefaults[j];
				}
				Settings.Default.AsterPlot_Defaults = text;
			}
		}

		private void applyDEFAULTSettingsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to replace the current Plot Options settings with your default settings", "Set settings to defaults", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				ApplyDefaults();
			}
		}

		private void ApplyDefaults()
		{
			string text = Settings.Default.AsterPlot_Defaults.PadRight(32, '0');
			for (int i = 0; i < 32; i++)
			{
				PlotMenuDefaults[i] = text.Substring(i, 1);
			}
			Settings @default = Settings.Default;
			bool asterPlot_Colour;
			mnuBlackBackground.set_Checked(asterPlot_Colour = PlotMenuDefaults[0] == "1");
			@default.AsterPlot_Colour = asterPlot_Colour;
			Settings default2 = Settings.Default;
			mnuPathsInColour.set_Checked(asterPlot_Colour = PlotMenuDefaults[1] == "1");
			default2.AsterPlot_ChordsIncolour = asterPlot_Colour;
			Settings default3 = Settings.Default;
			mnuThickLines_Paths.set_Checked(asterPlot_Colour = PlotMenuDefaults[2] == "1");
			default3.AsterPlot_ThickLines_Paths = asterPlot_Colour;
			Settings default4 = Settings.Default;
			mnuLargeLabels.set_Checked(asterPlot_Colour = PlotMenuDefaults[3] == "1");
			default4.AsterPlot_LargeFont = asterPlot_Colour;
			Settings default5 = Settings.Default;
			mnuShowMeanDiameterDAMIT.set_Checked(asterPlot_Colour = PlotMenuDefaults[4] == "1");
			default5.ShapeModel_ShowMean = asterPlot_Colour;
			Settings default6 = Settings.Default;
			mnuShowScaleGrid.set_Checked(asterPlot_Colour = PlotMenuDefaults[5] == "1");
			default6.Asterplot_ShowScaleGrid = asterPlot_Colour;
			Settings default7 = Settings.Default;
			mnuShowPlotScalekm.set_Checked(asterPlot_Colour = PlotMenuDefaults[7] == "1");
			default7.AsterPlot_ShowScaleKM = asterPlot_Colour;
			Settings default8 = Settings.Default;
			mnuShowSolution.set_Checked(asterPlot_Colour = PlotMenuDefaults[8] == "1");
			default8.AsterPlot_ShowSolution = asterPlot_Colour;
			Settings default9 = Settings.Default;
			mnuShowEllipse.set_Checked(asterPlot_Colour = PlotMenuDefaults[9] == "1");
			default9.AsterPlot_ShowEllipse = asterPlot_Colour;
			Settings default10 = Settings.Default;
			mnuShowEllipseInOutline.set_Checked(asterPlot_Colour = PlotMenuDefaults[10] == "1");
			default10.AsterPlot_ShowEllipseInOutline = asterPlot_Colour;
			Settings default11 = Settings.Default;
			drawAsteroidProfileToolStripMenuItem.set_Checked(asterPlot_Colour = PlotMenuDefaults[11] == "1");
			default11.AsterPlotDrawLimb = asterPlot_Colour;
			Panel obj = panelLimbFit;
			Settings default12 = Settings.Default;
			bool flag;
			drawAsteroidProfileToolStripMenuItem.set_Checked(flag = PlotMenuDefaults[12] == "1");
			asterPlot_Colour = (default12.AsterPlotDrawLimb = flag);
			((Control)obj).set_Visible(asterPlot_Colour);
			Settings default13 = Settings.Default;
			mnuShowAxes.set_Checked(asterPlot_Colour = PlotMenuDefaults[13] == "1");
			default13.AsterPlot_ShowAxes = asterPlot_Colour;
			Settings default14 = Settings.Default;
			showASTROMETRYLocation.set_Checked(asterPlot_Colour = PlotMenuDefaults[14] == "1");
			default14.AsterPlot_ShowAstrometry = asterPlot_Colour;
			Settings default15 = Settings.Default;
			mnuShowMarkers.set_Checked(asterPlot_Colour = PlotMenuDefaults[15] == "1");
			default15.AsterPlot_ShowMarkers = asterPlot_Colour;
			Settings default16 = Settings.Default;
			mnuShowID.set_Checked(asterPlot_Colour = PlotMenuDefaults[16] == "1");
			default16.AsterPlot_ShowNumbers = asterPlot_Colour;
			Settings default17 = Settings.Default;
			mnuShowPredicted.set_Checked(asterPlot_Colour = PlotMenuDefaults[17] == "1");
			default17.AsterPlot_ShowPredicted = asterPlot_Colour;
			Settings default18 = Settings.Default;
			mnuAlign.set_Checked(asterPlot_Colour = PlotMenuDefaults[18] == "1");
			default18.AsterPlot_AlignDoubles = asterPlot_Colour;
			Settings default19 = Settings.Default;
			mnuShowErrors.set_Checked(asterPlot_Colour = PlotMenuDefaults[19] == "1");
			default19.AsterPlot_ShowErrors = asterPlot_Colour;
			Settings default20 = Settings.Default;
			mnuShowPathsOcculted.set_Checked(asterPlot_Colour = PlotMenuDefaults[20] == "1");
			default20.AsterPlot_ShowOcculted = asterPlot_Colour;
			Settings default21 = Settings.Default;
			mnuShowPathsVisible.set_Checked(asterPlot_Colour = PlotMenuDefaults[21] == "1");
			default21.AsterPlot_ShowVisible = asterPlot_Colour;
			Settings default22 = Settings.Default;
			mnuIncludeTimeShift.set_Checked(asterPlot_Colour = PlotMenuDefaults[22] == "1");
			default22.AsterPlot_IncludeTimeShift = asterPlot_Colour;
			Settings default23 = Settings.Default;
			mnuShowPathsMiss.set_Checked(asterPlot_Colour = PlotMenuDefaults[23] == "1");
			default23.AsterPlot_ShowMiss = asterPlot_Colour;
			Settings default24 = Settings.Default;
			mnuShowVideo.set_Checked(asterPlot_Colour = PlotMenuDefaults[24] == "1");
			default24.AsterPlot_VideoOnly = asterPlot_Colour;
			Settings default25 = Settings.Default;
			mnuShowZeroWeighted.set_Checked(asterPlot_Colour = PlotMenuDefaults[25] == "1");
			default25.AsterPlot_ShowZeroWeighted = asterPlot_Colour;
			Settings default26 = Settings.Default;
			mnuShowCloud.set_Checked(asterPlot_Colour = PlotMenuDefaults[26] == "1");
			default26.AsterPlot_ShowClouded = asterPlot_Colour;
			Settings default27 = Settings.Default;
			mnuShowTitle.set_Checked(asterPlot_Colour = PlotMenuDefaults[27] == "1");
			default27.AsterPlot_ShowTitle = asterPlot_Colour;
			Settings default28 = Settings.Default;
			showAsteroidEllipseInColorToolStripMenuItem.set_Checked(asterPlot_Colour = PlotMenuDefaults[28] == "1");
			default28.AsterPlot_EllipseIncolour = asterPlot_Colour;
			Settings default29 = Settings.Default;
			showEventInColorToolStripMenuItem.set_Checked(asterPlot_Colour = PlotMenuDefaults[29] == "1");
			default29.AsterPlot_EventsIncolour = asterPlot_Colour;
			Settings default30 = Settings.Default;
			mnuThickLines_EllipseEvents.set_Checked(asterPlot_Colour = PlotMenuDefaults[30] == "1");
			default30.Asterplot_ThickLinesOther = asterPlot_Colour;
			Settings default31 = Settings.Default;
			showMISSEventsAsDashedLinesToolStripMenuItem.set_Checked(asterPlot_Colour = PlotMenuDefaults[31] == "1");
			default31.AsterPlot_MissDashed = asterPlot_Colour;
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void restoreOccultDefaultValuesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to restore your current default settings to the Occult default settings", "Restore Default values to the Occult values", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				Settings.Default.AsterPlot_Defaults = ((SettingsBase)Settings.Default).get_Properties().get_Item("AsterPlot_Defaults").get_DefaultValue()
					.ToString();
				ApplyDefaults();
			}
		}

		private void setPlotOptionsForPublicationPlotToolStripMenuItem_Click(object sender, EventArgs e)
		{
			mnuShowSolution.set_Checked(false);
			mnuShowTitle.set_Checked(true);
			showFresnelDiffractionPeakToolStripMenuItem.set_Checked(false);
			mnuShowID.set_Checked(true);
			showASTROMETRYLocation.set_Checked(false);
			mnuBlackBackground.set_Checked(false);
			mnuGrayBackground.set_Checked(false);
			mnuShowPredicted.set_Checked(false);
			mnuShowPathsMiss.set_Checked(true);
			showMISSEventsAsDashedLinesToolStripMenuItem.set_Checked(false);
			mnuShowZeroWeighted.set_Checked(false);
			mnuShowCloud.set_Checked(false);
			mnuShowAxes.set_Checked(false);
			drawAsteroidProfileToolStripMenuItem.set_Checked(false);
			mnuShowPlotScalekm.set_Checked(true);
			mnuShowEllipseInOutline.set_Checked(false);
			mnuShowEllipse.set_Checked(true);
			mnuShowMarkers.set_Checked(true);
			mnuShowErrors.set_Checked(true);
			mnuShowPathsOcculted.set_Checked(false);
			mnuShowPathsVisible.set_Checked(true);
			mnuIncludeTimeShift.set_Checked(false);
			mnuShowVideo.set_Checked(false);
			mnuPathsInColour.set_Checked(false);
			mnuThickLines_Paths.set_Checked(false);
			mnuLargeLabels.set_Checked(false);
			showAsteroidEllipseInColorToolStripMenuItem.set_Checked(true);
			showEventInColorToolStripMenuItem.set_Checked(true);
			mnuThickLines_EllipseEvents.set_Checked(true);
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void optBoth_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void optPrimary_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void optSecondary_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picPlot.get_Image());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.PrintEventPreview();
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.PrintEvent();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "(" + EventDetails.AsteroidNumber.Trim() + ") " + EventDetails.AsteroidID.Trim() + "_" + EventDetails.Year + " " + Utilities.ShortMonths[EventDetails.Month] + " " + EventDetails.Day.ToString().PadLeft(2, '0');
			Settings.Default.Save_AsteroidObservations = Output.SaveGraphic(picPlot.get_Image(), text, Settings.Default.Save_AsteroidObservations);
			if (mnuShowID.get_Checked())
			{
				Settings.Default.Save_AsteroidObservations = Output.SaveGraphic(picLegend.get_Image(), text + "Legend", Settings.Default.Save_AsteroidObservations);
			}
		}

		private void saveImageToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Application.DoEvents();
			double num = (double)Settings.Default.MonitorScale / 100.0;
			int width = (int)(num * (double)((Control)this).get_Bounds().Width);
			int height = (int)(num * (double)((Control)this).get_Bounds().Height);
			Bitmap bitmap = new Bitmap(width, height);
			Graphics.FromImage(bitmap).CopyFromScreen((int)(num * (double)((Form)this).get_Location().X), (int)(num * (double)((Form)this).get_Location().Y), 0, 0, bitmap.Size);
			string fileRootName = Data_and_Plots.Year + Utilities.ShortMonths[Data_and_Plots.Month] + Data_and_Plots.Day + "_" + EventDetails.AsteroidID.Trim();
			Settings.Default.Save_AsteroidObservationsForm = Output.SaveGraphic(bitmap, fileRootName, Settings.Default.Save_AsteroidObservationsForm);
		}

		private void setScalecurrently100ToMatchMonitorScaleToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new MonitorScale()).ShowDialog();
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).set_Text("Set scale (currently " + Settings.Default.MonitorScale + "%) to match Monitor scale");
		}

		private void picPlot_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Invalid comparison between Unknown and I4
			if (GettingTag || GettingSlope)
			{
				return;
			}
			float num = e.get_X();
			float num2 = e.get_Y();
			string text = "";
			Color color = ((!(((Control)picPlot).get_BackColor() == Color.White)) ? Color.White : Color.Black);
			if ((int)e.get_Button() == 1048576)
			{
				GettingSlope = true;
				if (Xrev0 < 0)
				{
					Xrev0 = (int)num;
					Yrev0 = (int)num2;
					Xrev1 = (int)num;
					Yrev1 = (int)num2;
				}
				else
				{
					ControlPaint.DrawReversibleLine(((Control)picPlot).PointToScreen(new Point(Xrev0, Yrev0)), ((Control)picPlot).PointToScreen(new Point(Xrev1, Yrev1)), ((Control)picPlot).get_BackColor());
					Xrev1 = (int)num;
					Yrev1 = (int)num2;
					ControlPaint.DrawReversibleLine(((Control)picPlot).PointToScreen(new Point(Xrev0, Yrev0)), ((Control)picPlot).PointToScreen(new Point(Xrev1, Yrev1)), color);
					double num3 = Math.Atan2(Xrev1 - Xrev0, Yrev1 - Yrev0) * (180.0 / Math.PI);
					if (num3 > 90.0)
					{
						num3 = -180.0 + num3;
					}
					if (num3 < -90.0)
					{
						num3 = 180.0 + num3;
					}
					text = "";
					if (CurrentPlotRecord >= 0)
					{
						double num4 = (0.0 - Math.Atan2(Data_and_Plots.Tags[CurrentPlotRecord].dX, Data_and_Plots.Tags[CurrentPlotRecord].dY)) * (180.0 / Math.PI);
						if (num4 > 90.0)
						{
							num4 = -180.0 + num4;
						}
						if (num4 < -90.0)
						{
							num4 = 180.0 + num4;
						}
						NormalMotion = Math.Abs((double)Data_and_Plots.Tags[CurrentPlotRecord].RateMilliArcSec * Math.Sin((num4 - num3) / (180.0 / Math.PI)));
						if (Data_and_Plots.Tags[CurrentPlotRecord].EventType == 1)
						{
							NormalMotion = 0.0 - NormalMotion;
						}
						text = Data_and_Plots.Tags[CurrentPlotRecord].Tag + "\r\n" + string.Format("Star motion {0,1:f3} masec/sec in PA {1,1:F2} deg.", Data_and_Plots.Tags[CurrentPlotRecord].RateMilliArcSec, num4) + "\r\n";
					}
					text = text + "Limb slope PA = " + string.Format("{0,1:F1} deg.", num3);
					if (NormalMotion != 0.0)
					{
						text += string.Format("\r\nNormal motion {0,1:f3} masec/sec", NormalMotion);
					}
					GetToolTip = !GetToolTip;
					if (GetToolTip)
					{
						toolTip.SetToolTip((Control)(object)picPlot, text);
					}
					Application.DoEvents();
				}
				GettingSlope = false;
				return;
			}
			if (Xrev0 >= 0)
			{
				ControlPaint.DrawReversibleLine(((Control)picPlot).PointToScreen(new Point(Xrev0, Yrev0)), ((Control)picPlot).PointToScreen(new Point(Xrev1, Yrev1)), ((Control)picPlot).get_BackColor());
				Xrev0 = -1;
			}
			if (Data_and_Plots.Tags.Count < 1 || ((Math.Abs(num - OldX) < 2f) & (Math.Abs(num2 - OldY) < 2f)))
			{
				return;
			}
			GettingTag = true;
			float num5 = num - 4f - 1f;
			int num6 = 0;
			int num7 = Data_and_Plots.Tags.Count - 1;
			int num8;
			do
			{
				num8 = (num7 + num6) / 2;
				if (num5 == Data_and_Plots.Tags[num8].X)
				{
					break;
				}
				if (num5 < Data_and_Plots.Tags[num8].X)
				{
					num7 = num8 - 1;
				}
				else
				{
					num6 = num8 + 1;
				}
			}
			while (num7 >= num6);
			int num9 = num8;
			CurrentPlotRecord = -1;
			do
			{
				if ((Math.Abs(num - Data_and_Plots.Tags[num9].X) < 4f) & (Math.Abs(num2 - Data_and_Plots.Tags[num9].Y) < 4f))
				{
					if (Data_and_Plots.Tags[num9].Tag.Contains("Predict"))
					{
						for (int i = 0; i < EventDetails.Observers.Count; i++)
						{
							if ((EventDetails.Observers[i].Event_D == "P") & (EventDetails.Observers[i].PlotCode == " "))
							{
								Data_and_Plots.MinimumDistance(i, out var dT, out var Dist);
								((Control)lblTag).set_Text(string.Format("Prediction O-C  dT = {0,5:F1} secs, dist = {1:F0} km north", dT, Dist));
								break;
							}
						}
					}
					else
					{
						((Control)lblTag).set_Text(Data_and_Plots.Tags[num9].Tag);
					}
					OldX = num;
					OldY = num2;
					OldArg0 = num9;
					CurrentPlotRecord = num9;
					GettingTag = false;
					return;
				}
				if (Data_and_Plots.Tags[num9].X - num > 4f)
				{
					((Control)lblTag).set_Text("{Observer && time}");
					GettingTag = false;
					return;
				}
				num9++;
			}
			while (num9 < Data_and_Plots.Tags.Count);
			((Control)lblTag).set_Text("{Observer && time}");
			GettingTag = false;
		}

		private void picPlot_MouseUp(object sender, MouseEventArgs e)
		{
			if (!showStarDiameterAnalyserToolStripMenuItem.get_Checked())
			{
				return;
			}
			StarDiameterAnalysis.ShowStarDiameterAnalyser();
			if (CurrentPlotRecord >= 0)
			{
				((Control)StarDiameterAnalysis.StarDiameter_Analyser.txtObserver).set_Text("Observer:   " + Data_and_Plots.Tags[CurrentPlotRecord].Tag);
				if (NormalMotion < 0.0)
				{
					((Control)StarDiameterAnalysis.StarDiameter_Analyser.txtMotionD).set_Text(string.Format("{0,1:f2}", 0.0 - NormalMotion));
				}
				else
				{
					((Control)StarDiameterAnalysis.StarDiameter_Analyser.txtMotionR).set_Text(string.Format("{0,1:f2}", NormalMotion));
				}
				((Control)StarDiameterAnalysis.StarDiameter_Analyser).Focus();
			}
		}

		private void cmdFit_Click(object sender, EventArgs e)
		{
			FitChords();
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			if (((Control)panelDouble).get_Visible())
			{
				SetLabelValues();
			}
		}

		private void cmdFit_Doubles_Click(object sender, EventArgs e)
		{
			if (opt4.get_Checked() && !DoubleStarOffsets_HaveBeenSet())
			{
				NoOffsets_Message();
				return;
			}
			bool @checked = chkX.get_Checked();
			bool checked2 = chkY.get_Checked();
			bool checked3 = chkA.get_Checked();
			bool checked4 = chkB.get_Checked();
			bool checked5 = chkPA.get_Checked();
			bool checked6 = chkMiss.get_Checked();
			CheckBox obj = chkX;
			CheckBox obj2 = chkY;
			CheckBox obj3 = chkA;
			CheckBox obj4 = chkB;
			CheckBox obj5 = chkPA;
			bool flag;
			chkMiss.set_Checked(flag = false);
			bool flag2;
			obj5.set_Checked(flag2 = flag);
			bool flag3;
			obj4.set_Checked(flag3 = flag2);
			bool flag4;
			obj3.set_Checked(flag4 = flag3);
			bool checked7;
			obj2.set_Checked(checked7 = flag4);
			obj.set_Checked(checked7);
			FitChords();
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
			chkX.set_Checked(@checked);
			chkY.set_Checked(checked2);
			chkA.set_Checked(checked3);
			chkB.set_Checked(checked4);
			chkPA.set_Checked(checked5);
			chkMiss.set_Checked(checked6);
			SetLabelValues();
		}

		internal void FitChords()
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			Updating = true;
			if (!decimal.TryParse(((Control)chkX).get_Text(), out var result))
			{
				result = default(decimal);
			}
			if (!decimal.TryParse(((Control)chkY).get_Text(), out var result2))
			{
				result2 = default(decimal);
			}
			if (!decimal.TryParse(((Control)chkA).get_Text(), out var result3))
			{
				result3 = default(decimal);
			}
			if (!decimal.TryParse(((Control)chkB).get_Text(), out var result4))
			{
				result4 = default(decimal);
			}
			if (!decimal.TryParse(((Control)chkPA).get_Text(), out var result5))
			{
				result5 = default(decimal);
			}
			if (!decimal.TryParse(((Control)chkCompanionSep).get_Text(), out var result6))
			{
				result6 = default(decimal);
			}
			if (!decimal.TryParse(((Control)chkCompanionPA).get_Text(), out var result7))
			{
				result7 = default(decimal);
			}
			decimal num = (decimal)EventDetails.AsteroidNominalDiameter / 10m;
			if (Math.Abs(result) > num)
			{
				result = num * (decimal)Math.Sign(result);
			}
			if (Math.Abs(result2) > num)
			{
				result2 = num * (decimal)Math.Sign(result2);
			}
			if (Math.Abs(result3) > num)
			{
				result3 = num * (decimal)Math.Sign(result3);
			}
			if (Math.Abs(result4) > num)
			{
				result4 = num * (decimal)Math.Sign(result4);
			}
			if (Math.Abs(result5) > 2m)
			{
				result5 = 2 * Math.Sign(result5);
			}
			if (chkShapeModelCentered.get_Checked())
			{
				result2 = default(decimal);
				result = result2;
			}
			decimal num2 = updnB.get_Value() / 2m;
			if (updnA.get_Value() / 2m < num2)
			{
				num2 = updnA.get_Value() / 2m;
			}
			if ((Math.Abs(updnX.get_Value() + result) > num2) | (Math.Abs(updnY.get_Value() + result2) > num2))
			{
				result *= 0.05m;
				result2 *= 0.05m;
			}
			if (Math.Abs(result6) > 2m)
			{
				result6 = 2 * Math.Sign(result6);
			}
			if (Math.Abs(result7) > 2m)
			{
				result7 = 2 * Math.Sign(result7);
			}
			if (chkX.get_Checked())
			{
				NumericUpDown obj = updnX;
				obj.set_Value(obj.get_Value() + result);
			}
			if (chkY.get_Checked())
			{
				NumericUpDown obj2 = updnY;
				obj2.set_Value(obj2.get_Value() + result2);
			}
			if (chkA.get_Checked())
			{
				if (updnA.get_Value() + result3 < updnA.get_Value() / 2m)
				{
					NumericUpDown obj3 = updnA;
					obj3.set_Value(obj3.get_Value() / 2m);
				}
				else
				{
					NumericUpDown obj4 = updnA;
					obj4.set_Value(obj4.get_Value() + result3);
				}
			}
			if (chkCircle.get_Checked() & chkA.get_Checked())
			{
				updnB.set_Value(updnA.get_Value());
			}
			else if (chkB.get_Checked())
			{
				if (updnB.get_Value() + result4 < updnB.get_Value() / 2m)
				{
					NumericUpDown obj5 = updnB;
					obj5.set_Value(obj5.get_Value() / 2m);
				}
				else
				{
					NumericUpDown obj6 = updnB;
					obj6.set_Value(obj6.get_Value() + result4);
				}
			}
			if (chkPA.get_Checked())
			{
				NumericUpDown obj7 = updnPA;
				obj7.set_Value(obj7.get_Value() + result5);
			}
			if (chkCompanionSep.get_Checked())
			{
				if (updn_DoubleSep[CurrentlySelectedComponent].get_Value() + result6 > 0m)
				{
					NumericUpDown obj8 = updn_DoubleSep[CurrentlySelectedComponent];
					obj8.set_Value(obj8.get_Value() + result6);
				}
				else if (updn_DoublePA[CurrentlySelectedComponent].get_Value() < 180m)
				{
					NumericUpDown obj9 = updn_DoublePA[CurrentlySelectedComponent];
					obj9.set_Value(obj9.get_Value() + 180m);
				}
				else
				{
					NumericUpDown obj10 = updn_DoublePA[CurrentlySelectedComponent];
					obj10.set_Value(obj10.get_Value() - 360m);
				}
			}
			if (chkCompanionPA.get_Checked())
			{
				NumericUpDown obj11 = updn_DoublePA[CurrentlySelectedComponent];
				obj11.set_Value(obj11.get_Value() + result7);
			}
			if (updnA.get_Value() < updnB.get_Value())
			{
				decimal value = updnA.get_Value();
				updnA.set_Value(updnB.get_Value());
				updnB.set_Value(value);
				if (updnPA.get_Value() < 180m)
				{
					NumericUpDown obj12 = updnPA;
					obj12.set_Value(obj12.get_Value() + 90m);
				}
				else
				{
					NumericUpDown obj13 = updnPA;
					obj13.set_Value(obj13.get_Value() - 90m);
				}
			}
			if (updnPA.get_Value() > 360m)
			{
				NumericUpDown obj14 = updnPA;
				obj14.set_Value(obj14.get_Value() - 360m);
			}
			if (updnPA.get_Value() < 0m)
			{
				NumericUpDown obj15 = updnPA;
				obj15.set_Value(obj15.get_Value() + 360m);
			}
			if (updn_DoublePA[CurrentlySelectedComponent].get_Value() > 360m)
			{
				NumericUpDown obj16 = updn_DoublePA[CurrentlySelectedComponent];
				obj16.set_Value(obj16.get_Value() - 360m);
			}
			if (updn_DoublePA[CurrentlySelectedComponent].get_Value() < 0m)
			{
				NumericUpDown obj17 = updn_DoublePA[CurrentlySelectedComponent];
				obj17.set_Value(obj17.get_Value() + 360m);
			}
			Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
			SetRatioAndMag();
			Updating = false;
			Data_and_Plots.PlotEventOnScreen();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void chkCircle_CheckedChanged(object sender, EventArgs e)
		{
			UpdateCircleCheck();
		}

		private void UpdateCircleCheck()
		{
			Updating = true;
			if (!chkCircle.get_Checked())
			{
				((Control)chkA).set_Enabled(true);
				((Control)chkB).set_Enabled(true);
				((Control)chkPA).set_Enabled(true);
				((Control)updnA).set_Enabled(true);
				((Control)updnB).set_Enabled(true);
				((Control)updnPA).set_Enabled(true);
			}
			else
			{
				CheckBox obj = chkB;
				CheckBox obj2 = chkB;
				bool flag;
				((Control)updnB).set_Enabled(flag = false);
				bool enabled;
				obj2.set_Checked(enabled = flag);
				((Control)obj).set_Enabled(enabled);
				updnB.set_Value(updnA.get_Value());
				((Control)chkPA).set_Enabled(false);
				chkPA.set_Checked(false);
				((Control)updnPA).set_Enabled(false);
				updnPA.set_Value(0m);
				Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
			}
			Data_and_Plots.PlotEventOnScreen();
			Updating = false;
		}

		private void chkCircle_MouseClick(object sender, MouseEventArgs e)
		{
			chkCircle.set_Checked(!chkCircle.get_Checked());
			if (!chkCircle.get_Checked())
			{
				chkUseAssumedDiameter.set_Checked(false);
			}
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void chkUseAssumedDiameter_CheckedChanged(object sender, EventArgs e)
		{
			if (!Updating)
			{
				if (!chkUseAssumedDiameter.get_Checked())
				{
					((Control)chkA).set_Enabled(true);
					((Control)updnA).set_Enabled(true);
					chkCircle.set_Checked(true);
					CheckBox obj = chkB;
					CheckBox obj2 = chkPA;
					NumericUpDown obj3 = updnB;
					bool flag;
					((Control)updnPA).set_Enabled(flag = true);
					bool flag2;
					((Control)obj3).set_Enabled(flag2 = flag);
					bool enabled;
					((Control)obj2).set_Enabled(enabled = flag2);
					((Control)obj).set_Enabled(enabled);
				}
				else
				{
					Updating = true;
					CheckBox obj4 = chkA;
					CheckBox obj5 = chkB;
					bool flag2;
					((Control)chkPA).set_Enabled(flag2 = false);
					bool enabled;
					((Control)obj5).set_Enabled(enabled = flag2);
					((Control)obj4).set_Enabled(enabled);
					CheckBox obj6 = chkA;
					CheckBox obj7 = chkB;
					chkPA.set_Checked(flag2 = false);
					obj7.set_Checked(enabled = flag2);
					obj6.set_Checked(enabled);
					NumericUpDown obj8 = updnA;
					NumericUpDown obj9 = updnB;
					((Control)updnPA).set_Enabled(flag2 = false);
					((Control)obj9).set_Enabled(enabled = flag2);
					((Control)obj8).set_Enabled(enabled);
					updnPA.set_Value(0m);
					NumericUpDown obj10 = updnA;
					decimal value;
					updnB.set_Value(value = (decimal)Data_and_Plots.GetAssumedDiameter());
					obj10.set_Value(value);
					Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
				}
				EventDetails.UsedAssumedDiameter = chkUseAssumedDiameter.get_Checked();
				Data_and_Plots.PlotEventOnScreen();
				UpdateCircleCheck();
			}
		}

		private void chkUseAssumedDiameter_MouseClick(object sender, MouseEventArgs e)
		{
			chkUseAssumedDiameter.set_Checked(!chkUseAssumedDiameter.get_Checked());
			if (chkUseAssumedDiameter.get_Checked())
			{
				chkCircle.set_Checked(true);
			}
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void copyObserverListToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void saveObserverListToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidObservations = Output.SavePredictionText(CollectEvents(), EventDetails.AsteroidNumber.Trim() + " " + EventDetails.AsteroidID.Trim() + " " + EventDetails.FormattedDate + " Observer List", Settings.Default.Save_AsteroidObservations);
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstObservers.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstObservers.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void cmdMissTimes_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ShowMissTimesForm();
			Data_and_Plots.ClosestMissTimes();
		}

		private void cmdShowEditor_Click(object sender, EventArgs e)
		{
			Data_and_Plots.ComputeSolution_and_UpDate_EventDetails();
			Data_and_Plots.ShowEditor();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid observations - plot");
		}

		private void trackLimbFit_ValueChanged(object sender, EventArgs e)
		{
			if (((Control)trackLimbFit).get_Visible())
			{
				Data_and_Plots.PlotEventOnScreen();
			}
		}

		private void cmdCloseGridAngles_Click(object sender, EventArgs e)
		{
			((Control)panelGridAngles).set_Visible(false);
		}

		private void cmdCloseShapeModelControl_Click(object sender, EventArgs e)
		{
			if (Asteroid_Observations_Reports.Display_ShapeModels != null && ((Control)Asteroid_Observations_Reports.Display_ShapeModels.pnlPhaseChange).get_Visible())
			{
				Asteroid_Observations_Reports.Display_ShapeModels.ClosePhaseChangePanel(Asteroid_Observations_Reports.Display_ShapeModels.CurrentModel);
				return;
			}
			ToolStripMenuItem obj = showShapeModelControlsOnPlotToolStripMenuItem;
			bool @checked;
			((Control)panelShapeModelControl).set_Visible(@checked = false);
			obj.set_Checked(@checked);
		}

		private void showShapeModelControlsOnPlotToolStripMenuItem_Click(object sender, EventArgs e)
		{
			showShapeModelControlsOnPlotToolStripMenuItem.set_Checked(!showShapeModelControlsOnPlotToolStripMenuItem.get_Checked());
			if (showShapeModelControlsOnPlotToolStripMenuItem.get_Checked())
			{
				((Control)panelShapeModelControl).set_Visible(true);
				Data_and_Plots.PlotEventOnScreen();
				SetTransferButtons(0);
			}
			else
			{
				((Control)panelShapeModelControl).set_Visible(false);
			}
		}

		private void tbarScaleAdjust_Scroll(object sender, EventArgs e)
		{
			SetScale();
		}

		private void SetScale()
		{
			if (Asteroid_Observations_Reports.Display_ShapeModels != null)
			{
				Asteroid_Observations_Reports.Display_ShapeModels.trackBarImageSize.set_Value(tbarScaleAdjust.get_Value());
				if (((Control)Asteroid_Observations_Reports.Display_ShapeModels.pnlPhaseChange).get_Visible())
				{
					Asteroid_Observations_Reports.Display_ShapeModels.TransferPhaseImage(Asteroid_Observations_Reports.Display_ShapeModels.CurrentPhaseImage);
				}
				else
				{
					Asteroid_Observations_Reports.Display_ShapeModels.TransferImage(Asteroid_Observations_Reports.Display_ShapeModels.CurrentImage);
				}
			}
		}

		private void keepOnTopToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).set_TopMost(!((Form)this).get_TopMost());
			if (((Form)this).get_TopMost())
			{
				((ToolStripItem)keepOnTopToolStripMenuItem).set_Text("Return form to &Normal");
			}
			else
			{
				((ToolStripItem)keepOnTopToolStripMenuItem).set_Text("Keep form on &Top");
			}
		}

		internal void EnsurePlotObservations_NotOnTop(bool EnsureNotOnTop)
		{
			if (!((Form)this).get_TopMost() && Asteroid_Observations_Reports.Display_ShapeModels != null && ((Form)Asteroid_Observations_Reports.Display_ShapeModels).get_TopMost())
			{
				Asteroid_Observations_Reports.Display_ShapeModels.DAMIT_OnTop();
			}
			if (EnsureNotOnTop & ((Form)this).get_TopMost())
			{
				((Form)this).set_TopMost(false);
				((ToolStripItem)keepOnTopToolStripMenuItem).set_Text("Keep form on &Top");
			}
		}

		private void panelShapeModelControl_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 1048576)
			{
				xPos_Shape = e.get_X();
				yPos_Shape = e.get_Y();
			}
		}

		private void panelShapeModelControl_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0018: Invalid comparison between Unknown and I4
			Panel val = (Panel)((sender is Panel) ? sender : null);
			if (val != null && (int)e.get_Button() == 1048576)
			{
				((Control)val).set_Top(((Control)val).get_Top() + (e.get_Y() - yPos_Shape));
				if (((Control)val).get_Bottom() < 15)
				{
					((Control)val).set_Top(15 - ((Control)val).get_Height());
				}
				if (((Control)val).get_Top() > ((Control)this).get_Height() - 50)
				{
					((Control)val).set_Top(((Control)this).get_Height() - 50);
				}
				((Control)val).set_Left(((Control)val).get_Left() + (e.get_X() - xPos_Shape));
				if (((Control)val).get_Right() < 10)
				{
					((Control)val).set_Left(10 - ((Control)val).get_Width());
				}
				if (((Control)val).get_Left() > ((Control)this).get_Width() - 30)
				{
					((Control)val).set_Left(((Control)this).get_Width() - 30);
				}
			}
		}

		private void trackOpacity_ValueChanged(object sender, EventArgs e)
		{
			((Form)this).set_Opacity((double)trackOpacity.get_Value() / 100.0);
		}

		internal void Set_optCompanions(bool SingleSolution)
		{
			optCompanion[0].set_Checked(true);
			for (int i = 1; i < 4; i++)
			{
				optCompanion[i].set_Checked(false);
				((Control)optCompanion[i]).set_Enabled(!SingleSolution);
			}
		}

		private void cmdSetOffsets_Click(object sender, EventArgs e)
		{
			try
			{
				((Control)Data_and_Plots.SetOffsets).Show();
			}
			catch
			{
				Data_and_Plots.SetOffsets = new SetDoubleStarOffsets();
				((Control)Data_and_Plots.SetOffsets).Show();
			}
			optPrimary.set_Checked(true);
			RadioButton obj2 = optSecondary;
			RadioButton obj3 = optBoth;
			CheckBox obj4 = chkCompanionPA;
			CheckBox obj5 = chkCompanionSep;
			bool flag;
			((Control)panelDouble).set_Enabled(flag = false);
			bool flag2;
			((Control)obj5).set_Enabled(flag2 = flag);
			bool flag3;
			((Control)obj4).set_Enabled(flag3 = flag2);
			bool enabled;
			((Control)obj3).set_Enabled(enabled = flag3);
			((Control)obj2).set_Enabled(enabled);
			chkCompanionPA.set_Checked(false);
			chkCompanionSep.set_Checked(false);
			chkCompanionPA.set_Checked(false);
			CheckBox obj6 = chkX;
			chkY.set_Checked(enabled = true);
			obj6.set_Checked(enabled);
			((Control)Data_and_Plots.SetOffsets).Focus();
		}

		private void cmdGetLightCurves_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.Display_ShapeModels.ListDAMITLightCurves();
			DisplayShapeModels.DisplayDAMITLightCurveFits();
		}

		private void cmbModelFitQuality_SelectedIndexChanged(object sender, EventArgs e)
		{
			SetFitQuality();
		}

		private void SetFitQuality()
		{
			if ((((ListControl)cmbModelFitQuality).get_SelectedIndex() < 3) | (((ListControl)cmbModelFitQuality).get_SelectedIndex() == 7))
			{
				TextBox obj = txtModelMinDia;
				string text;
				((Control)txtModelMaxDia).set_Text(text = "");
				((Control)obj).set_Text(text);
				TextBox obj2 = txtModelMinDia;
				Color pink;
				((Control)txtModelMaxDia).set_BackColor(pink = Color.Pink);
				((Control)obj2).set_BackColor(pink);
			}
			else if (((ListControl)cmbModelFitQuality).get_SelectedIndex() == 3)
			{
				((Control)txtModelMaxDia).set_Text("");
				((Control)txtModelMaxDia).set_BackColor(Color.Pink);
				((Control)txtModelMinDia).set_BackColor(Control.get_DefaultBackColor());
			}
			else
			{
				TextBox obj3 = txtModelMinDia;
				Color pink;
				((Control)txtModelMaxDia).set_BackColor(pink = Control.get_DefaultBackColor());
				((Control)obj3).set_BackColor(pink);
			}
			((Control)lblQualityExplanation).set_Text(QualityExplanation[((ListControl)cmbModelFitQuality).get_SelectedIndex()]);
			((Control)lblQualityExplanation).set_Left(240 - ((Control)lblQualityExplanation).get_Width() / 2);
			UnsavedShapeChanges = true;
			((Control)cmdSaveModelInfo).set_Enabled(true);
			((Control)cmdSaveModelInfo).set_BackColor(Color.LightGreen);
		}

		private void cmbModelFitQuality_MouseDown(object sender, MouseEventArgs e)
		{
			SetFitQuality();
		}

		private void cmdSetModelMin_Click(object sender, EventArgs e)
		{
			if (Data_and_Plots.LengthOfDiameterByVolLine > 30f)
			{
				((Control)txtModelMinDia).set_Text(string.Format("{0,1:f0}", Data_and_Plots.LengthOfDiameterByVolLine));
			}
			else
			{
				((Control)txtModelMinDia).set_Text(string.Format("{0,1:f1}", Data_and_Plots.LengthOfDiameterByVolLine));
			}
			UnsavedShapeChanges = true;
			((Control)cmdSaveModelInfo).set_Enabled(true);
			((Control)cmdSaveModelInfo).set_BackColor(Color.LightGreen);
		}

		private void cmdSetModelMax_Click(object sender, EventArgs e)
		{
			if (Data_and_Plots.LengthOfDiameterByVolLine > 30f)
			{
				((Control)txtModelMaxDia).set_Text(string.Format("{0,1:f0}", Data_and_Plots.LengthOfDiameterByVolLine));
			}
			else
			{
				((Control)txtModelMaxDia).set_Text(string.Format("{0,1:f1}", Data_and_Plots.LengthOfDiameterByVolLine));
			}
			UnsavedShapeChanges = true;
			((Control)cmdSaveModelInfo).set_Enabled(true);
			((Control)cmdSaveModelInfo).set_BackColor(Color.LightGreen);
		}

		private void cmdSaveModelInfo_Click(object sender, EventArgs e)
		{
			bool flag = false;
			int transferButtonColors = 0;
			int num = 0;
			int num2 = -1;
			double result;
			for (int i = 0; i < EventDetails.ShapeData.Count; i++)
			{
				if ((EventDetails.ShapeData[i].Source == ((Control)txtModelSource).get_Text()) & (EventDetails.ShapeData[i].ID == ((Control)txtModelID).get_Text()))
				{
					EventDetails.ShapeData[i].SurfaceVolumeRatio = double.Parse(((Control)txtSurface_Volume_Ratio).get_Text());
					EventDetails.ShapeData[i].Version = ((Control)txtVersion).get_Text().Trim();
					EventDetails.ShapeData[i].PhaseCorrn = int.Parse(((Control)txtModelPhase).get_Text().Replace("°", ""));
					EventDetails.ShapeData[i].FitQuality = ((ListControl)cmbModelFitQuality).get_SelectedIndex();
					if (((ListControl)cmbModelFitQuality).get_SelectedIndex() <= 3)
					{
						((Control)txtModelMaxDia).set_BackColor(Color.Pink);
					}
					else
					{
						((Control)txtModelMaxDia).set_BackColor(Control.get_DefaultBackColor());
					}
					double.TryParse(((Control)txtModelMinDia).get_Text(), out result);
					EventDetails.ShapeData[i].DiaMin = result;
					double.TryParse(((Control)txtModelMaxDia).get_Text(), out result);
					EventDetails.ShapeData[i].DiaMax = result;
					flag = true;
					break;
				}
			}
			if (!flag)
			{
				AddShapeModel(((Control)txtModelSource).get_Text(), ((Control)txtModelID).get_Text(), ((Control)txtVersion).get_Text().Trim());
			}
			for (int j = 0; j < 12; j++)
			{
				if (((Control)txtModelID).get_Text() == DisplayShapeModels.ModelParameters[j].ModelNumber_String)
				{
					transferButtonColors = j;
					if (j < 6)
					{
						num2 = Asteroid_Observations_Reports.Display_ShapeModels.ISAMEquivalent[j];
					}
					else
					{
						_ = Asteroid_Observations_Reports.Display_ShapeModels.DamitEquivalent[j - 6];
					}
					break;
				}
			}
			if (num2 >= 0)
			{
				flag = false;
				string text = "ISAM";
				string modelNumber_String = DisplayShapeModels.ModelParameters[num2 + 6].ModelNumber_String;
				string version = DisplayShapeModels.ModelParameters[num2 + 6].VersionComments.Trim();
				for (int k = 0; k < EventDetails.ShapeData.Count; k++)
				{
					if (!((EventDetails.ShapeData[k].Source == text) & (EventDetails.ShapeData[k].ID == modelNumber_String)))
					{
						continue;
					}
					for (int l = 0; l < 12; l++)
					{
						if (EventDetails.ShapeData[k].ID == DisplayShapeModels.ModelParameters[l].ModelNumber_String)
						{
							num = l;
							break;
						}
					}
					EventDetails.ShapeData[k].SurfaceVolumeRatio = double.Parse(DisplayShapeModels.ModelParameters[num].SurfaceToVolumeRatio.ToString());
					EventDetails.ShapeData[k].Version = DisplayShapeModels.ModelParameters[num].VersionComments.Trim();
					EventDetails.ShapeData[k].PhaseCorrn = int.Parse(((Control)txtModelPhase).get_Text().Replace("°", ""));
					EventDetails.ShapeData[k].FitQuality = ((ListControl)cmbModelFitQuality).get_SelectedIndex();
					double.TryParse(((Control)txtModelMinDia).get_Text(), out result);
					EventDetails.ShapeData[k].DiaMin = result;
					double.TryParse(((Control)txtModelMaxDia).get_Text(), out result);
					EventDetails.ShapeData[k].DiaMax = result;
					flag = true;
					break;
				}
				if (!flag)
				{
					AddShapeModel(text, modelNumber_String, version);
				}
			}
			EventDetails.ShapeData.Sort();
			DisplayExistingSolutionForCurrentModel(((Control)txtModelSource).get_Text(), ((Control)txtModelID).get_Text());
			SetTransferButtonColors(transferButtonColors);
			UnsavedShapeChanges = false;
			((Control)cmdSaveModelInfo).set_Enabled(false);
			((Control)cmdSaveModelInfo).set_BackColor(Color.Pink);
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void AddShapeModel(string Source, string ID, string Version)
		{
			ShapeModelData shapeModelData = new ShapeModelData();
			shapeModelData.Source = Source;
			shapeModelData.ID = ID;
			double.TryParse(((Control)txtSurface_Volume_Ratio).get_Text(), out var result);
			shapeModelData.SurfaceVolumeRatio = result;
			shapeModelData.Version = Version;
			int.TryParse(((Control)txtModelPhase).get_Text().Replace("°", ""), out var result2);
			shapeModelData.PhaseCorrn = result2;
			shapeModelData.FitQuality = ((ListControl)cmbModelFitQuality).get_SelectedIndex();
			double.TryParse(((Control)txtModelMinDia).get_Text(), out result);
			shapeModelData.DiaMin = result;
			double.TryParse(((Control)txtModelMaxDia).get_Text(), out result);
			shapeModelData.DiaMax = result;
			EventDetails.ShapeData.Add(shapeModelData);
			EventDetails.ShapeModelFitted = true;
			EventDetails.ShapeData.Sort();
			DisplayExistingSolutionForCurrentModel(((Control)txtModelSource).get_Text(), ((Control)txtModelID).get_Text());
		}

		private void panelSatellites_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 1048576)
			{
				xPos_Satellites = e.get_X();
				yPos_Satellites = e.get_Y();
			}
		}

		private void panelSatellites_MouseHover(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_SizeAll());
		}

		private void panelSatellites_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0018: Invalid comparison between Unknown and I4
			Panel val = (Panel)((sender is Panel) ? sender : null);
			if (val != null && (int)e.get_Button() == 1048576)
			{
				((Control)val).set_Top(((Control)val).get_Top() + (e.get_Y() - yPos_Satellites));
				if (((Control)val).get_Bottom() < 15)
				{
					((Control)val).set_Top(15 - ((Control)val).get_Height());
				}
				if (((Control)val).get_Top() > ((Control)this).get_Height() - 55)
				{
					((Control)val).set_Top(((Control)this).get_Height() - 55);
				}
				((Control)val).set_Left(((Control)val).get_Left() + (e.get_X() - xPos_Satellites));
				if (((Control)val).get_Right() < 15)
				{
					((Control)val).set_Left(15 - ((Control)val).get_Width());
				}
				if (((Control)val).get_Left() > ((Control)this).get_Width() - 35)
				{
					((Control)val).set_Left(((Control)this).get_Width() - 35);
				}
			}
		}

		private void picDrag_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 1048576)
			{
				xPos_SatellitesIcon = e.get_X();
				yPos_SatellitesIcon = e.get_Y();
			}
		}

		private void picDrag_MouseEnter(object sender, EventArgs e)
		{
			((Control)PanelSatellites).Focus();
		}

		private void picDrag_MouseHover(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_SizeAll());
		}

		private void picDrag_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() != 1048576)
			{
				return;
			}
			try
			{
				Panel panelSatellites = PanelSatellites;
				((Control)panelSatellites).set_Top(((Control)panelSatellites).get_Top() + (e.get_Y() - yPos_SatellitesIcon));
				if (((Control)PanelSatellites).get_Bottom() < 15)
				{
					((Control)PanelSatellites).set_Top(15 - ((Control)PanelSatellites).get_Height());
				}
				if (((Control)PanelSatellites).get_Top() > ((Control)this).get_Height() - 55)
				{
					((Control)PanelSatellites).set_Top(((Control)this).get_Height() - 55);
				}
				Panel panelSatellites2 = PanelSatellites;
				((Control)panelSatellites2).set_Left(((Control)panelSatellites2).get_Left() + (e.get_X() - xPos_SatellitesIcon));
				if (((Control)PanelSatellites).get_Right() < 15)
				{
					((Control)PanelSatellites).set_Left(15 - ((Control)PanelSatellites).get_Width());
				}
				if (((Control)PanelSatellites).get_Left() > ((Control)this).get_Width() - 35)
				{
					((Control)PanelSatellites).set_Left(((Control)this).get_Width() - 35);
				}
			}
			catch
			{
			}
		}

		private void picDragDouble_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 1048576)
			{
				xPos_DoublesIcon = e.get_X();
				yPos_DoublesIcon = e.get_Y();
			}
		}

		private void picDragDouble_MouseHover(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_SizeAll());
		}

		private void picDragDouble_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() != 1048576)
			{
				return;
			}
			try
			{
				Panel obj = panelDouble;
				((Control)obj).set_Top(((Control)obj).get_Top() + (e.get_Y() - yPos_DoublesIcon));
				if (((Control)panelDouble).get_Bottom() < 15)
				{
					((Control)panelDouble).set_Top(15 - ((Control)panelDouble).get_Height());
				}
				if (((Control)panelDouble).get_Top() > ((Control)this).get_Height() - 55)
				{
					((Control)panelDouble).set_Top(((Control)this).get_Height() - 55);
				}
				Panel obj2 = panelDouble;
				((Control)obj2).set_Left(((Control)obj2).get_Left() + (e.get_X() - xPos_DoublesIcon));
				if (((Control)panelDouble).get_Right() < 15)
				{
					((Control)panelDouble).set_Left(15 - ((Control)panelDouble).get_Width());
				}
				if (((Control)panelDouble).get_Left() > ((Control)this).get_Width() - 35)
				{
					((Control)panelDouble).set_Left(((Control)this).get_Width() - 35);
				}
			}
			catch
			{
			}
		}

		private void optyPlotNormal_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.PlotEventOnScreen();
		}

		private void optPlotx2_CheckedChanged(object sender, EventArgs e)
		{
			Data_and_Plots.PlotEventOnScreen();
		}

		private void PanelSatellites_MouseEnter(object sender, EventArgs e)
		{
			((Control)PanelSatellites).Focus();
		}

		private void cmdSet3Checks_Click(object sender, EventArgs e)
		{
			CheckBox obj = chkA;
			CheckBox obj2 = chkB;
			bool flag;
			chkPA.set_Checked(flag = true);
			bool @checked;
			obj2.set_Checked(@checked = flag);
			obj.set_Checked(@checked);
			chkUseAssumedDiameter.set_Checked(false);
			chkCircle.set_Checked(false);
			chkMiss.set_Checked(true);
		}

		private void cmdHelpDoubles_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Solve asteroid doubles");
		}

		private void lblsat1_DoubleClick(object sender, EventArgs e)
		{
			((Control)txtSatID[(int)updnSatNum.get_Value() - 1]).set_Text(((Control)lblSat1).get_Text());
		}

		private void lblSat2_DoubleClick(object sender, EventArgs e)
		{
			((Control)txtSatID[(int)updnSatNum.get_Value() - 1]).set_Text(((Control)lblSat2).get_Text());
		}

		private void lblSat3_DoubleClick(object sender, EventArgs e)
		{
			((Control)txtSatID[(int)updnSatNum.get_Value() - 1]).set_Text(((Control)lblSat3).get_Text());
		}

		private void lblSat4_Click(object sender, EventArgs e)
		{
			((Control)txtSatID[(int)updnSatNum.get_Value() - 1]).set_Text(((Control)lblSat4).get_Text());
			((Control)updnBrightRatio).set_BackColor(Color.White);
		}

		private void chkMiss_MouseClick(object sender, MouseEventArgs e)
		{
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void chkShapeModelCentered_MouseClick(object sender, MouseEventArgs e)
		{
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void cmbQuality_MouseClick(object sender, MouseEventArgs e)
		{
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void cmdSet3Checks_MouseClick(object sender, MouseEventArgs e)
		{
			((Control)Data_and_Plots.Observations_Editor.lblLastSaveTime).set_ForeColor(Color.Red);
		}

		private void updnCenterOfMass_X_ValueChanged(object sender, EventArgs e)
		{
			if (updnCenterOfMass_X.get_Value() != 0m)
			{
				CheckBox obj = chkShapeModelCentered;
				bool @checked;
				((Control)chkShapeModelCentered).set_Enabled(@checked = false);
				obj.set_Checked(@checked);
			}
			RePlotAfterChanges();
		}

		private void updnCenterOfMass_Y_ValueChanged(object sender, EventArgs e)
		{
			if (updnCenterOfMass_Y.get_Value() != 0m)
			{
				CheckBox obj = chkShapeModelCentered;
				bool @checked;
				((Control)chkShapeModelCentered).set_Enabled(@checked = false);
				obj.set_Checked(@checked);
			}
			RePlotAfterChanges();
		}

		private void chkScoll_CheckedChanged(object sender, EventArgs e)
		{
			RePlotAfterChanges();
		}

		private void updnBrightRatio_ValueChanged(object sender, EventArgs e)
		{
			if ((updnBrightRatio.get_Value() == 50m) | (updnBrightRatio.get_Value() == 0.02m))
			{
				((Control)lblOffset).set_Text("No offset");
				((Control)updnBrightRatio).set_BackColor(Color.YellowGreen);
			}
			else
			{
				((Control)lblOffset).set_Text("Brightness ratio");
				((Control)updnBrightRatio).set_BackColor(Color.White);
			}
			if (!LightDropChanging)
			{
				LightDropChanging = true;
				double.TryParse(((Control)txtMainDrop).get_Text(), out var result);
				((Control)txt2ndDrop).set_Text(string.Format("{0,1:f0}", result / (double)updnBrightRatio.get_Value()));
				SetDoubleMagnitudes();
				LightDropChanging = false;
			}
		}

		internal void SetDoubleMagnitudes()
		{
			double num = Math.Pow(10.0, EventDetails.MgStar / -2.5);
			double d = num * (double)updnBrightRatio.get_Value() / (1.0 + (double)updnBrightRatio.get_Value());
			double d2 = num / (1.0 + (double)updnBrightRatio.get_Value());
			((Control)txtMagMain).set_Text(string.Format("{0,1:f1}", -2.5 * Math.Log10(d)));
			((Control)txtMag2nd).set_Text(string.Format("{0,1:f1}", -2.5 * Math.Log10(d2)));
		}

		private void txt2ndDrop_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtMainDrop_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtMainDrop_TextChanged(object sender, EventArgs e)
		{
			if (!LightDropChanging)
			{
				LightDropChanging = true;
				double.TryParse(((Control)txtMainDrop).get_Text(), out var result);
				double.TryParse(((Control)txt2ndDrop).get_Text(), out var result2);
				double num = result / result2;
				if (num < 0.02)
				{
					num = 0.02;
				}
				if (num > 50.0)
				{
					num = 50.0;
				}
				updnBrightRatio.set_Value((decimal)num);
				SetDoubleMagnitudes();
				LightDropChanging = false;
			}
		}

		private void txt2ndDrop_TextChanged(object sender, EventArgs e)
		{
			if (!LightDropChanging)
			{
				LightDropChanging = true;
				double.TryParse(((Control)txtMainDrop).get_Text(), out var result);
				double.TryParse(((Control)txt2ndDrop).get_Text(), out var result2);
				double num = result / result2;
				if (num < 0.02)
				{
					num = 0.02;
				}
				if (num > 50.0)
				{
					num = 50.0;
				}
				updnBrightRatio.set_Value((decimal)num);
				SetDoubleMagnitudes();
				LightDropChanging = false;
			}
		}

		private void cmdShapeHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Fit Shape models");
		}

		private void optSingle_Click(object sender, EventArgs e)
		{
			SetDoubleSolutionType(1, Initialising: false);
		}

		private void optTwo_Click(object sender, EventArgs e)
		{
			SetDoubleSolutionType(2, Initialising: false);
		}

		private void picLegend_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() != 2097152)
			{
				return;
			}
			int y = e.get_Y();
			int num = 0;
			for (int i = 0; i < 500 && (i - 1) * 12 <= y; i++)
			{
				int num2 = 4 * (int)Math.Floor((double)i / 5.0);
				if (y >= i * 12 + num2 && y < (i + 1) * 12 + num2)
				{
					num = i;
					break;
				}
			}
			if (num < Data_and_Plots.ObserverListIndex.Count)
			{
				Data_and_Plots.EventLineToHighlight = Data_and_Plots.ObserverListIndex[num];
			}
			if (!Updating)
			{
				Updating = true;
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.PlotEventOnScreen();
				Updating = false;
				Data_and_Plots.EventLineToHighlight = -1;
			}
		}

		private void cmdEstimateOrbit_Click(object sender, EventArgs e)
		{
			((Control)new OrbitSizes()).Show();
		}

		private void chkEditCBET_CheckedChanged(object sender, EventArgs e)
		{
			Edit_CBET_Settings(chkEditCBET.get_Checked());
		}

		internal void Edit_CBET_Settings(bool SetToEdit)
		{
			for (int i = 0; i < 4; i++)
			{
				((TextBoxBase)txtCBET[i]).set_ReadOnly(!chkEditCBET.get_Checked());
				if (chkEditCBET.get_Checked())
				{
					((Control)txtCBET[i]).set_BackColor(Color.LightGoldenrodYellow);
					((Control)txtCBET[i]).Focus();
					((TextBoxBase)txtCBET[i]).set_SelectionStart(0);
				}
				else
				{
					((Control)txtCBET[i]).set_BackColor(Color.LightCyan);
				}
			}
		}

		private void chkEditJDSO_CheckedChanged(object sender, EventArgs e)
		{
			EditJDSO_Settings();
		}

		private void EditJDSO_Settings()
		{
			TextBox obj = txtDoublePairID;
			TextBox obj2 = txtJDSOSubmitDate;
			bool flag;
			((TextBoxBase)txtJDSO_Vol_Num_Pg).set_ReadOnly(flag = !chkEditJDSO.get_Checked());
			bool readOnly;
			((TextBoxBase)obj2).set_ReadOnly(readOnly = flag);
			((TextBoxBase)obj).set_ReadOnly(readOnly);
			if (chkEditJDSO.get_Checked())
			{
				TextBox obj3 = txtJDSOSubmitDate;
				Color backColor;
				((Control)txtJDSO_Vol_Num_Pg).set_BackColor(backColor = Color.FromArgb(255, 235, 255, 230));
				((Control)obj3).set_BackColor(backColor);
			}
			else
			{
				TextBox obj4 = txtJDSOSubmitDate;
				Color backColor;
				((Control)txtJDSO_Vol_Num_Pg).set_BackColor(backColor = Color.FromArgb(255, 255, 230, 240));
				((Control)obj4).set_BackColor(backColor);
			}
		}

		private void txtJDSO_Vol_Num_Pg_Leave(object sender, EventArgs e)
		{
			((Control)txtJDSO_Vol_Num_Pg).set_Text(((Control)txtJDSO_Vol_Num_Pg).get_Text().Replace("|", ""));
		}

		private void txtJDSO_Vol_Num_Pg_TextChanged(object sender, EventArgs e)
		{
			((Control)txtJDSO_Vol_Num_Pg).set_Text(((Control)txtJDSO_Vol_Num_Pg).get_Text().Replace("|", ""));
		}

		private void txtJDSOSubmitDate_TextChanged(object sender, EventArgs e)
		{
			((Control)txtJDSOSubmitDate).set_Text(((Control)txtJDSOSubmitDate).get_Text().Replace("|", ""));
		}

		private void txtJDSOSubmitDate_Leave(object sender, EventArgs e)
		{
			((Control)txtJDSOSubmitDate).set_Text(((Control)txtJDSOSubmitDate).get_Text().Replace("|", ""));
		}

		private void opt1Component_Click(object sender, EventArgs e)
		{
			SetDoubleSolutionType(1, Initialising: false);
		}

		private void opt4_Click(object sender, EventArgs e)
		{
			SetDoubleSolutionType(4, Initialising: false);
		}

		internal void SetDoubleSolutionType(int Solutions, bool Initialising)
		{
			//IL_00af: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Invalid comparison between Unknown and I4
			string[] array = new string[3] { "", "one.", "two." };
			bool flag2;
			bool @checked;
			if (!Initialising && (Solutions == 1 || Solutions == 2))
			{
				bool flag = false;
				if ((updn_DoubleSep[2].get_Value() != 0m) | (updn_DoubleSep[3].get_Value() != 0m))
				{
					flag = true;
				}
				if ((Solutions == 1) & (updn_DoubleSep[1].get_Value() != 0m))
				{
					flag = true;
				}
				if (flag && (int)MessageBox.Show("You are reducing the number of solutions to " + array[Solutions] + "\r\n\r\nYou have solution values that will be deleted as a result\r\n\r\nDo you wish to continue?", "Confirm loss of solutions", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
				{
					return;
				}
				CurrentlySelectedComponent = 0;
				optCompanion[0].set_Checked(true);
				RadioButton obj = optCompanion[1];
				RadioButton obj2 = optCompanion[2];
				optCompanion[3].set_Checked(flag2 = false);
				obj2.set_Checked(@checked = flag2);
				obj.set_Checked(@checked);
				NumericUpDown obj3 = updn_DoubleSep[2];
				NumericUpDown obj4 = updn_DoubleSep[3];
				decimal value = default(decimal);
				obj4.set_Value(value);
				obj3.set_Value(value);
				if (Solutions == 1)
				{
					updn_DoubleSep[1].set_Value(0m);
				}
				SetDoubleStarOffsetValues(0m, 0m, 0m, 0m);
				SetCompanion(0);
			}
			if (!Data_and_Plots.PrimaryStarEventFound)
			{
				Panel obj5 = pnlDoubleStarSolution;
				((Control)panelSolve).set_Enabled(@checked = false);
				((Control)obj5).set_Enabled(@checked);
				opt1Component.set_Checked(true);
				RadioButton obj6 = optSingle;
				RadioButton obj7 = optTwo;
				opt4.set_Checked(flag2 = false);
				obj7.set_Checked(@checked = flag2);
				obj6.set_Checked(@checked);
				((Control)opt1Component).set_Enabled(true);
				RadioButton obj8 = optSingle;
				RadioButton obj9 = optTwo;
				((Control)opt4).set_Enabled(flag2 = false);
				((Control)obj9).set_Enabled(@checked = flag2);
				((Control)obj8).set_Enabled(@checked);
				optCompanion[0].set_Checked(true);
				RadioButton obj10 = optCompanion[1];
				RadioButton obj11 = optCompanion[2];
				optCompanion[3].set_Checked(flag2 = false);
				obj11.set_Checked(@checked = flag2);
				obj10.set_Checked(@checked);
			}
			else
			{
				Panel obj12 = pnlDoubleStarSolution;
				((Control)panelSolve).set_Enabled(@checked = true);
				((Control)obj12).set_Enabled(@checked);
				opt1Component.set_Checked(!Data_and_Plots.PrimaryStarEventFound);
				((Control)opt1Component).set_Enabled(false);
				RadioButton obj13 = optSingle;
				RadioButton obj14 = optTwo;
				((Control)opt4).set_Enabled(flag2 = true);
				((Control)obj14).set_Enabled(@checked = flag2);
				((Control)obj13).set_Enabled(@checked);
				optSingle.set_Checked((Solutions == 1) & (updn_DoubleSep[0].get_Value() >= 0m));
				optTwo.set_Checked(Solutions == 2);
				opt4.set_Checked(Solutions == 4);
				((Control)optCompanion[1]).set_Enabled(Solutions > 1);
				RadioButton obj15 = optCompanion[2];
				((Control)optCompanion[3]).set_Enabled(@checked = Solutions == 4);
				((Control)obj15).set_Enabled(@checked);
				if (Solutions < 2)
				{
					if (optCompanion[1].get_Checked() | optCompanion[2].get_Checked() | optCompanion[3].get_Checked())
					{
						optCompanion[0].set_Checked(true);
						SetCompanion(0);
						CurrentlySelectedComponent = 0;
					}
				}
				else if (Solutions == 2 && (optCompanion[2].get_Checked() | optCompanion[3].get_Checked()))
				{
					optCompanion[0].set_Checked(true);
					SetCompanion(0);
					CurrentlySelectedComponent = 0;
				}
			}
			Label obj16 = lblSelectNth;
			Label obj17 = lblSelectSth;
			((Control)lblSelectSep).set_Visible(flag2 = Solutions == 4);
			((Control)obj17).set_Visible(@checked = flag2);
			((Control)obj16).set_Visible(@checked);
			if (Solutions == 4)
			{
				((Control)panelSolve).set_Height(104);
				if (!DoubleStarOffsets_HaveBeenSet())
				{
					FlashTimerDouble.Start();
				}
				else
				{
					FlashTimerDouble.Stop();
					((Control)cmdSetOffsets).set_BackColor(Color.Chartreuse);
				}
			}
			else
			{
				((Control)panelSolve).set_Height(58);
				FlashTimerDouble.Stop();
				((Control)cmdSetOffsets).set_BackColor(Color.Chartreuse);
			}
			((Control)panelJDSO).set_Top(((Control)panelSolve).get_Bottom() + 6);
			((Control)panelDouble).set_Height(((Control)panelJDSO).get_Bottom() + 7);
			SetLabelValues();
		}

		private void PlotObservations_FormClosing(object sender, FormClosingEventArgs e)
		{
			Settings.Default.LocationAsterPlotObs = ((Form)this).get_Location();
		}

		private void showStarWithADiameterOfToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings @default = Settings.Default;
			bool asterPlot_ShowStar;
			showStarWithADiameterOfToolStripMenuItem.set_Checked(asterPlot_ShowStar = !showStarWithADiameterOfToolStripMenuItem.get_Checked());
			@default.AsterPlot_ShowStar = asterPlot_ShowStar;
			Settings.Default.Asterplot_StarDia = ((ToolStripItem)toolStripStarDia_mas).get_Text();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void addThisTextToTheTitleToolStripMenuItem_Click(object sender, EventArgs e)
		{
			addThisTextToTheTitleToolStripMenuItem.set_Checked(!addThisTextToTheTitleToolStripMenuItem.get_Checked());
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void includeAsterodIDAndModelIDToolStripMenuItem_Click(object sender, EventArgs e)
		{
			includeAsterodIDAndModelIDToolStripMenuItem.set_Checked(DrawDamitID_ShapeModel = !DrawDamitID_ShapeModel);
			Settings.Default.DrawDamitID = DrawDamitID_ShapeModel;
			try
			{
				Asteroid_Observations_Reports.Display_ShapeModels.ShowModelID();
			}
			catch
			{
			}
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showPolarAxesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings @default = Settings.Default;
			bool includeAxisOfRotation;
			showPolarAxesToolStripMenuItem.set_Checked(includeAxisOfRotation = (IncludeAxisOfRotation_ShapeModel = !IncludeAxisOfRotation_ShapeModel));
			@default.IncludeAxisOfRotation = includeAxisOfRotation;
			try
			{
				Asteroid_Observations_Reports.Display_ShapeModels.IncludeAxis();
			}
			catch
			{
			}
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showModelMeanDiameterLinesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ToolStripMenuItem obj = showModelMeanDiameterLinesToolStripMenuItem;
			bool @checked = (Settings.Default.ShapeModel_DrawMean = (DrawDAMITMean_ShapeModel = !DrawDAMITMean_ShapeModel));
			obj.set_Checked(@checked);
			try
			{
				Asteroid_Observations_Reports.Display_ShapeModels.ShowMeanDiameter();
			}
			catch
			{
			}
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showModelFacesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ToolStripMenuItem obj = showModelFacesToolStripMenuItem;
			bool @checked = (Settings.Default.ModelFaceEdges = (ShowModelFaceEdges_ShapeModel = !ShowModelFaceEdges_ShapeModel));
			obj.set_Checked(@checked);
			try
			{
				Asteroid_Observations_Reports.Display_ShapeModels.ShowModelFaces();
			}
			catch
			{
			}
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void plotModelDarkToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ToolStripMenuItem obj = plotModelDarkToolStripMenuItem;
			bool @checked = (Settings.Default.ModelDark = (DarkShapeModel = !DarkShapeModel));
			obj.set_Checked(@checked);
			try
			{
				Asteroid_Observations_Reports.Display_ShapeModels.DarkPlotModel();
			}
			catch
			{
			}
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void plotModelInGreyScaleToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ToolStripMenuItem obj = plotModelInGreyScaleToolStripMenuItem;
			bool @checked = (Settings.Default.ModelinBlackWhite = (BlackWhiteShapeModel = !BlackWhiteShapeModel));
			obj.set_Checked(@checked);
			try
			{
				Asteroid_Observations_Reports.Display_ShapeModels.GreyScalePlotModel();
			}
			catch
			{
			}
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void drawStarImageAtPlotCenterToolStripMenuItem_Click(object sender, EventArgs e)
		{
			bool centerStarPlotOnPlot;
			drawStarImageAtPlotCenterToolStripMenuItem.set_Checked(centerStarPlotOnPlot = !drawStarImageAtPlotCenterToolStripMenuItem.get_Checked());
			Data_and_Plots.CenterStarPlotOnPlot = centerStarPlotOnPlot;
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void toolStripStarDia_mas_Click(object sender, EventArgs e)
		{
			Settings.Default.Asterplot_StarDia = ((ToolStripItem)toolStripStarDia_mas).get_Text();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showMISSEventsAsDashedLinesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings @default = Settings.Default;
			bool asterPlot_MissDashed;
			showMISSEventsAsDashedLinesToolStripMenuItem.set_Checked(asterPlot_MissDashed = !showMISSEventsAsDashedLinesToolStripMenuItem.get_Checked());
			@default.AsterPlot_MissDashed = asterPlot_MissDashed;
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		private void cmdForDiameters_Click(object sender, EventArgs e)
		{
			ErrorBarsForFittingToShapeModels_menu.set_Checked(!ErrorBarsForFittingToShapeModels_menu.get_Checked());
			Set_ColorFor_cmdForDiameters();
			Data_and_Plots.GetPlotParametersFromForm();
			Data_and_Plots.PlotEventOnScreen();
		}

		internal void Set_ColorFor_cmdForDiameters()
		{
			if (ErrorBarsForFittingToShapeModels_menu.get_Checked())
			{
				((Control)cmdForDiameters).set_BackColor(Color.Yellow);
				((Control)cmdForDiameters).set_Text("Dia");
			}
			else
			{
				((Control)cmdForDiameters).set_BackColor(Color.LimeGreen);
				((Control)cmdForDiameters).set_Text("Err");
			}
		}

		private void panelDouble_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 1048576)
			{
				xPos_Doubles = e.get_X();
				yPos_Doubles = e.get_Y();
			}
		}

		private void panelDouble_MouseHover(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_SizeAll());
		}

		private void panelDouble_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0018: Invalid comparison between Unknown and I4
			Panel val = (Panel)((sender is Panel) ? sender : null);
			if (val != null && (int)e.get_Button() == 1048576)
			{
				((Control)val).set_Top(((Control)val).get_Top() + (e.get_Y() - yPos_Doubles));
				if (((Control)val).get_Bottom() < 8)
				{
					((Control)val).set_Top(8 - ((Control)val).get_Height());
				}
				if (((Control)val).get_Top() > ((Control)this).get_Height() - 46)
				{
					((Control)val).set_Top(((Control)this).get_Height() - 46);
				}
				((Control)val).set_Left(((Control)val).get_Left() + (e.get_X() - xPos_Doubles));
				if (((Control)val).get_Right() < 8)
				{
					((Control)val).set_Left(8 - ((Control)val).get_Width());
				}
				if (((Control)val).get_Left() > ((Control)this).get_Width() - 25)
				{
					((Control)val).set_Left(((Control)this).get_Width() - 25);
				}
			}
		}

		private void cmdGetSatelliteMotions_Click(object sender, EventArgs e)
		{
			Data_and_Plots.getMiriadeSatelliteMotions();
		}

		private void updnSatNum_ValueChanged(object sender, EventArgs e)
		{
			chkEditCBET.set_Checked(false);
			Edit_CBET_Settings(chkEditCBET.get_Checked());
			AddNewSatellite();
		}

		private void AddNewSatellite()
		{
			//IL_002c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Invalid comparison between Unknown and I4
			if (updnSatNum.get_Value() > (decimal)EventDetails.NumberOfSatellites)
			{
				if ((int)MessageBox.Show("Do you want to create a new Satellite entry?", "New satellite", (MessageBoxButtons)4, (MessageBoxIcon)32) == 7)
				{
					if (!(updnSatNum.get_Value() == 1m))
					{
						NumericUpDown obj = updnSatNum;
						decimal value = obj.get_Value();
						obj.set_Value(value - 1m);
					}
					return;
				}
				SatelliteData value2 = new SatelliteData();
				EventDetails.Satellites[EventDetails.NumberOfSatellites] = value2;
				EventDetails.NumberOfSatellites++;
				((Control)lblSatelliteNumbers).set_Text("of " + EventDetails.NumberOfSatellites);
			}
			SetSatelliteControls();
			Data_and_Plots.SelectedSatellite = (int)updnSatNum.get_Value() - 1;
			SetSatelliteControls_Visible((int)updnSatNum.get_Value() - 1);
			RePlotAfterChanges();
		}

		private void panelShapeModelControl_MouseHover(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_SizeAll());
		}

		private void cmdHelpOnCompanionID_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Satellite solution");
		}

		private void DisplayExistingSolutionForCurrentModel(string ModelSource, string ID)
		{
			string text;
			if (EventDetails.ShapeModelFitted)
			{
				for (int i = 0; i < EventDetails.ShapeData.Count; i++)
				{
					if (!(ID == "") && !(EventDetails.ShapeData[i].ID == "") && ((EventDetails.ShapeData[i].Source == ModelSource) & ((EventDetails.ShapeData[i].ID == ID) | ((ModelSource == "ISAM") & (EventDetails.ShapeData[i].ID.Substring(EventDetails.ShapeData[i].ID.Length - 1) == ID.Substring(ID.Length - 1))))))
					{
						TextBox obj = txtModelPhase;
						((Control)lblCurrentPhase).set_Text(text = EventDetails.ShapeData[i].PhaseCorrn + "°");
						((Control)obj).set_Text(text);
						if (EventDetails.ShapeData[i].FitQuality < 0)
						{
							EventDetails.ShapeData[i].FitQuality = 0;
						}
						((ListControl)cmbModelFitQuality).set_SelectedIndex(EventDetails.ShapeData[i].FitQuality);
						((Control)lblCurrentFit).set_Text(cmbModelFitQuality.get_Items().get_Item(EventDetails.ShapeData[i].FitQuality).ToString());
						TextBox obj2 = txtModelMinDia;
						((Control)lblCurrentMinDia).set_Text(text = EventDetails.ShapeData[i].DiaMin.ToString());
						((Control)obj2).set_Text(text);
						TextBox obj3 = txtModelMaxDia;
						((Control)lblCurrentMaxDia).set_Text(text = EventDetails.ShapeData[i].DiaMax.ToString());
						((Control)obj3).set_Text(text);
						if ((((ListControl)cmbModelFitQuality).get_SelectedIndex() <= 3) | (((ListControl)cmbModelFitQuality).get_SelectedIndex() == 7))
						{
							((Control)txtModelMaxDia).set_Text("");
							EventDetails.ShapeData[i].DiaMax = 0.0;
							((Control)lblCurrentMaxDia).set_Text("- - - -");
							((Control)txtModelMaxDia).set_BackColor(Color.Pink);
						}
						else
						{
							((Control)txtModelMaxDia).set_BackColor(Control.get_DefaultBackColor());
						}
						if (EventDetails.ShapeData[i].DiaMin > 0.0)
						{
							((Control)txtModelMinDia).set_Text(EventDetails.ShapeData[i].DiaMin.ToString());
						}
						else
						{
							((Control)txtModelMinDia).set_Text("");
						}
						double.TryParse(((Control)txtModelMaxDia).get_Text(), out var result);
						EventDetails.ShapeData[i].DiaMax = result;
						if (result == 0.0)
						{
							((Control)txtModelMaxDia).set_Text("");
							((Control)lblCurrentMaxDia).set_Text("- - - -");
						}
						UnsavedShapeChanges = false;
						((Control)cmdSaveModelInfo).set_Enabled(true);
						return;
					}
				}
			}
			((Control)txtModelPhase).set_Text("0°");
			((ListControl)cmbModelFitQuality).set_SelectedIndex(0);
			TextBox obj4 = txtModelMinDia;
			((Control)txtModelMaxDia).set_Text(text = "");
			((Control)obj4).set_Text(text);
			((Control)lblCurrentPhase).set_Text("0°");
			((Control)lblCurrentFit).set_Text("Not fitted");
			((Control)lblCurrentMinDia).set_Text("- - - -");
			((Control)lblCurrentMaxDia).set_Text("- - - -");
		}

		private void panelGridAngles_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 1048576)
			{
				xPos_Grid = e.get_X();
				yPos_Grid = e.get_Y();
			}
		}

		private void panelGridAngles_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0018: Invalid comparison between Unknown and I4
			Panel val = (Panel)((sender is Panel) ? sender : null);
			if (val != null && (int)e.get_Button() == 1048576)
			{
				((Control)val).set_Top(((Control)val).get_Top() + (e.get_Y() - yPos_Grid));
				if (((Control)val).get_Top() < 4)
				{
					((Control)val).set_Top(4);
				}
				if (((Control)val).get_Top() > ((Control)this).get_Height() - ((Control)val).get_Height() - 40)
				{
					((Control)val).set_Top(((Control)this).get_Height() - ((Control)val).get_Height() - 40);
				}
				((Control)val).set_Left(((Control)val).get_Left() + (e.get_X() - xPos_Grid));
				if (((Control)val).get_Left() < 4)
				{
					((Control)val).set_Left(4);
				}
				if (((Control)val).get_Left() > ((Control)this).get_Width() - ((Control)val).get_Width() - 40)
				{
					((Control)val).set_Left(((Control)this).get_Width() - ((Control)val).get_Width() - 40);
				}
			}
		}

		private void dropAngles_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (mnuShowScaleGrid.get_Checked())
			{
				Data_and_Plots.GetPlotParametersFromForm();
				Data_and_Plots.PlotEventOnScreen();
			}
		}

		private void cmdGridPlus_Click(object sender, EventArgs e)
		{
			if (((ListControl)dropAngles).get_SelectedIndex() < dropAngles.get_Items().get_Count() - 1)
			{
				ComboBox obj = dropAngles;
				int selectedIndex = ((ListControl)obj).get_SelectedIndex();
				((ListControl)obj).set_SelectedIndex(selectedIndex + 1);
			}
		}

		private void cmdGridMinus_Click(object sender, EventArgs e)
		{
			if (((ListControl)dropAngles).get_SelectedIndex() > 0)
			{
				ComboBox obj = dropAngles;
				int selectedIndex = ((ListControl)obj).get_SelectedIndex();
				((ListControl)obj).set_SelectedIndex(selectedIndex - 1);
			}
		}

		private void addImageFromClipboardToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Data_and_Plots.PlotEventOnScreen();
		}

		private void showStarDiameterAnalyserToolStripMenuItem_Click(object sender, EventArgs e)
		{
			StarDiameterAnalysis.ShowStarDiameterAnalyser();
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
			//IL_4b00: Unknown result type (might be due to invalid IL or missing references)
			//IL_4b0a: Expected O, but got Unknown
			//IL_4bc8: Unknown result type (might be due to invalid IL or missing references)
			//IL_4bd2: Expected O, but got Unknown
			//IL_50fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_5108: Expected O, but got Unknown
			//IL_5404: Unknown result type (might be due to invalid IL or missing references)
			//IL_540e: Expected O, but got Unknown
			//IL_54c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_54d2: Expected O, but got Unknown
			//IL_560e: Unknown result type (might be due to invalid IL or missing references)
			//IL_5618: Expected O, but got Unknown
			//IL_59b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_59bf: Expected O, but got Unknown
			//IL_6211: Unknown result type (might be due to invalid IL or missing references)
			//IL_621b: Expected O, but got Unknown
			//IL_6256: Unknown result type (might be due to invalid IL or missing references)
			//IL_6260: Expected O, but got Unknown
			//IL_62cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_62d6: Expected O, but got Unknown
			//IL_6311: Unknown result type (might be due to invalid IL or missing references)
			//IL_631b: Expected O, but got Unknown
			//IL_748f: Unknown result type (might be due to invalid IL or missing references)
			//IL_7499: Expected O, but got Unknown
			//IL_74bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_74c7: Expected O, but got Unknown
			//IL_7533: Unknown result type (might be due to invalid IL or missing references)
			//IL_753d: Expected O, but got Unknown
			//IL_7561: Unknown result type (might be due to invalid IL or missing references)
			//IL_756b: Expected O, but got Unknown
			//IL_83b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_83c0: Expected O, but got Unknown
			//IL_8444: Unknown result type (might be due to invalid IL or missing references)
			//IL_844e: Expected O, but got Unknown
			//IL_9b30: Unknown result type (might be due to invalid IL or missing references)
			//IL_9b3a: Expected O, but got Unknown
			//IL_9bcd: Unknown result type (might be due to invalid IL or missing references)
			//IL_9bd7: Expected O, but got Unknown
			//IL_9e7a: Unknown result type (might be due to invalid IL or missing references)
			//IL_9e84: Expected O, but got Unknown
			//IL_9f8c: Unknown result type (might be due to invalid IL or missing references)
			//IL_9f96: Expected O, but got Unknown
			//IL_9fa3: Unknown result type (might be due to invalid IL or missing references)
			//IL_9fad: Expected O, but got Unknown
			//IL_a483: Unknown result type (might be due to invalid IL or missing references)
			//IL_a48d: Expected O, but got Unknown
			//IL_a4b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_a4bb: Expected O, but got Unknown
			//IL_b6c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_b6cb: Expected O, but got Unknown
			//IL_b6d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_b6e2: Expected O, but got Unknown
			//IL_b7f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_b800: Expected O, but got Unknown
			//IL_b88c: Unknown result type (might be due to invalid IL or missing references)
			//IL_b896: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(PlotObservations));
			menuStrip1 = new MenuStrip();
			withPlotToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveEntireFormAsAnImageToolStripMenuItem = new ToolStripMenuItem();
			saveImageToolStripMenuItem = new ToolStripMenuItem();
			setScalecurrently100ToMatchMonitorScaleToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyObserverListToolStripMenuItem = new ToolStripMenuItem();
			saveObserverListToolStripMenuItem = new ToolStripMenuItem();
			plotOptionsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator13 = new ToolStripSeparator();
			legendToolStripMenuItem = new ToolStripMenuItem();
			mnuShowSolution = new ToolStripMenuItem();
			mnuShowTitle = new ToolStripMenuItem();
			addThisTextToTheTitleToolStripMenuItem = new ToolStripMenuItem();
			toolStrip_txtExtra = new ToolStripTextBox();
			toolStripSeparator14 = new ToolStripSeparator();
			showPLOTSCALEmasToolStripMenuItem = new ToolStripMenuItem();
			mnuShowPlotScalekm = new ToolStripMenuItem();
			mnuShowScaleGrid = new ToolStripMenuItem();
			setOrientationToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator7 = new ToolStripSeparator();
			kmToolStripMenuItem_100m = new ToolStripMenuItem();
			kmToolStripMenuItem1_200m = new ToolStripMenuItem();
			kmToolStripMenuItem_500m = new ToolStripMenuItem();
			kmToolStripMenuItem_1 = new ToolStripMenuItem();
			kmToolStripMenuItem_2 = new ToolStripMenuItem();
			kmToolStripMenuItem_5 = new ToolStripMenuItem();
			kmToolStripMenuItem_10 = new ToolStripMenuItem();
			kmToolStripMenuItem_20 = new ToolStripMenuItem();
			kmToolStripMenuItem_50 = new ToolStripMenuItem();
			kmToolStripMenuItem_100 = new ToolStripMenuItem();
			kmToolStripMenuItem_200 = new ToolStripMenuItem();
			kmToolStripMenuItem_500 = new ToolStripMenuItem();
			toolStripSeparator15 = new ToolStripSeparator();
			showStarWithADiameterOfToolStripMenuItem = new ToolStripMenuItem();
			toolStripStarDia_mas = new ToolStripTextBox();
			drawStarImageAtPlotCenterToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			plotColorsLineWidthsWatermarksToolStripMenuItem = new ToolStripMenuItem();
			mnuWhiteBackground = new ToolStripMenuItem();
			mnuGrayBackground = new ToolStripMenuItem();
			mnuBlackBackground = new ToolStripMenuItem();
			showAsteroidEllipseInColorToolStripMenuItem = new ToolStripMenuItem();
			mnuPathsInColour = new ToolStripMenuItem();
			showEventInColorToolStripMenuItem = new ToolStripMenuItem();
			showMISSEventsAsDashedLinesToolStripMenuItem = new ToolStripMenuItem();
			mnuThickLines_Paths = new ToolStripMenuItem();
			mnuThickLines_EllipseEvents = new ToolStripMenuItem();
			setTHICKLineWidthInUserSettingsGroup8ToolStripMenuItem = new ToolStripMenuItem();
			mnuLargeLabels = new ToolStripMenuItem();
			lARGEFontSizeSetInUserSettingsGroup8ToolStripMenuItem = new ToolStripMenuItem();
			applyWatermark = new ToolStripMenuItem();
			applyProvisionalMarkToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator8 = new ToolStripSeparator();
			shapeModelsToolStripMenuItem = new ToolStripMenuItem();
			addShapeImageFromDAMIT_ISAMToolStripMenuItem = new ToolStripMenuItem();
			showShapeModelControlsOnPlotToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator17 = new ToolStripSeparator();
			includeAsterodIDAndModelIDToolStripMenuItem = new ToolStripMenuItem();
			showPolarAxesToolStripMenuItem = new ToolStripMenuItem();
			showModelFacesToolStripMenuItem = new ToolStripMenuItem();
			plotModelDarkToolStripMenuItem = new ToolStripMenuItem();
			plotModelInGreyScaleToolStripMenuItem = new ToolStripMenuItem();
			mnuShowMeanDiameterDAMIT = new ToolStripMenuItem();
			showModelMeanDiameterLinesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator16 = new ToolStripSeparator();
			addImageFromClipboardToolStripMenuItem = new ToolStripMenuItem();
			seeShapeModelDisplayFormForMorePlotOptionsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator6 = new ToolStripSeparator();
			mnuShowEllipse = new ToolStripMenuItem();
			showFresnelDiffractionPeakToolStripMenuItem = new ToolStripMenuItem();
			mnuShowEllipseInOutline = new ToolStripMenuItem();
			drawAsteroidProfileToolStripMenuItem = new ToolStripMenuItem();
			mnuShowAxes = new ToolStripMenuItem();
			showSatelliteOrbitsifAnyToolStripMenuItem = new ToolStripMenuItem();
			showASTROMETRYLocation = new ToolStripMenuItem();
			mnuShowMarkers = new ToolStripMenuItem();
			mnuShowID = new ToolStripMenuItem();
			mnuShowPredicted = new ToolStripMenuItem();
			mnuAlign = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			mnuShowErrors = new ToolStripMenuItem();
			ErrorBarsForFittingToShapeModels_menu = new ToolStripMenuItem();
			mnuShowPathsOcculted = new ToolStripMenuItem();
			mnuShowPathsVisible = new ToolStripMenuItem();
			showRINGEventPathsToolStripMenuItem = new ToolStripMenuItem();
			mnuIncludeTimeShift = new ToolStripMenuItem();
			toolStripSeparator4 = new ToolStripSeparator();
			mnuShowPathsMiss = new ToolStripMenuItem();
			mnuShowVideo = new ToolStripMenuItem();
			mnuShowZeroWeighted = new ToolStripMenuItem();
			mnuShowCloud = new ToolStripMenuItem();
			toolStripSeparator5 = new ToolStripSeparator();
			toolStripSeparator10 = new ToolStripSeparator();
			setPlotOptionsForPublicationPlotToolStripMenuItem = new ToolStripMenuItem();
			applyDEFAULTSettingsToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator12 = new ToolStripSeparator();
			saveCurrentSettingsAsDefaultToolStripMenuItem = new ToolStripMenuItem();
			restoreOccultDefaultValuesToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator9 = new ToolStripSeparator();
			toolStripSeparator11 = new ToolStripSeparator();
			showStarDiameterAnalyserToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			keepOnTopToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			grpBestFit = new GroupBox();
			label63 = new Label();
			pnlReview = new Panel();
			lblDiaEquiv = new Label();
			panelLimbFit = new Panel();
			trackLimbFit = new TrackBar();
			lblLimbFit = new Label();
			panel5 = new Panel();
			chkScoll = new CheckBox();
			label16 = new Label();
			label15 = new Label();
			trackOpacity = new TrackBar();
			label18 = new Label();
			panel4 = new Panel();
			optPlotx5 = new RadioButton();
			optyPlotNormal = new RadioButton();
			optPlotx2 = new RadioButton();
			label10 = new Label();
			SliderScale = new TrackBar();
			label45 = new Label();
			label44 = new Label();
			updnCenterOfMass_Y = new NumericUpDown();
			updnCenterOfMass_X = new NumericUpDown();
			cmdSet3Checks = new Button();
			chkShapeModelCentered = new CheckBox();
			pnlDouble = new Panel();
			optSecondary = new RadioButton();
			optPrimary = new RadioButton();
			label46 = new Label();
			optBoth = new RadioButton();
			lblMotion = new Label();
			PanelLegend = new Panel();
			picLegend = new PictureBox();
			lblAxisRatio = new Label();
			cmdFit = new Button();
			lblRMS = new Label();
			lstObservers = new ListBox();
			chkCircle = new CheckBox();
			chkUseAssumedDiameter = new CheckBox();
			chkY = new CheckBox();
			chkMiss = new CheckBox();
			chkA = new CheckBox();
			chkB = new CheckBox();
			chkPA = new CheckBox();
			chkX = new CheckBox();
			label9 = new Label();
			cmbQuality = new ComboBox();
			label8 = new Label();
			label7 = new Label();
			label6 = new Label();
			label5 = new Label();
			label1 = new Label();
			updnY = new NumericUpDown();
			updnA = new NumericUpDown();
			updnB = new NumericUpDown();
			updnPA = new NumericUpDown();
			updnX = new NumericUpDown();
			PanelSatellites = new Panel();
			picDrag = new PictureBox();
			pnlCBET = new Panel();
			chkEditCBET = new CheckBox();
			label66 = new Label();
			cmdEstimateOrbit = new Button();
			pnlNames = new Panel();
			label43 = new Label();
			lblSat4 = new Label();
			lblSat1 = new Label();
			lblSat3 = new Label();
			lblSat2 = new Label();
			label42 = new Label();
			label41 = new Label();
			label40 = new Label();
			label39 = new Label();
			lblSatelliteNumbers = new Label();
			label2 = new Label();
			label14 = new Label();
			label38 = new Label();
			label37 = new Label();
			label36 = new Label();
			label28 = new Label();
			updnSatNum = new NumericUpDown();
			PanelMiriade = new Panel();
			label34 = new Label();
			label33 = new Label();
			label32 = new Label();
			label31 = new Label();
			label29 = new Label();
			cmdGetSatelliteMotions = new Button();
			lblSatelliteQuality = new Label();
			lblSatNum = new Label();
			cmdHelpOnCompanionID = new Button();
			label30 = new Label();
			panelDouble = new Panel();
			picDragDouble = new PictureBox();
			panelJDSO = new Panel();
			panel3 = new Panel();
			chkEditJDSO = new CheckBox();
			panel1 = new Panel();
			txtDoublePairID = new TextBox();
			label64 = new Label();
			label62 = new Label();
			label61 = new Label();
			txtJDSO_Vol_Num_Pg = new TextBox();
			txtJDSOSubmitDate = new TextBox();
			label59 = new Label();
			label60 = new Label();
			panel7 = new Panel();
			label58 = new Label();
			label49 = new Label();
			label13 = new Label();
			lblOffset = new Label();
			txtMag2nd = new TextBox();
			txtMagMain = new TextBox();
			label56 = new Label();
			label55 = new Label();
			label54 = new Label();
			label53 = new Label();
			txt2ndDrop = new TextBox();
			txtMainDrop = new TextBox();
			label48 = new Label();
			label47 = new Label();
			updnBrightRatio = new NumericUpDown();
			updnBrightnessUncertPerCent = new NumericUpDown();
			panelSolve = new Panel();
			pnlOffset = new Panel();
			panel9 = new Panel();
			lbl4 = new Label();
			lbl3 = new Label();
			label57 = new Label();
			label51 = new Label();
			panel8 = new Panel();
			lbl2 = new Label();
			lbl1 = new Label();
			label52 = new Label();
			cmdSetOffsets = new Button();
			cmdFit_Doubles = new Button();
			lblSolveSolnNum = new Label();
			label4 = new Label();
			label3 = new Label();
			chkCompanionPA = new CheckBox();
			chkCompanionSep = new CheckBox();
			panel6 = new Panel();
			opt1Component = new RadioButton();
			textBox1 = new TextBox();
			label50 = new Label();
			opt4 = new RadioButton();
			optTwo = new RadioButton();
			optSingle = new RadioButton();
			cmdHelpDoubles = new Button();
			pnlDoubleStarSolution = new Panel();
			panel10 = new Panel();
			lblSelectSep = new Panel();
			lblNumberOfSolutions = new Label();
			lblSelectSth = new Label();
			lblSelectNth = new Label();
			lblDoubleStar = new Label();
			Vbar = new VScrollBar();
			Hbar = new HScrollBar();
			lblTag = new Label();
			cmdMissTimes = new Button();
			cmdShowEditor = new Button();
			toolTip = new ToolTip(components);
			cmbModelFitQuality = new ComboBox();
			panelGridAngles = new Panel();
			cmdGridMinus = new Button();
			cmdGridPlus = new Button();
			label17 = new Label();
			dropAngles = new ComboBox();
			cmdCloseGridAngles = new Button();
			panelShapeModelControl = new Panel();
			lblQualityExplanation = new Label();
			cmdForDiameters = new Button();
			cmdShapeHelp = new Button();
			label35 = new Label();
			panel2 = new Panel();
			label23 = new Label();
			cmdSetModelMax = new Button();
			cmdSetModelMin = new Button();
			label20 = new Label();
			label19 = new Label();
			lblCurrentPhase = new Label();
			txtModelMaxDia = new TextBox();
			txtModelPhase = new TextBox();
			txtModelMinDia = new TextBox();
			lblCurrentFit = new Label();
			lblCurrentMinDia = new Label();
			lblCurrentMaxDia = new Label();
			label25 = new Label();
			label27 = new Label();
			txtSurface_Volume_Ratio = new TextBox();
			cmdGetLightCurves = new Button();
			label26 = new Label();
			label24 = new Label();
			txtVersion = new TextBox();
			label22 = new Label();
			txtModelID = new TextBox();
			cmdSaveModelInfo = new Button();
			label21 = new Label();
			txtModelSource = new TextBox();
			label12 = new Label();
			label11 = new Label();
			cmdCloseShapeModelControl = new Button();
			tbarScaleAdjust = new TrackBar();
			picPlot = new PictureBox();
			((Control)menuStrip1).SuspendLayout();
			((Control)grpBestFit).SuspendLayout();
			((Control)panelLimbFit).SuspendLayout();
			((ISupportInitialize)trackLimbFit).BeginInit();
			((Control)panel5).SuspendLayout();
			((ISupportInitialize)trackOpacity).BeginInit();
			((Control)panel4).SuspendLayout();
			((ISupportInitialize)SliderScale).BeginInit();
			((ISupportInitialize)updnCenterOfMass_Y).BeginInit();
			((ISupportInitialize)updnCenterOfMass_X).BeginInit();
			((Control)pnlDouble).SuspendLayout();
			((Control)PanelLegend).SuspendLayout();
			((ISupportInitialize)picLegend).BeginInit();
			((ISupportInitialize)updnY).BeginInit();
			((ISupportInitialize)updnA).BeginInit();
			((ISupportInitialize)updnB).BeginInit();
			((ISupportInitialize)updnPA).BeginInit();
			((ISupportInitialize)updnX).BeginInit();
			((Control)PanelSatellites).SuspendLayout();
			((ISupportInitialize)picDrag).BeginInit();
			((Control)pnlCBET).SuspendLayout();
			((Control)pnlNames).SuspendLayout();
			((ISupportInitialize)updnSatNum).BeginInit();
			((Control)PanelMiriade).SuspendLayout();
			((Control)panelDouble).SuspendLayout();
			((ISupportInitialize)picDragDouble).BeginInit();
			((Control)panelJDSO).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)panel7).SuspendLayout();
			((ISupportInitialize)updnBrightRatio).BeginInit();
			((ISupportInitialize)updnBrightnessUncertPerCent).BeginInit();
			((Control)panelSolve).SuspendLayout();
			((Control)pnlOffset).SuspendLayout();
			((Control)panel9).SuspendLayout();
			((Control)panel8).SuspendLayout();
			((Control)panel6).SuspendLayout();
			((Control)pnlDoubleStarSolution).SuspendLayout();
			((Control)panelGridAngles).SuspendLayout();
			((Control)panelShapeModelControl).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((ISupportInitialize)tbarScaleAdjust).BeginInit();
			((ISupportInitialize)picPlot).BeginInit();
			((Control)this).SuspendLayout();
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)withPlotToolStripMenuItem,
				(ToolStripItem)plotOptionsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)keepOnTopToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(802, 24));
			((Control)menuStrip1).set_TabIndex(1);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPlotToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyObserverListToolStripMenuItem,
				(ToolStripItem)saveObserverListToolStripMenuItem
			});
			((ToolStripItem)withPlotToolStripMenuItem).set_Name("withPlotToolStripMenuItem");
			((ToolStripItem)withPlotToolStripMenuItem).set_Size(new Size(75, 20));
			((ToolStripItem)withPlotToolStripMenuItem).set_Text("with Plot...");
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(299, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(299, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(299, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)saveToolStripMenuItem).set_ForeColor(Color.Navy);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(299, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save plot image, && legend image");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)saveEntireFormAsAnImageToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)saveImageToolStripMenuItem,
				(ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem
			});
			((ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem).set_Name("saveEntireFormAsAnImageToolStripMenuItem");
			((ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem).set_Size(new Size(299, 22));
			((ToolStripItem)saveEntireFormAsAnImageToolStripMenuItem).set_Text("Save  image of this entire form");
			((ToolStripItem)saveImageToolStripMenuItem).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)saveImageToolStripMenuItem).set_Name("saveImageToolStripMenuItem");
			((ToolStripItem)saveImageToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)saveImageToolStripMenuItem).set_Text("Save image");
			((ToolStripItem)saveImageToolStripMenuItem).add_Click((EventHandler)saveImageToolStripMenuItem_Click);
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).set_Name("setScalecurrently100ToMatchMonitorScaleToolStripMenuItem");
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).set_Size(new Size(334, 22));
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).set_Text("Set scale (currently 100%) to match Monitor scale");
			((ToolStripItem)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem).add_Click((EventHandler)setScalecurrently100ToMatchMonitorScaleToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(296, 6));
			((ToolStripItem)copyObserverListToolStripMenuItem).set_Name("copyObserverListToolStripMenuItem");
			copyObserverListToolStripMenuItem.set_ShortcutKeys((Keys)196675);
			((ToolStripItem)copyObserverListToolStripMenuItem).set_Size(new Size(299, 22));
			((ToolStripItem)copyObserverListToolStripMenuItem).set_Text("Copy observer list (text)");
			((ToolStripItem)copyObserverListToolStripMenuItem).add_Click((EventHandler)copyObserverListToolStripMenuItem_Click);
			((ToolStripItem)saveObserverListToolStripMenuItem).set_Name("saveObserverListToolStripMenuItem");
			saveObserverListToolStripMenuItem.set_ShortcutKeys((Keys)196691);
			((ToolStripItem)saveObserverListToolStripMenuItem).set_Size(new Size(299, 22));
			((ToolStripItem)saveObserverListToolStripMenuItem).set_Text("Save observer list (text)");
			((ToolStripItem)saveObserverListToolStripMenuItem).add_Click((EventHandler)saveObserverListToolStripMenuItem_Click);
			((ToolStripDropDownItem)plotOptionsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[41]
			{
				(ToolStripItem)toolStripSeparator13,
				(ToolStripItem)legendToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)plotColorsLineWidthsWatermarksToolStripMenuItem,
				(ToolStripItem)toolStripSeparator8,
				(ToolStripItem)shapeModelsToolStripMenuItem,
				(ToolStripItem)seeShapeModelDisplayFormForMorePlotOptionsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator6,
				(ToolStripItem)mnuShowEllipse,
				(ToolStripItem)showFresnelDiffractionPeakToolStripMenuItem,
				(ToolStripItem)mnuShowEllipseInOutline,
				(ToolStripItem)drawAsteroidProfileToolStripMenuItem,
				(ToolStripItem)mnuShowAxes,
				(ToolStripItem)showSatelliteOrbitsifAnyToolStripMenuItem,
				(ToolStripItem)showASTROMETRYLocation,
				(ToolStripItem)mnuShowMarkers,
				(ToolStripItem)mnuShowID,
				(ToolStripItem)mnuShowPredicted,
				(ToolStripItem)mnuAlign,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)mnuShowErrors,
				(ToolStripItem)ErrorBarsForFittingToShapeModels_menu,
				(ToolStripItem)mnuShowPathsOcculted,
				(ToolStripItem)mnuShowPathsVisible,
				(ToolStripItem)showRINGEventPathsToolStripMenuItem,
				(ToolStripItem)mnuIncludeTimeShift,
				(ToolStripItem)toolStripSeparator4,
				(ToolStripItem)mnuShowPathsMiss,
				(ToolStripItem)mnuShowVideo,
				(ToolStripItem)mnuShowZeroWeighted,
				(ToolStripItem)mnuShowCloud,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)toolStripSeparator10,
				(ToolStripItem)setPlotOptionsForPublicationPlotToolStripMenuItem,
				(ToolStripItem)applyDEFAULTSettingsToolStripMenuItem,
				(ToolStripItem)toolStripSeparator12,
				(ToolStripItem)saveCurrentSettingsAsDefaultToolStripMenuItem,
				(ToolStripItem)restoreOccultDefaultValuesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator9,
				(ToolStripItem)toolStripSeparator11,
				(ToolStripItem)showStarDiameterAnalyserToolStripMenuItem
			});
			((ToolStripItem)plotOptionsToolStripMenuItem).set_Name("plotOptionsToolStripMenuItem");
			((ToolStripItem)plotOptionsToolStripMenuItem).set_Size(new Size(92, 20));
			((ToolStripItem)plotOptionsToolStripMenuItem).set_Text("Plot options...");
			((ToolStripItem)toolStripSeparator13).set_Name("toolStripSeparator13");
			((ToolStripItem)toolStripSeparator13).set_Size(new Size(376, 6));
			((ToolStripItem)legendToolStripMenuItem).set_BackColor(Color.OldLace);
			((ToolStripDropDownItem)legendToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[10]
			{
				(ToolStripItem)mnuShowSolution,
				(ToolStripItem)mnuShowTitle,
				(ToolStripItem)addThisTextToTheTitleToolStripMenuItem,
				(ToolStripItem)toolStripSeparator14,
				(ToolStripItem)showPLOTSCALEmasToolStripMenuItem,
				(ToolStripItem)mnuShowPlotScalekm,
				(ToolStripItem)mnuShowScaleGrid,
				(ToolStripItem)toolStripSeparator15,
				(ToolStripItem)showStarWithADiameterOfToolStripMenuItem,
				(ToolStripItem)drawStarImageAtPlotCenterToolStripMenuItem
			});
			((ToolStripItem)legendToolStripMenuItem).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)legendToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)legendToolStripMenuItem).set_Name("legendToolStripMenuItem");
			((ToolStripItem)legendToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)legendToolStripMenuItem).set_Text("Title,  Solution,  Scales,  Grid,  Star (size && location)");
			mnuShowSolution.set_Checked(true);
			mnuShowSolution.set_CheckState((CheckState)1);
			((ToolStripItem)mnuShowSolution).set_Name("mnuShowSolution");
			((ToolStripItem)mnuShowSolution).set_Size(new Size(271, 22));
			((ToolStripItem)mnuShowSolution).set_Text("Show SOLUTION values on plot ...or...");
			((ToolStripItem)mnuShowSolution).add_Click((EventHandler)mnuShowSolution_Click);
			((ToolStripItem)mnuShowTitle).set_Name("mnuShowTitle");
			((ToolStripItem)mnuShowTitle).set_Size(new Size(271, 22));
			((ToolStripItem)mnuShowTitle).set_Text("Show PLOT TITLE");
			((ToolStripItem)mnuShowTitle).add_Click((EventHandler)mnuShowTitle_Click);
			((ToolStripDropDownItem)addThisTextToTheTitleToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)toolStrip_txtExtra });
			((ToolStripItem)addThisTextToTheTitleToolStripMenuItem).set_Name("addThisTextToTheTitleToolStripMenuItem");
			((ToolStripItem)addThisTextToTheTitleToolStripMenuItem).set_Size(new Size(271, 22));
			((ToolStripItem)addThisTextToTheTitleToolStripMenuItem).set_Text("... add this text to PLOT TITLE");
			((ToolStripItem)addThisTextToTheTitleToolStripMenuItem).add_Click((EventHandler)addThisTextToTheTitleToolStripMenuItem_Click);
			((ToolStripItem)toolStrip_txtExtra).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)toolStrip_txtExtra).set_Name("toolStrip_txtExtra");
			((ToolStripItem)toolStrip_txtExtra).set_Size(new Size(100, 23));
			((ToolStripItem)toolStrip_txtExtra).set_Text("Config. 1");
			((ToolStripItem)toolStripSeparator14).set_Name("toolStripSeparator14");
			((ToolStripItem)toolStripSeparator14).set_Size(new Size(268, 6));
			showPLOTSCALEmasToolStripMenuItem.set_Checked(true);
			showPLOTSCALEmasToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)showPLOTSCALEmasToolStripMenuItem).set_Name("showPLOTSCALEmasToolStripMenuItem");
			((ToolStripItem)showPLOTSCALEmasToolStripMenuItem).set_Size(new Size(271, 22));
			((ToolStripItem)showPLOTSCALEmasToolStripMenuItem).set_Text("Show PLOT SCALE  -  mas   ( ↕ )");
			((ToolStripItem)showPLOTSCALEmasToolStripMenuItem).add_Click((EventHandler)showPLOTSCALEmasToolStripMenuItem_Click);
			((ToolStripItem)mnuShowPlotScalekm).set_Name("mnuShowPlotScalekm");
			((ToolStripItem)mnuShowPlotScalekm).set_Size(new Size(271, 22));
			((ToolStripItem)mnuShowPlotScalekm).set_Text("Show PLOT SCALE  -  km   ( ↔ )");
			((ToolStripItem)mnuShowPlotScalekm).add_Click((EventHandler)mnuShowPlotScalekm_Click);
			mnuShowScaleGrid.set_Checked(true);
			mnuShowScaleGrid.set_CheckState((CheckState)1);
			((ToolStripDropDownItem)mnuShowScaleGrid).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[14]
			{
				(ToolStripItem)setOrientationToolStripMenuItem,
				(ToolStripItem)toolStripSeparator7,
				(ToolStripItem)kmToolStripMenuItem_100m,
				(ToolStripItem)kmToolStripMenuItem1_200m,
				(ToolStripItem)kmToolStripMenuItem_500m,
				(ToolStripItem)kmToolStripMenuItem_1,
				(ToolStripItem)kmToolStripMenuItem_2,
				(ToolStripItem)kmToolStripMenuItem_5,
				(ToolStripItem)kmToolStripMenuItem_10,
				(ToolStripItem)kmToolStripMenuItem_20,
				(ToolStripItem)kmToolStripMenuItem_50,
				(ToolStripItem)kmToolStripMenuItem_100,
				(ToolStripItem)kmToolStripMenuItem_200,
				(ToolStripItem)kmToolStripMenuItem_500
			});
			((ToolStripItem)mnuShowScaleGrid).set_Name("mnuShowScaleGrid");
			((ToolStripItem)mnuShowScaleGrid).set_Size(new Size(271, 22));
			((ToolStripItem)mnuShowScaleGrid).set_Text("Show SCALE GRID (km)");
			((ToolStripItem)mnuShowScaleGrid).add_Click((EventHandler)mnuShowScaleGrid_Click);
			((ToolStripItem)setOrientationToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Italic));
			((ToolStripItem)setOrientationToolStripMenuItem).set_Name("setOrientationToolStripMenuItem");
			setOrientationToolStripMenuItem.set_ShortcutKeys((Keys)196687);
			((ToolStripItem)setOrientationToolStripMenuItem).set_Size(new Size(225, 22));
			((ToolStripItem)setOrientationToolStripMenuItem).set_Text("Set orientation");
			((ToolStripItem)setOrientationToolStripMenuItem).add_Click((EventHandler)setOrientationToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator7).set_Name("toolStripSeparator7");
			((ToolStripItem)toolStripSeparator7).set_Size(new Size(222, 6));
			((ToolStripItem)kmToolStripMenuItem_100m).set_Name("kmToolStripMenuItem_100m");
			((ToolStripItem)kmToolStripMenuItem_100m).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem_100m).set_Text("100 m");
			((ToolStripItem)kmToolStripMenuItem_100m).add_Click((EventHandler)kmToolStripMenuItem__100m_Click);
			((ToolStripItem)kmToolStripMenuItem1_200m).set_Name("kmToolStripMenuItem1_200m");
			((ToolStripItem)kmToolStripMenuItem1_200m).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem1_200m).set_Text("200 m");
			((ToolStripItem)kmToolStripMenuItem1_200m).add_Click((EventHandler)kmToolStripMenuItem1_200m_Click);
			((ToolStripItem)kmToolStripMenuItem_500m).set_Name("kmToolStripMenuItem_500m");
			((ToolStripItem)kmToolStripMenuItem_500m).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem_500m).set_Text("500 m");
			((ToolStripItem)kmToolStripMenuItem_500m).add_Click((EventHandler)kmToolStripMenuItem_500m_Click);
			((ToolStripItem)kmToolStripMenuItem_1).set_Name("kmToolStripMenuItem_1");
			kmToolStripMenuItem_1.set_ShortcutKeys((Keys)131120);
			((ToolStripItem)kmToolStripMenuItem_1).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem_1).set_Text("1 km");
			((ToolStripItem)kmToolStripMenuItem_1).add_Click((EventHandler)kmToolStripMenuItem_1_Click);
			((ToolStripItem)kmToolStripMenuItem_2).set_Name("kmToolStripMenuItem_2");
			kmToolStripMenuItem_2.set_ShortcutKeys((Keys)131121);
			((ToolStripItem)kmToolStripMenuItem_2).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem_2).set_Text("2 km");
			((ToolStripItem)kmToolStripMenuItem_2).add_Click((EventHandler)kmToolStripMenuItem_2_Click);
			((ToolStripItem)kmToolStripMenuItem_5).set_Name("kmToolStripMenuItem_5");
			kmToolStripMenuItem_5.set_ShortcutKeys((Keys)131122);
			((ToolStripItem)kmToolStripMenuItem_5).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem_5).set_Text("5 km");
			((ToolStripItem)kmToolStripMenuItem_5).add_Click((EventHandler)kmToolStripMenuItem_5_Click);
			kmToolStripMenuItem_10.set_Checked(true);
			kmToolStripMenuItem_10.set_CheckState((CheckState)1);
			((ToolStripItem)kmToolStripMenuItem_10).set_Name("kmToolStripMenuItem_10");
			kmToolStripMenuItem_10.set_ShortcutKeys((Keys)131123);
			((ToolStripItem)kmToolStripMenuItem_10).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem_10).set_Text("10 km");
			((ToolStripItem)kmToolStripMenuItem_10).add_Click((EventHandler)kmToolStripMenuItem_10_Click);
			((ToolStripItem)kmToolStripMenuItem_20).set_Name("kmToolStripMenuItem_20");
			kmToolStripMenuItem_20.set_ShortcutKeys((Keys)131124);
			((ToolStripItem)kmToolStripMenuItem_20).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem_20).set_Text("20 km");
			((ToolStripItem)kmToolStripMenuItem_20).add_Click((EventHandler)kmToolStripMenuItem_20_Click);
			((ToolStripItem)kmToolStripMenuItem_50).set_Name("kmToolStripMenuItem_50");
			kmToolStripMenuItem_50.set_ShortcutKeys((Keys)131125);
			((ToolStripItem)kmToolStripMenuItem_50).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem_50).set_Text("50 km");
			((ToolStripItem)kmToolStripMenuItem_50).add_Click((EventHandler)kmToolStripMenuItem_50_Click);
			((ToolStripItem)kmToolStripMenuItem_100).set_Name("kmToolStripMenuItem_100");
			kmToolStripMenuItem_100.set_ShortcutKeys((Keys)131126);
			((ToolStripItem)kmToolStripMenuItem_100).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem_100).set_Text("100 km");
			((ToolStripItem)kmToolStripMenuItem_100).add_Click((EventHandler)kmToolStripMenuItem_100_Click);
			((ToolStripItem)kmToolStripMenuItem_200).set_Name("kmToolStripMenuItem_200");
			kmToolStripMenuItem_200.set_ShortcutKeys((Keys)131127);
			((ToolStripItem)kmToolStripMenuItem_200).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem_200).set_Text("200 km");
			((ToolStripItem)kmToolStripMenuItem_200).add_Click((EventHandler)kmToolStripMenuItem_200_Click);
			((ToolStripItem)kmToolStripMenuItem_500).set_Name("kmToolStripMenuItem_500");
			kmToolStripMenuItem_500.set_ShortcutKeys((Keys)131128);
			((ToolStripItem)kmToolStripMenuItem_500).set_Size(new Size(225, 22));
			((ToolStripItem)kmToolStripMenuItem_500).set_Text("500 km");
			((ToolStripItem)kmToolStripMenuItem_500).add_Click((EventHandler)kmToolStripMenuItem_500_Click);
			((ToolStripItem)toolStripSeparator15).set_Name("toolStripSeparator15");
			((ToolStripItem)toolStripSeparator15).set_Size(new Size(268, 6));
			showStarWithADiameterOfToolStripMenuItem.set_Checked(Settings.Default.AsterPlot_ShowStar);
			((ToolStripDropDownItem)showStarWithADiameterOfToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)toolStripStarDia_mas });
			((ToolStripItem)showStarWithADiameterOfToolStripMenuItem).set_Name("showStarWithADiameterOfToolStripMenuItem");
			((ToolStripItem)showStarWithADiameterOfToolStripMenuItem).set_Size(new Size(271, 22));
			((ToolStripItem)showStarWithADiameterOfToolStripMenuItem).set_Text("Show star, with a diameter (mas) of");
			((ToolStripItem)showStarWithADiameterOfToolStripMenuItem).add_Click((EventHandler)showStarWithADiameterOfToolStripMenuItem_Click);
			((ToolStripItem)toolStripStarDia_mas).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)toolStripStarDia_mas).set_Name("toolStripStarDia_mas");
			((ToolStripItem)toolStripStarDia_mas).set_Size(new Size(30, 23));
			((ToolStripItem)toolStripStarDia_mas).set_Text(Settings.Default.Asterplot_StarDia);
			((ToolStripItem)toolStripStarDia_mas).add_Click((EventHandler)toolStripStarDia_mas_Click);
			((ToolStripItem)drawStarImageAtPlotCenterToolStripMenuItem).set_Name("drawStarImageAtPlotCenterToolStripMenuItem");
			((ToolStripItem)drawStarImageAtPlotCenterToolStripMenuItem).set_Size(new Size(271, 22));
			((ToolStripItem)drawStarImageAtPlotCenterToolStripMenuItem).set_Text("Draw star image at plot center");
			((ToolStripItem)drawStarImageAtPlotCenterToolStripMenuItem).add_Click((EventHandler)drawStarImageAtPlotCenterToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(376, 6));
			((ToolStripItem)plotColorsLineWidthsWatermarksToolStripMenuItem).set_BackColor(Color.OldLace);
			((ToolStripDropDownItem)plotColorsLineWidthsWatermarksToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[14]
			{
				(ToolStripItem)mnuWhiteBackground,
				(ToolStripItem)mnuGrayBackground,
				(ToolStripItem)mnuBlackBackground,
				(ToolStripItem)showAsteroidEllipseInColorToolStripMenuItem,
				(ToolStripItem)mnuPathsInColour,
				(ToolStripItem)showEventInColorToolStripMenuItem,
				(ToolStripItem)showMISSEventsAsDashedLinesToolStripMenuItem,
				(ToolStripItem)mnuThickLines_Paths,
				(ToolStripItem)mnuThickLines_EllipseEvents,
				(ToolStripItem)setTHICKLineWidthInUserSettingsGroup8ToolStripMenuItem,
				(ToolStripItem)mnuLargeLabels,
				(ToolStripItem)lARGEFontSizeSetInUserSettingsGroup8ToolStripMenuItem,
				(ToolStripItem)applyWatermark,
				(ToolStripItem)applyProvisionalMarkToolStripMenuItem
			});
			((ToolStripItem)plotColorsLineWidthsWatermarksToolStripMenuItem).set_ForeColor(Color.Red);
			((ToolStripItem)plotColorsLineWidthsWatermarksToolStripMenuItem).set_Name("plotColorsLineWidthsWatermarksToolStripMenuItem");
			((ToolStripItem)plotColorsLineWidthsWatermarksToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)plotColorsLineWidthsWatermarksToolStripMenuItem).set_Text("Plot Colors, Line widths, Watermarks");
			((ToolStripItem)mnuWhiteBackground).set_Name("mnuWhiteBackground");
			((ToolStripItem)mnuWhiteBackground).set_Size(new Size(367, 22));
			((ToolStripItem)mnuWhiteBackground).set_Text("Plot model on WHITE background (screen only)");
			((ToolStripItem)mnuWhiteBackground).add_Click((EventHandler)mnuWhiteBackground_Click);
			((ToolStripItem)mnuGrayBackground).set_Name("mnuGrayBackground");
			((ToolStripItem)mnuGrayBackground).set_Size(new Size(367, 22));
			((ToolStripItem)mnuGrayBackground).set_Text("Plot model on GRAY background (screen only)");
			((ToolStripItem)mnuGrayBackground).add_Click((EventHandler)mnuGrayBackground_Click);
			mnuBlackBackground.set_Checked(true);
			mnuBlackBackground.set_CheckState((CheckState)1);
			((ToolStripItem)mnuBlackBackground).set_Name("mnuBlackBackground");
			mnuBlackBackground.set_ShortcutKeys((Keys)131138);
			((ToolStripItem)mnuBlackBackground).set_Size(new Size(367, 22));
			((ToolStripItem)mnuBlackBackground).set_Text("Plot model on &BLACK background (screen only)");
			((ToolStripItem)mnuBlackBackground).add_Click((EventHandler)mnuBlackBackground_Click);
			((ToolStripItem)showAsteroidEllipseInColorToolStripMenuItem).set_Name("showAsteroidEllipseInColorToolStripMenuItem");
			((ToolStripItem)showAsteroidEllipseInColorToolStripMenuItem).set_Size(new Size(367, 22));
			((ToolStripItem)showAsteroidEllipseInColorToolStripMenuItem).set_Text("Show asteroid ELLIPSE in colour");
			((ToolStripItem)showAsteroidEllipseInColorToolStripMenuItem).add_Click((EventHandler)showAsteroidEllipseInColorToolStripMenuItem_Click);
			((ToolStripItem)mnuPathsInColour).set_Name("mnuPathsInColour");
			((ToolStripItem)mnuPathsInColour).set_Size(new Size(367, 22));
			((ToolStripItem)mnuPathsInColour).set_Text("Show STAR PATHS in colour");
			((ToolStripItem)mnuPathsInColour).add_Click((EventHandler)mnuPathsInColour_Click);
			((ToolStripItem)showEventInColorToolStripMenuItem).set_Name("showEventInColorToolStripMenuItem");
			((ToolStripItem)showEventInColorToolStripMenuItem).set_Size(new Size(367, 22));
			((ToolStripItem)showEventInColorToolStripMenuItem).set_Text("Show EVENTS in colour");
			((ToolStripItem)showEventInColorToolStripMenuItem).add_Click((EventHandler)showEventToolStripMenuItem_Click);
			showMISSEventsAsDashedLinesToolStripMenuItem.set_Checked(true);
			showMISSEventsAsDashedLinesToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)showMISSEventsAsDashedLinesToolStripMenuItem).set_Name("showMISSEventsAsDashedLinesToolStripMenuItem");
			((ToolStripItem)showMISSEventsAsDashedLinesToolStripMenuItem).set_Size(new Size(367, 22));
			((ToolStripItem)showMISSEventsAsDashedLinesToolStripMenuItem).set_Text("Show MISS events as dashed lines");
			((ToolStripItem)showMISSEventsAsDashedLinesToolStripMenuItem).add_Click((EventHandler)showMISSEventsAsDashedLinesToolStripMenuItem_Click);
			((ToolStripItem)mnuThickLines_Paths).set_Name("mnuThickLines_Paths");
			((ToolStripItem)mnuThickLines_Paths).set_Size(new Size(367, 22));
			((ToolStripItem)mnuThickLines_Paths).set_Text("use THICK lines for Paths");
			((ToolStripItem)mnuThickLines_Paths).add_Click((EventHandler)mnuThickLines_Click);
			((ToolStripItem)mnuThickLines_EllipseEvents).set_Name("mnuThickLines_EllipseEvents");
			((ToolStripItem)mnuThickLines_EllipseEvents).set_Size(new Size(367, 22));
			((ToolStripItem)mnuThickLines_EllipseEvents).set_Text("use THICK lines for Ellipse && Events");
			((ToolStripItem)mnuThickLines_EllipseEvents).add_Click((EventHandler)mnuThickLines_EllipseEvents_Click);
			((ToolStripItem)setTHICKLineWidthInUserSettingsGroup8ToolStripMenuItem).set_ForeColor(Color.Green);
			((ToolStripItem)setTHICKLineWidthInUserSettingsGroup8ToolStripMenuItem).set_Name("setTHICKLineWidthInUserSettingsGroup8ToolStripMenuItem");
			((ToolStripItem)setTHICKLineWidthInUserSettingsGroup8ToolStripMenuItem).set_Size(new Size(367, 22));
			((ToolStripItem)setTHICKLineWidthInUserSettingsGroup8ToolStripMenuItem).set_Text("... THICK line width - set in User Settings, Group 8");
			((ToolStripItem)mnuLargeLabels).set_Name("mnuLargeLabels");
			((ToolStripItem)mnuLargeLabels).set_Size(new Size(367, 22));
			((ToolStripItem)mnuLargeLabels).set_Text("use LARGE font for labels");
			((ToolStripItem)mnuLargeLabels).add_Click((EventHandler)mnuLargeLabels_Click);
			((ToolStripItem)lARGEFontSizeSetInUserSettingsGroup8ToolStripMenuItem).set_ForeColor(Color.Green);
			((ToolStripItem)lARGEFontSizeSetInUserSettingsGroup8ToolStripMenuItem).set_Name("lARGEFontSizeSetInUserSettingsGroup8ToolStripMenuItem");
			((ToolStripItem)lARGEFontSizeSetInUserSettingsGroup8ToolStripMenuItem).set_Size(new Size(367, 22));
			((ToolStripItem)lARGEFontSizeSetInUserSettingsGroup8ToolStripMenuItem).set_Text("... LARGE font size - set in User Settings, Group 8");
			((ToolStripItem)applyWatermark).set_Name("applyWatermark");
			((ToolStripItem)applyWatermark).set_Size(new Size(367, 22));
			((ToolStripItem)applyWatermark).set_Text("Apply Watermark");
			((ToolStripItem)applyWatermark).add_Click((EventHandler)applyWatermark_Click);
			((ToolStripItem)applyProvisionalMarkToolStripMenuItem).set_Name("applyProvisionalMarkToolStripMenuItem");
			((ToolStripItem)applyProvisionalMarkToolStripMenuItem).set_Size(new Size(367, 22));
			((ToolStripItem)applyProvisionalMarkToolStripMenuItem).set_Text("Apply 'Provisional' mark");
			((ToolStripItem)applyProvisionalMarkToolStripMenuItem).add_Click((EventHandler)applyProvisionalMarkToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator8).set_Name("toolStripSeparator8");
			((ToolStripItem)toolStripSeparator8).set_Size(new Size(376, 6));
			((ToolStripItem)shapeModelsToolStripMenuItem).set_BackColor(Color.Lavender);
			((ToolStripDropDownItem)shapeModelsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[12]
			{
				(ToolStripItem)addShapeImageFromDAMIT_ISAMToolStripMenuItem,
				(ToolStripItem)showShapeModelControlsOnPlotToolStripMenuItem,
				(ToolStripItem)toolStripSeparator17,
				(ToolStripItem)includeAsterodIDAndModelIDToolStripMenuItem,
				(ToolStripItem)showPolarAxesToolStripMenuItem,
				(ToolStripItem)showModelFacesToolStripMenuItem,
				(ToolStripItem)plotModelDarkToolStripMenuItem,
				(ToolStripItem)plotModelInGreyScaleToolStripMenuItem,
				(ToolStripItem)mnuShowMeanDiameterDAMIT,
				(ToolStripItem)showModelMeanDiameterLinesToolStripMenuItem,
				(ToolStripItem)toolStripSeparator16,
				(ToolStripItem)addImageFromClipboardToolStripMenuItem
			});
			((ToolStripItem)shapeModelsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)shapeModelsToolStripMenuItem).set_Font(new Font("Segoe UI", 9f));
			((ToolStripItem)shapeModelsToolStripMenuItem).set_ForeColor(Color.DarkBlue);
			((ToolStripItem)shapeModelsToolStripMenuItem).set_Name("shapeModelsToolStripMenuItem");
			((ToolStripItem)shapeModelsToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)shapeModelsToolStripMenuItem).set_Text("Shape Model appearance");
			addShapeImageFromDAMIT_ISAMToolStripMenuItem.set_CheckOnClick(true);
			((ToolStripItem)addShapeImageFromDAMIT_ISAMToolStripMenuItem).set_Name("addShapeImageFromDAMIT_ISAMToolStripMenuItem");
			addShapeImageFromDAMIT_ISAMToolStripMenuItem.set_ShortcutKeys((Keys)262221);
			((ToolStripItem)addShapeImageFromDAMIT_ISAMToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)addShapeImageFromDAMIT_ISAMToolStripMenuItem).set_Text("show Shape Models");
			((ToolStripItem)addShapeImageFromDAMIT_ISAMToolStripMenuItem).add_Click((EventHandler)addShapeImageFromDAMIT_ISAMToolStripMenuItem_Click);
			((ToolStripItem)showShapeModelControlsOnPlotToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Italic));
			((ToolStripItem)showShapeModelControlsOnPlotToolStripMenuItem).set_Name("showShapeModelControlsOnPlotToolStripMenuItem");
			showShapeModelControlsOnPlotToolStripMenuItem.set_ShortcutKeys((Keys)262211);
			((ToolStripItem)showShapeModelControlsOnPlotToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)showShapeModelControlsOnPlotToolStripMenuItem).set_Text("show Shape Model controls on plot");
			((ToolStripItem)showShapeModelControlsOnPlotToolStripMenuItem).add_Click((EventHandler)showShapeModelControlsOnPlotToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator17).set_Name("toolStripSeparator17");
			((ToolStripItem)toolStripSeparator17).set_Size(new Size(289, 6));
			((ToolStripItem)includeAsterodIDAndModelIDToolStripMenuItem).set_Name("includeAsterodIDAndModelIDToolStripMenuItem");
			((ToolStripItem)includeAsterodIDAndModelIDToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)includeAsterodIDAndModelIDToolStripMenuItem).set_Text("show Asteroid and Model Identifiers");
			((ToolStripItem)includeAsterodIDAndModelIDToolStripMenuItem).add_Click((EventHandler)includeAsterodIDAndModelIDToolStripMenuItem_Click);
			((ToolStripItem)showPolarAxesToolStripMenuItem).set_Name("showPolarAxesToolStripMenuItem");
			((ToolStripItem)showPolarAxesToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)showPolarAxesToolStripMenuItem).set_Text("show Polar Axes");
			((ToolStripItem)showPolarAxesToolStripMenuItem).add_Click((EventHandler)showPolarAxesToolStripMenuItem_Click);
			((ToolStripItem)showModelFacesToolStripMenuItem).set_Name("showModelFacesToolStripMenuItem");
			((ToolStripItem)showModelFacesToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)showModelFacesToolStripMenuItem).set_Text("show Model Faces");
			((ToolStripItem)showModelFacesToolStripMenuItem).add_Click((EventHandler)showModelFacesToolStripMenuItem_Click);
			((ToolStripItem)plotModelDarkToolStripMenuItem).set_Name("plotModelDarkToolStripMenuItem");
			((ToolStripItem)plotModelDarkToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)plotModelDarkToolStripMenuItem).set_Text("plot model Dark");
			((ToolStripItem)plotModelDarkToolStripMenuItem).add_Click((EventHandler)plotModelDarkToolStripMenuItem_Click);
			((ToolStripItem)plotModelInGreyScaleToolStripMenuItem).set_Name("plotModelInGreyScaleToolStripMenuItem");
			((ToolStripItem)plotModelInGreyScaleToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)plotModelInGreyScaleToolStripMenuItem).set_Text("plot model in Grey scale");
			((ToolStripItem)plotModelInGreyScaleToolStripMenuItem).add_Click((EventHandler)plotModelInGreyScaleToolStripMenuItem_Click);
			((ToolStripItem)mnuShowMeanDiameterDAMIT).set_Name("mnuShowMeanDiameterDAMIT");
			((ToolStripItem)mnuShowMeanDiameterDAMIT).set_Size(new Size(292, 22));
			((ToolStripItem)mnuShowMeanDiameterDAMIT).set_Text("show SCALED mean diameter (km)");
			((ToolStripItem)mnuShowMeanDiameterDAMIT).add_Click((EventHandler)showMeanDiameterDAMITToolStripMenuItem_Click);
			((ToolStripItem)showModelMeanDiameterLinesToolStripMenuItem).set_Name("showModelMeanDiameterLinesToolStripMenuItem");
			((ToolStripItem)showModelMeanDiameterLinesToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)showModelMeanDiameterLinesToolStripMenuItem).set_Text("show UNSCALED mean diameter");
			((ToolStripItem)showModelMeanDiameterLinesToolStripMenuItem).add_Click((EventHandler)showModelMeanDiameterLinesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator16).set_Name("toolStripSeparator16");
			((ToolStripItem)toolStripSeparator16).set_Size(new Size(289, 6));
			addImageFromClipboardToolStripMenuItem.set_CheckOnClick(true);
			((ToolStripItem)addImageFromClipboardToolStripMenuItem).set_Name("addImageFromClipboardToolStripMenuItem");
			((ToolStripItem)addImageFromClipboardToolStripMenuItem).set_Size(new Size(292, 22));
			((ToolStripItem)addImageFromClipboardToolStripMenuItem).set_Text("Add an image from the Clipboard");
			((ToolStripItem)addImageFromClipboardToolStripMenuItem).add_Click((EventHandler)addImageFromClipboardToolStripMenuItem_Click);
			((ToolStripItem)seeShapeModelDisplayFormForMorePlotOptionsToolStripMenuItem).set_BackColor(Color.Lavender);
			((ToolStripItem)seeShapeModelDisplayFormForMorePlotOptionsToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Italic));
			((ToolStripItem)seeShapeModelDisplayFormForMorePlotOptionsToolStripMenuItem).set_ForeColor(Color.DarkRed);
			((ToolStripItem)seeShapeModelDisplayFormForMorePlotOptionsToolStripMenuItem).set_Name("seeShapeModelDisplayFormForMorePlotOptionsToolStripMenuItem");
			((ToolStripItem)seeShapeModelDisplayFormForMorePlotOptionsToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)seeShapeModelDisplayFormForMorePlotOptionsToolStripMenuItem).set_Text(" ....for other Model options - see Shape Model display form ");
			((ToolStripItem)toolStripSeparator6).set_Name("toolStripSeparator6");
			((ToolStripItem)toolStripSeparator6).set_Size(new Size(376, 6));
			mnuShowEllipse.set_Checked(true);
			mnuShowEllipse.set_CheckState((CheckState)1);
			((ToolStripItem)mnuShowEllipse).set_Name("mnuShowEllipse");
			mnuShowEllipse.set_ShortcutKeys((Keys)131148);
			((ToolStripItem)mnuShowEllipse).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowEllipse).set_Text("Show best-fit E&LLIPSE");
			((ToolStripItem)mnuShowEllipse).add_Click((EventHandler)mnuShowEllipse_Click);
			((ToolStripItem)showFresnelDiffractionPeakToolStripMenuItem).set_Name("showFresnelDiffractionPeakToolStripMenuItem");
			((ToolStripItem)showFresnelDiffractionPeakToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)showFresnelDiffractionPeakToolStripMenuItem).set_Text("... show Fresnel diffraction peak");
			((ToolStripItem)showFresnelDiffractionPeakToolStripMenuItem).add_Click((EventHandler)showFresnelDiffractionPeakToolStripMenuItem_Click);
			((ToolStripItem)mnuShowEllipseInOutline).set_Name("mnuShowEllipseInOutline");
			((ToolStripItem)mnuShowEllipseInOutline).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowEllipseInOutline).set_Text("... show Ellipse in OUTLINE");
			((ToolStripItem)mnuShowEllipseInOutline).add_Click((EventHandler)mnuShowEllipseInOutline_Click);
			drawAsteroidProfileToolStripMenuItem.set_CheckOnClick(true);
			((ToolStripItem)drawAsteroidProfileToolStripMenuItem).set_Name("drawAsteroidProfileToolStripMenuItem");
			((ToolStripItem)drawAsteroidProfileToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)drawAsteroidProfileToolStripMenuItem).set_Text("Draw asteroid profile");
			drawAsteroidProfileToolStripMenuItem.add_CheckedChanged((EventHandler)drawAsteroidProfileToolStripMenuItem_CheckedChanged);
			((ToolStripItem)mnuShowAxes).set_Name("mnuShowAxes");
			mnuShowAxes.set_ShortcutKeys((Keys)131137);
			((ToolStripItem)mnuShowAxes).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowAxes).set_Text("Show &AXES of ellipse");
			((ToolStripItem)mnuShowAxes).add_Click((EventHandler)mnuShowAxes_Click);
			((ToolStripItem)showSatelliteOrbitsifAnyToolStripMenuItem).set_Name("showSatelliteOrbitsifAnyToolStripMenuItem");
			((ToolStripItem)showSatelliteOrbitsifAnyToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)showSatelliteOrbitsifAnyToolStripMenuItem).set_Text("Show satellite orbits (if any)");
			((ToolStripItem)showSatelliteOrbitsifAnyToolStripMenuItem).add_Click((EventHandler)showSatelliteOrbitsifAnyToolStripMenuItem_Click);
			((ToolStripItem)showASTROMETRYLocation).set_Name("showASTROMETRYLocation");
			((ToolStripItem)showASTROMETRYLocation).set_Size(new Size(379, 24));
			((ToolStripItem)showASTROMETRYLocation).set_Text("Show ASTROMETRY location");
			((ToolStripItem)showASTROMETRYLocation).add_Click((EventHandler)showASTROMETRYLocation_Click);
			mnuShowMarkers.set_Checked(true);
			mnuShowMarkers.set_CheckState((CheckState)1);
			((ToolStripItem)mnuShowMarkers).set_Name("mnuShowMarkers");
			mnuShowMarkers.set_ShortcutKeys((Keys)131149);
			((ToolStripItem)mnuShowMarkers).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowMarkers).set_Text("Show event MARKERS");
			((ToolStripItem)mnuShowMarkers).add_Click((EventHandler)mnuShowMarkers_Click);
			mnuShowID.set_Checked(true);
			mnuShowID.set_CheckState((CheckState)1);
			((ToolStripItem)mnuShowID).set_Name("mnuShowID");
			mnuShowID.set_ShortcutKeys((Keys)131150);
			((ToolStripItem)mnuShowID).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowID).set_Text("Show observer &NUMBERS");
			((ToolStripItem)mnuShowID).add_Click((EventHandler)mnuShowID_Click);
			mnuShowPredicted.set_Checked(true);
			mnuShowPredicted.set_CheckState((CheckState)1);
			((ToolStripItem)mnuShowPredicted).set_Name("mnuShowPredicted");
			mnuShowPredicted.set_ShortcutKeys((Keys)131154);
			((ToolStripItem)mnuShowPredicted).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowPredicted).set_Text("Show P&REDICTED centre path");
			((ToolStripItem)mnuShowPredicted).add_Click((EventHandler)mnuShowPredicted_Click);
			((ToolStripItem)mnuAlign).set_Name("mnuAlign");
			((ToolStripItem)mnuAlign).set_Size(new Size(379, 24));
			((ToolStripItem)mnuAlign).set_Text("ALIGN primary and secondary star events");
			((ToolStripItem)mnuAlign).add_Click((EventHandler)mnuAlign_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(376, 6));
			((ToolStripItem)mnuShowErrors).set_Name("mnuShowErrors");
			mnuShowErrors.set_ShortcutKeys((Keys)131141);
			((ToolStripItem)mnuShowErrors).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowErrors).set_Text("Show &ERROR bars");
			((ToolStripItem)mnuShowErrors).add_Click((EventHandler)mnuShowErrors_Click);
			((ToolStripItem)ErrorBarsForFittingToShapeModels_menu).set_Name("ErrorBarsForFittingToShapeModels_menu");
			((ToolStripItem)ErrorBarsForFittingToShapeModels_menu).set_Size(new Size(379, 24));
			((ToolStripItem)ErrorBarsForFittingToShapeModels_menu).set_Text(".... Set for fitting to shape models");
			((ToolStripItem)ErrorBarsForFittingToShapeModels_menu).add_Click((EventHandler)ErrorBarsForFittingToShapeModels_menu_Click);
			((ToolStripItem)mnuShowPathsOcculted).set_Name("mnuShowPathsOcculted");
			mnuShowPathsOcculted.set_ShortcutKeys((Keys)131151);
			((ToolStripItem)mnuShowPathsOcculted).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowPathsOcculted).set_Text("Show star paths - star &OCCULTED");
			((ToolStripItem)mnuShowPathsOcculted).add_Click((EventHandler)mnuShowPathsOcculted_Click);
			((ToolStripItem)mnuShowPathsVisible).set_Name("mnuShowPathsVisible");
			mnuShowPathsVisible.set_ShortcutKeys((Keys)131145);
			((ToolStripItem)mnuShowPathsVisible).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowPathsVisible).set_Text("Show star paths - star V&ISIBLE");
			((ToolStripItem)mnuShowPathsVisible).add_Click((EventHandler)mnuShowPathsVisible_Click);
			((ToolStripItem)showRINGEventPathsToolStripMenuItem).set_Name("showRINGEventPathsToolStripMenuItem");
			((ToolStripItem)showRINGEventPathsToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)showRINGEventPathsToolStripMenuItem).set_Text("Show RING event paths");
			((ToolStripItem)showRINGEventPathsToolStripMenuItem).add_Click((EventHandler)showRINGEventPathsToolStripMenuItem_Click);
			mnuIncludeTimeShift.set_Checked(true);
			mnuIncludeTimeShift.set_CheckState((CheckState)1);
			((ToolStripItem)mnuIncludeTimeShift).set_Name("mnuIncludeTimeShift");
			mnuIncludeTimeShift.set_ShortcutKeys((Keys)131156);
			((ToolStripItem)mnuIncludeTimeShift).set_Size(new Size(379, 24));
			((ToolStripItem)mnuIncludeTimeShift).set_Text("Include &TIME-SHIFT corrections");
			((ToolStripItem)mnuIncludeTimeShift).add_Click((EventHandler)mnuIncludeTimeShift_Click);
			((ToolStripItem)toolStripSeparator4).set_Name("toolStripSeparator4");
			((ToolStripItem)toolStripSeparator4).set_Size(new Size(376, 6));
			((ToolStripItem)mnuShowPathsMiss).set_Name("mnuShowPathsMiss");
			mnuShowPathsMiss.set_ShortcutKeys((Keys)196685);
			((ToolStripItem)mnuShowPathsMiss).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowPathsMiss).set_Text("Show star paths - NO DETECTION events");
			((ToolStripItem)mnuShowPathsMiss).add_Click((EventHandler)mnuShowPathsMiss_Click);
			((ToolStripItem)mnuShowVideo).set_Name("mnuShowVideo");
			((ToolStripItem)mnuShowVideo).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowVideo).set_Text("Show only &VIDEO/photoelectric ");
			((ToolStripItem)mnuShowVideo).add_Click((EventHandler)mnuShowVideo_Click);
			mnuShowZeroWeighted.set_Checked(true);
			mnuShowZeroWeighted.set_CheckState((CheckState)1);
			((ToolStripItem)mnuShowZeroWeighted).set_Name("mnuShowZeroWeighted");
			mnuShowZeroWeighted.set_ShortcutKeys((Keys)131162);
			((ToolStripItem)mnuShowZeroWeighted).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowZeroWeighted).set_Text("Show &ZERO-WEIGHTED observations");
			((ToolStripItem)mnuShowZeroWeighted).add_Click((EventHandler)mnuShowZeroWeighted_Click);
			((ToolStripItem)mnuShowCloud).set_Name("mnuShowCloud");
			((ToolStripItem)mnuShowCloud).set_Size(new Size(379, 24));
			((ToolStripItem)mnuShowCloud).set_Text("Show path - star not seen (eg CLOUD)");
			((ToolStripItem)mnuShowCloud).add_Click((EventHandler)mnuShowCloud_Click);
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(376, 6));
			((ToolStripItem)toolStripSeparator10).set_Name("toolStripSeparator10");
			((ToolStripItem)toolStripSeparator10).set_Size(new Size(376, 6));
			((ToolStripItem)setPlotOptionsForPublicationPlotToolStripMenuItem).set_BackColor(Color.DarkTurquoise);
			((ToolStripItem)setPlotOptionsForPublicationPlotToolStripMenuItem).set_Font(new Font("Segoe UI", 11f, FontStyle.Bold));
			((ToolStripItem)setPlotOptionsForPublicationPlotToolStripMenuItem).set_ForeColor(Color.DarkRed);
			((ToolStripItem)setPlotOptionsForPublicationPlotToolStripMenuItem).set_Name("setPlotOptionsForPublicationPlotToolStripMenuItem");
			((ToolStripItem)setPlotOptionsForPublicationPlotToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)setPlotOptionsForPublicationPlotToolStripMenuItem).set_Text("apply settings for a Publication plot");
			((ToolStripItem)setPlotOptionsForPublicationPlotToolStripMenuItem).add_Click((EventHandler)setPlotOptionsForPublicationPlotToolStripMenuItem_Click);
			((ToolStripItem)applyDEFAULTSettingsToolStripMenuItem).set_BackColor(Color.Cornsilk);
			((ToolStripItem)applyDEFAULTSettingsToolStripMenuItem).set_Font(new Font("Segoe UI", 11f, FontStyle.Bold));
			((ToolStripItem)applyDEFAULTSettingsToolStripMenuItem).set_ForeColor(Color.DarkGreen);
			((ToolStripItem)applyDEFAULTSettingsToolStripMenuItem).set_Name("applyDEFAULTSettingsToolStripMenuItem");
			((ToolStripItem)applyDEFAULTSettingsToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)applyDEFAULTSettingsToolStripMenuItem).set_Text("apply your DEFAULT settings");
			((ToolStripItem)applyDEFAULTSettingsToolStripMenuItem).add_Click((EventHandler)applyDEFAULTSettingsToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator12).set_Name("toolStripSeparator12");
			((ToolStripItem)toolStripSeparator12).set_Size(new Size(376, 6));
			((ToolStripItem)saveCurrentSettingsAsDefaultToolStripMenuItem).set_BackColor(Color.Beige);
			((ToolStripItem)saveCurrentSettingsAsDefaultToolStripMenuItem).set_Font(new Font("Segoe UI", 10f, FontStyle.Bold));
			((ToolStripItem)saveCurrentSettingsAsDefaultToolStripMenuItem).set_ForeColor(Color.MediumBlue);
			((ToolStripItem)saveCurrentSettingsAsDefaultToolStripMenuItem).set_Name("saveCurrentSettingsAsDefaultToolStripMenuItem");
			((ToolStripItem)saveCurrentSettingsAsDefaultToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)saveCurrentSettingsAsDefaultToolStripMenuItem).set_Text("save current settings as your DEFAULT values");
			((ToolStripItem)saveCurrentSettingsAsDefaultToolStripMenuItem).add_Click((EventHandler)saveCurrentSettingsAsDefaultToolStripMenuItem_Click);
			((ToolStripItem)restoreOccultDefaultValuesToolStripMenuItem).set_BackColor(Color.Beige);
			((ToolStripItem)restoreOccultDefaultValuesToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold | FontStyle.Italic));
			((ToolStripItem)restoreOccultDefaultValuesToolStripMenuItem).set_ForeColor(Color.Firebrick);
			((ToolStripItem)restoreOccultDefaultValuesToolStripMenuItem).set_Name("restoreOccultDefaultValuesToolStripMenuItem");
			((ToolStripItem)restoreOccultDefaultValuesToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)restoreOccultDefaultValuesToolStripMenuItem).set_Text("Replace your Default values with the Occult defaults");
			((ToolStripItem)restoreOccultDefaultValuesToolStripMenuItem).add_Click((EventHandler)restoreOccultDefaultValuesToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator9).set_Name("toolStripSeparator9");
			((ToolStripItem)toolStripSeparator9).set_Size(new Size(376, 6));
			((ToolStripItem)toolStripSeparator11).set_Name("toolStripSeparator11");
			((ToolStripItem)toolStripSeparator11).set_Size(new Size(376, 6));
			showStarDiameterAnalyserToolStripMenuItem.set_CheckOnClick(true);
			((ToolStripItem)showStarDiameterAnalyserToolStripMenuItem).set_Font(new Font("Segoe UI", 11f));
			((ToolStripItem)showStarDiameterAnalyserToolStripMenuItem).set_Name("showStarDiameterAnalyserToolStripMenuItem");
			((ToolStripItem)showStarDiameterAnalyserToolStripMenuItem).set_Size(new Size(379, 24));
			((ToolStripItem)showStarDiameterAnalyserToolStripMenuItem).set_Text("Show star diameter analyser");
			((ToolStripItem)showStarDiameterAnalyserToolStripMenuItem).add_Click((EventHandler)showStarDiameterAnalyserToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.Help16x16);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)keepOnTopToolStripMenuItem).set_Name("keepOnTopToolStripMenuItem");
			keepOnTopToolStripMenuItem.set_ShortcutKeys((Keys)131147);
			keepOnTopToolStripMenuItem.set_ShowShortcutKeys(false);
			((ToolStripItem)keepOnTopToolStripMenuItem).set_Size(new Size(112, 20));
			((ToolStripItem)keepOnTopToolStripMenuItem).set_Text("&Keep form on top");
			((ToolStripItem)keepOnTopToolStripMenuItem).add_Click((EventHandler)keepOnTopToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(38, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)grpBestFit).get_Controls().Add((Control)(object)label63);
			((Control)grpBestFit).get_Controls().Add((Control)(object)pnlReview);
			((Control)grpBestFit).get_Controls().Add((Control)(object)lblDiaEquiv);
			((Control)grpBestFit).get_Controls().Add((Control)(object)panelLimbFit);
			((Control)grpBestFit).get_Controls().Add((Control)(object)panel5);
			((Control)grpBestFit).get_Controls().Add((Control)(object)label45);
			((Control)grpBestFit).get_Controls().Add((Control)(object)label44);
			((Control)grpBestFit).get_Controls().Add((Control)(object)updnCenterOfMass_Y);
			((Control)grpBestFit).get_Controls().Add((Control)(object)updnCenterOfMass_X);
			((Control)grpBestFit).get_Controls().Add((Control)(object)cmdSet3Checks);
			((Control)grpBestFit).get_Controls().Add((Control)(object)chkShapeModelCentered);
			((Control)grpBestFit).get_Controls().Add((Control)(object)pnlDouble);
			((Control)grpBestFit).get_Controls().Add((Control)(object)lblMotion);
			((Control)grpBestFit).get_Controls().Add((Control)(object)PanelLegend);
			((Control)grpBestFit).get_Controls().Add((Control)(object)lblAxisRatio);
			((Control)grpBestFit).get_Controls().Add((Control)(object)cmdFit);
			((Control)grpBestFit).get_Controls().Add((Control)(object)lblRMS);
			((Control)grpBestFit).get_Controls().Add((Control)(object)lstObservers);
			((Control)grpBestFit).get_Controls().Add((Control)(object)chkCircle);
			((Control)grpBestFit).get_Controls().Add((Control)(object)chkUseAssumedDiameter);
			((Control)grpBestFit).get_Controls().Add((Control)(object)chkY);
			((Control)grpBestFit).get_Controls().Add((Control)(object)chkMiss);
			((Control)grpBestFit).get_Controls().Add((Control)(object)chkA);
			((Control)grpBestFit).get_Controls().Add((Control)(object)chkB);
			((Control)grpBestFit).get_Controls().Add((Control)(object)chkPA);
			((Control)grpBestFit).get_Controls().Add((Control)(object)chkX);
			((Control)grpBestFit).get_Controls().Add((Control)(object)label9);
			((Control)grpBestFit).get_Controls().Add((Control)(object)cmbQuality);
			((Control)grpBestFit).get_Controls().Add((Control)(object)label8);
			((Control)grpBestFit).get_Controls().Add((Control)(object)label7);
			((Control)grpBestFit).get_Controls().Add((Control)(object)label6);
			((Control)grpBestFit).get_Controls().Add((Control)(object)label5);
			((Control)grpBestFit).get_Controls().Add((Control)(object)label1);
			((Control)grpBestFit).get_Controls().Add((Control)(object)updnY);
			((Control)grpBestFit).get_Controls().Add((Control)(object)updnA);
			((Control)grpBestFit).get_Controls().Add((Control)(object)updnB);
			((Control)grpBestFit).get_Controls().Add((Control)(object)updnPA);
			((Control)grpBestFit).get_Controls().Add((Control)(object)updnX);
			((Control)grpBestFit).set_Location(new Point(453, 27));
			((Control)grpBestFit).set_Name("grpBestFit");
			((Control)grpBestFit).set_Size(new Size(344, 461));
			((Control)grpBestFit).set_TabIndex(2);
			grpBestFit.set_TabStop(false);
			((Control)grpBestFit).set_Text("Find best fit");
			((Control)label63).set_AutoSize(true);
			((Control)label63).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold));
			((Control)label63).set_ForeColor(Color.Maroon);
			((Control)label63).set_Location(new Point(34, 213));
			((Control)label63).set_Name("label63");
			((Control)label63).set_Size(new Size(80, 13));
			((Control)label63).set_TabIndex(66);
			((Control)label63).set_Text("Review flags");
			((Control)pnlReview).set_BackColor(Color.Honeydew);
			pnlReview.set_BorderStyle((BorderStyle)2);
			((Control)pnlReview).set_Location(new Point(117, 209));
			((Control)pnlReview).set_Name("pnlReview");
			((Control)pnlReview).set_Size(new Size(217, 91));
			((Control)pnlReview).set_TabIndex(65);
			((Control)pnlReview).add_Leave((EventHandler)pnlReview_Leave);
			((Control)lblDiaEquiv).set_AutoSize(true);
			((Control)lblDiaEquiv).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDiaEquiv).set_ForeColor(Color.DarkGreen);
			((Control)lblDiaEquiv).set_Location(new Point(252, 67));
			((Control)lblDiaEquiv).set_Name("lblDiaEquiv");
			((Control)lblDiaEquiv).set_Size(new Size(64, 13));
			((Control)lblDiaEquiv).set_TabIndex(63);
			((Control)lblDiaEquiv).set_Text("dia = 0.0 km");
			((Control)panelLimbFit).get_Controls().Add((Control)(object)trackLimbFit);
			((Control)panelLimbFit).get_Controls().Add((Control)(object)lblLimbFit);
			((Control)panelLimbFit).set_Location(new Point(191, 315));
			((Control)panelLimbFit).set_Name("panelLimbFit");
			((Control)panelLimbFit).set_Size(new Size(145, 25));
			((Control)panelLimbFit).set_TabIndex(62);
			((Control)panelLimbFit).set_Visible(false);
			((Control)trackLimbFit).set_AutoSize(false);
			((Control)trackLimbFit).set_BackColor(Color.LemonChiffon);
			((Control)trackLimbFit).set_Location(new Point(44, 2));
			trackLimbFit.set_Maximum(200);
			((Control)trackLimbFit).set_Name("trackLimbFit");
			((Control)trackLimbFit).set_Size(new Size(96, 21));
			trackLimbFit.set_SmallChange(5);
			((Control)trackLimbFit).set_TabIndex(38);
			trackLimbFit.set_TickFrequency(20);
			trackLimbFit.set_Value(20);
			trackLimbFit.add_ValueChanged((EventHandler)trackLimbFit_ValueChanged);
			((Control)lblLimbFit).set_AutoSize(true);
			((Control)lblLimbFit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblLimbFit).set_Location(new Point(4, 6));
			((Control)lblLimbFit).set_Name("lblLimbFit");
			((Control)lblLimbFit).set_Size(new Size(40, 13));
			((Control)lblLimbFit).set_TabIndex(39);
			((Control)lblLimbFit).set_Text("Limb fit");
			((Control)panel5).set_BackColor(Color.Azure);
			panel5.set_BorderStyle((BorderStyle)2);
			((Control)panel5).get_Controls().Add((Control)(object)chkScoll);
			((Control)panel5).get_Controls().Add((Control)(object)label16);
			((Control)panel5).get_Controls().Add((Control)(object)label15);
			((Control)panel5).get_Controls().Add((Control)(object)trackOpacity);
			((Control)panel5).get_Controls().Add((Control)(object)label18);
			((Control)panel5).get_Controls().Add((Control)(object)panel4);
			((Control)panel5).get_Controls().Add((Control)(object)label10);
			((Control)panel5).get_Controls().Add((Control)(object)SliderScale);
			((Control)panel5).set_Location(new Point(2, 231));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(340, 83));
			((Control)panel5).set_TabIndex(61);
			((Control)chkScoll).set_AutoSize(true);
			chkScoll.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkScoll).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkScoll).set_ForeColor(Color.Maroon);
			((Control)chkScoll).set_Location(new Point(23, 61));
			((Control)chkScoll).set_Name("chkScoll");
			((Control)chkScoll).set_Size(new Size(129, 17));
			((Control)chkScoll).set_TabIndex(61);
			((Control)chkScoll).set_Text("Scroll range x1.25");
			((ButtonBase)chkScoll).set_UseVisualStyleBackColor(true);
			chkScoll.add_CheckedChanged((EventHandler)chkScoll_CheckedChanged);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0, gdiVerticalFont: true));
			((Control)label16).set_ForeColor(Color.FromArgb(100, 0, 50));
			((Control)label16).set_Location(new Point(3, 14));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(16, 52));
			((Control)label16).set_TabIndex(60);
			((Control)label16).set_Text("P\r\nL\r\nO\r\nT");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_ForeColor(Color.FromArgb(100, 0, 50));
			((Control)label15).set_Location(new Point(23, 36));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(31, 13));
			((Control)label15).set_TabIndex(3);
			((Control)label15).set_Text("Size");
			((Control)trackOpacity).set_AutoSize(false);
			((Control)trackOpacity).set_BackColor(Color.LemonChiffon);
			((Control)trackOpacity).set_Location(new Point(220, 55));
			trackOpacity.set_Maximum(100);
			trackOpacity.set_Minimum(50);
			((Control)trackOpacity).set_Name("trackOpacity");
			((Control)trackOpacity).set_Size(new Size(115, 21));
			trackOpacity.set_SmallChange(5);
			((Control)trackOpacity).set_TabIndex(38);
			trackOpacity.set_TickFrequency(10);
			trackOpacity.set_Value(100);
			trackOpacity.add_ValueChanged((EventHandler)trackOpacity_ValueChanged);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_ForeColor(Color.Purple);
			((Control)label18).set_Location(new Point(238, 37));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(79, 13));
			((Control)label18).set_TabIndex(39);
			((Control)label18).set_Text("Form opacity");
			((Control)panel4).set_BackColor(Color.Honeydew);
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)optPlotx5);
			((Control)panel4).get_Controls().Add((Control)(object)optyPlotNormal);
			((Control)panel4).get_Controls().Add((Control)(object)optPlotx2);
			((Control)panel4).set_Location(new Point(56, 32));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(164, 24));
			((Control)panel4).set_TabIndex(50);
			((Control)optPlotx5).set_AutoSize(true);
			((Control)optPlotx5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optPlotx5).set_ForeColor(Color.FromArgb(100, 0, 50));
			((Control)optPlotx5).set_Location(new Point(116, 1));
			((Control)optPlotx5).set_Name("optPlotx5");
			((Control)optPlotx5).set_Size(new Size(42, 17));
			((Control)optPlotx5).set_TabIndex(1);
			((Control)optPlotx5).set_Text("x 5");
			((ButtonBase)optPlotx5).set_UseVisualStyleBackColor(true);
			((Control)optyPlotNormal).set_AutoSize(true);
			optyPlotNormal.set_Checked(true);
			((Control)optyPlotNormal).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optyPlotNormal).set_ForeColor(Color.FromArgb(100, 0, 50));
			((Control)optyPlotNormal).set_Location(new Point(8, 1));
			((Control)optyPlotNormal).set_Name("optyPlotNormal");
			((Control)optyPlotNormal).set_Size(new Size(62, 17));
			((Control)optyPlotNormal).set_TabIndex(2);
			optyPlotNormal.set_TabStop(true);
			((Control)optyPlotNormal).set_Text("normal");
			((ButtonBase)optyPlotNormal).set_UseVisualStyleBackColor(true);
			optyPlotNormal.add_CheckedChanged((EventHandler)optyPlotNormal_CheckedChanged);
			((Control)optPlotx2).set_AutoSize(true);
			((Control)optPlotx2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optPlotx2).set_ForeColor(Color.FromArgb(100, 0, 50));
			((Control)optPlotx2).set_Location(new Point(72, 1));
			((Control)optPlotx2).set_Name("optPlotx2");
			((Control)optPlotx2).set_Size(new Size(42, 17));
			((Control)optPlotx2).set_TabIndex(0);
			((Control)optPlotx2).set_Text("x 2");
			((ButtonBase)optPlotx2).set_UseVisualStyleBackColor(true);
			optPlotx2.add_CheckedChanged((EventHandler)optPlotx2_CheckedChanged);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_ForeColor(Color.FromArgb(100, 0, 50));
			((Control)label10).set_Location(new Point(23, 10));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(39, 13));
			((Control)label10).set_TabIndex(28);
			((Control)label10).set_Text("Scale");
			((Control)SliderScale).set_AutoSize(false);
			((Control)SliderScale).set_BackColor(Color.LemonChiffon);
			SliderScale.set_LargeChange(1);
			((Control)SliderScale).set_Location(new Point(65, 7));
			SliderScale.set_Maximum(500);
			SliderScale.set_Minimum(-500);
			((Control)SliderScale).set_Name("SliderScale");
			((Control)SliderScale).set_Size(new Size(269, 19));
			((Control)SliderScale).set_TabIndex(27);
			SliderScale.set_TickFrequency(50);
			SliderScale.add_Scroll((EventHandler)SliderScale_Scroll);
			SliderScale.add_ValueChanged((EventHandler)SliderScale_ValueChanged);
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(237, 37));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(42, 13));
			((Control)label45).set_TabIndex(57);
			((Control)label45).set_Text("Mass Y");
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label44).set_Location(new Point(237, 15));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(42, 13));
			((Control)label44).set_TabIndex(56);
			((Control)label44).set_Text("Mass X");
			updnCenterOfMass_Y.set_DecimalPlaces(1);
			updnCenterOfMass_Y.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnCenterOfMass_Y).set_Location(new Point(281, 33));
			updnCenterOfMass_Y.set_Maximum(new decimal(new int[4] { 20000, 0, 0, 0 }));
			updnCenterOfMass_Y.set_Minimum(new decimal(new int[4] { 1000, 0, 0, -2147483648 }));
			((Control)updnCenterOfMass_Y).set_Name("updnCenterOfMass_Y");
			((Control)updnCenterOfMass_Y).set_Size(new Size(58, 20));
			((Control)updnCenterOfMass_Y).set_TabIndex(55);
			updnCenterOfMass_Y.add_ValueChanged((EventHandler)updnCenterOfMass_Y_ValueChanged);
			updnCenterOfMass_X.set_DecimalPlaces(1);
			updnCenterOfMass_X.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnCenterOfMass_X).set_Location(new Point(281, 11));
			updnCenterOfMass_X.set_Maximum(new decimal(new int[4] { 20000, 0, 0, 0 }));
			updnCenterOfMass_X.set_Minimum(new decimal(new int[4] { 1000, 0, 0, -2147483648 }));
			((Control)updnCenterOfMass_X).set_Name("updnCenterOfMass_X");
			((Control)updnCenterOfMass_X).set_Size(new Size(58, 20));
			((Control)updnCenterOfMass_X).set_TabIndex(54);
			updnCenterOfMass_X.add_ValueChanged((EventHandler)updnCenterOfMass_X_ValueChanged);
			((Control)cmdSet3Checks).set_BackColor(Color.Red);
			((ButtonBase)cmdSet3Checks).get_FlatAppearance().set_BorderSize(0);
			((Control)cmdSet3Checks).set_Location(new Point(175, 122));
			((Control)cmdSet3Checks).set_Name("cmdSet3Checks");
			((Control)cmdSet3Checks).set_Size(new Size(21, 10));
			((Control)cmdSet3Checks).set_TabIndex(53);
			((ButtonBase)cmdSet3Checks).set_UseVisualStyleBackColor(false);
			((Control)cmdSet3Checks).add_Click((EventHandler)cmdSet3Checks_Click);
			((Control)cmdSet3Checks).add_MouseClick(new MouseEventHandler(cmdSet3Checks_MouseClick));
			((Control)chkShapeModelCentered).set_AutoSize(true);
			chkShapeModelCentered.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkShapeModelCentered).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkShapeModelCentered).set_Location(new Point(270, 53));
			((Control)chkShapeModelCentered).set_Name("chkShapeModelCentered");
			((Control)chkShapeModelCentered).set_Size(new Size(68, 14));
			((Control)chkShapeModelCentered).set_TabIndex(49);
			((Control)chkShapeModelCentered).set_Text("Shape model");
			((ButtonBase)chkShapeModelCentered).set_UseVisualStyleBackColor(true);
			chkShapeModelCentered.add_CheckedChanged((EventHandler)chkShapeModelCentered_CheckedChanged);
			((Control)chkShapeModelCentered).add_MouseClick(new MouseEventHandler(chkShapeModelCentered_MouseClick));
			pnlDouble.set_BorderStyle((BorderStyle)2);
			((Control)pnlDouble).get_Controls().Add((Control)(object)optSecondary);
			((Control)pnlDouble).get_Controls().Add((Control)(object)optPrimary);
			((Control)pnlDouble).get_Controls().Add((Control)(object)label46);
			((Control)pnlDouble).get_Controls().Add((Control)(object)optBoth);
			((Control)pnlDouble).set_Enabled(false);
			((Control)pnlDouble).set_Location(new Point(11, 160));
			((Control)pnlDouble).set_Name("pnlDouble");
			((Control)pnlDouble).set_Size(new Size(323, 22));
			((Control)pnlDouble).set_TabIndex(31);
			((Control)optSecondary).set_AutoSize(true);
			((Control)optSecondary).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSecondary).set_ForeColor(Color.DarkBlue);
			((Control)optSecondary).set_Location(new Point(249, 2));
			((Control)optSecondary).set_Name("optSecondary");
			((Control)optSecondary).set_Size(new Size(67, 16));
			((Control)optSecondary).set_TabIndex(2);
			((Control)optSecondary).set_Text("Secondary");
			((ButtonBase)optSecondary).set_UseVisualStyleBackColor(true);
			optSecondary.add_CheckedChanged((EventHandler)optSecondary_CheckedChanged);
			((Control)optPrimary).set_AutoSize(true);
			((Control)optPrimary).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optPrimary).set_ForeColor(Color.DarkBlue);
			((Control)optPrimary).set_Location(new Point(184, 2));
			((Control)optPrimary).set_Name("optPrimary");
			((Control)optPrimary).set_Size(new Size(55, 16));
			((Control)optPrimary).set_TabIndex(1);
			((Control)optPrimary).set_Text("Primary");
			((ButtonBase)optPrimary).set_UseVisualStyleBackColor(true);
			optPrimary.add_CheckedChanged((EventHandler)optPrimary_CheckedChanged);
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label46).set_ForeColor(Color.DarkBlue);
			((Control)label46).set_Location(new Point(3, 2));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(119, 13));
			((Control)label46).set_TabIndex(58);
			((Control)label46).set_Text("Double stars - show");
			((Control)optBoth).set_AutoSize(true);
			optBoth.set_Checked(true);
			((Control)optBoth).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optBoth).set_ForeColor(Color.DarkBlue);
			((Control)optBoth).set_Location(new Point(132, 2));
			((Control)optBoth).set_Name("optBoth");
			((Control)optBoth).set_Size(new Size(42, 16));
			((Control)optBoth).set_TabIndex(0);
			optBoth.set_TabStop(true);
			((Control)optBoth).set_Text("Both");
			((ButtonBase)optBoth).set_UseVisualStyleBackColor(true);
			optBoth.add_CheckedChanged((EventHandler)optBoth_CheckedChanged);
			((Control)lblMotion).set_AutoSize(true);
			((Control)lblMotion).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMotion).set_Location(new Point(237, 111));
			((Control)lblMotion).set_Name("lblMotion");
			((Control)lblMotion).set_Size(new Size(90, 13));
			((Control)lblMotion).set_TabIndex(46);
			((Control)lblMotion).set_Text("Motion: 3.00 km/s");
			((ScrollableControl)PanelLegend).set_AutoScroll(true);
			((ScrollableControl)PanelLegend).set_AutoScrollMargin(new Size(10, 10));
			((ScrollableControl)PanelLegend).set_AutoScrollMinSize(new Size(10, 100));
			((Control)PanelLegend).set_BackColor(Color.White);
			PanelLegend.set_BorderStyle((BorderStyle)2);
			((Control)PanelLegend).get_Controls().Add((Control)(object)picLegend);
			((Control)PanelLegend).set_Location(new Point(16, 337));
			((Control)PanelLegend).set_Name("PanelLegend");
			((Control)PanelLegend).set_Size(new Size(328, 110));
			((Control)PanelLegend).set_TabIndex(37);
			((Control)picLegend).set_BackColor(Color.White);
			((Control)picLegend).set_Location(new Point(3, 8));
			((Control)picLegend).set_Name("picLegend");
			((Control)picLegend).set_Size(new Size(328, 90));
			picLegend.set_TabIndex(9);
			picLegend.set_TabStop(false);
			toolTip.SetToolTip((Control)(object)picLegend, "Right-click on observer to\r\nhighlight their chord");
			((Control)picLegend).add_MouseDown(new MouseEventHandler(picLegend_MouseDown));
			((Control)lblAxisRatio).set_AutoSize(true);
			((Control)lblAxisRatio).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAxisRatio).set_ForeColor(Color.Blue);
			((Control)lblAxisRatio).set_Location(new Point(233, 81));
			((Control)lblAxisRatio).set_Name("lblAxisRatio");
			((Control)lblAxisRatio).set_Size(new Size(109, 13));
			((Control)lblAxisRatio).set_TabIndex(33);
			((Control)lblAxisRatio).set_Text("a/b: 1.00, dMag: 0.00");
			lblAxisRatio.set_TextAlign(ContentAlignment.MiddleLeft);
			((Control)cmdFit).set_BackgroundImage((Image)Resources.Graph05Gray);
			((Control)cmdFit).set_BackgroundImageLayout((ImageLayout)3);
			((ButtonBase)cmdFit).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)cmdFit).set_FlatStyle((FlatStyle)1);
			((Control)cmdFit).set_Location(new Point(9, 18));
			((Control)cmdFit).set_Name("cmdFit");
			((Control)cmdFit).set_Size(new Size(32, 32));
			((Control)cmdFit).set_TabIndex(29);
			((ButtonBase)cmdFit).set_UseVisualStyleBackColor(true);
			((Control)cmdFit).add_Click((EventHandler)cmdFit_Click);
			((Control)lblRMS).set_AutoSize(true);
			((Control)lblRMS).set_Location(new Point(13, 324));
			((Control)lblRMS).set_Name("lblRMS");
			((Control)lblRMS).set_Size(new Size(41, 13));
			((Control)lblRMS).set_TabIndex(32);
			((Control)lblRMS).set_Text("label11");
			((Control)lstObservers).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstObservers).set_FormattingEnabled(true);
			lstObservers.set_HorizontalExtent(350);
			lstObservers.set_HorizontalScrollbar(true);
			lstObservers.set_ItemHeight(14);
			((Control)lstObservers).set_Location(new Point(8, 344));
			((Control)lstObservers).set_Name("lstObservers");
			((Control)lstObservers).set_Size(new Size(328, 102));
			((Control)lstObservers).set_TabIndex(26);
			chkCircle.set_AutoCheck(false);
			((Control)chkCircle).set_AutoSize(true);
			((Control)chkCircle).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkCircle).set_Location(new Point(13, 133));
			((Control)chkCircle).set_Name("chkCircle");
			((Control)chkCircle).set_Size(new Size(61, 17));
			((Control)chkCircle).set_TabIndex(16);
			((Control)chkCircle).set_Text("Circular");
			((ButtonBase)chkCircle).set_UseVisualStyleBackColor(true);
			chkCircle.add_CheckedChanged((EventHandler)chkCircle_CheckedChanged);
			((Control)chkCircle).add_MouseClick(new MouseEventHandler(chkCircle_MouseClick));
			chkUseAssumedDiameter.set_AutoCheck(false);
			((Control)chkUseAssumedDiameter).set_AutoSize(true);
			((Control)chkUseAssumedDiameter).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkUseAssumedDiameter).set_Location(new Point(79, 126));
			((Control)chkUseAssumedDiameter).set_Name("chkUseAssumedDiameter");
			((Control)chkUseAssumedDiameter).set_Size(new Size(89, 30));
			((Control)chkUseAssumedDiameter).set_TabIndex(48);
			((Control)chkUseAssumedDiameter).set_Text("Use assumed\r\ndia (0.0 km)");
			((ButtonBase)chkUseAssumedDiameter).set_UseVisualStyleBackColor(true);
			chkUseAssumedDiameter.add_CheckedChanged((EventHandler)chkUseAssumedDiameter_CheckedChanged);
			((Control)chkUseAssumedDiameter).add_MouseClick(new MouseEventHandler(chkUseAssumedDiameter_MouseClick));
			((Control)chkY).set_AutoSize(true);
			chkY.set_Checked(true);
			chkY.set_CheckState((CheckState)1);
			((Control)chkY).set_Location(new Point(179, 35));
			((Control)chkY).set_Name("chkY");
			((Control)chkY).set_Size(new Size(29, 17));
			((Control)chkY).set_TabIndex(25);
			((Control)chkY).set_Text(".");
			((ButtonBase)chkY).set_UseVisualStyleBackColor(true);
			chkY.add_CheckedChanged((EventHandler)chkY_CheckedChanged);
			((Control)chkMiss).set_AutoSize(true);
			((Control)chkMiss).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkMiss).set_Location(new Point(181, 133));
			((Control)chkMiss).set_Name("chkMiss");
			((Control)chkMiss).set_Size(new Size(120, 17));
			((Control)chkMiss).set_TabIndex(15);
			((Control)chkMiss).set_Text("Include Miss events");
			((ButtonBase)chkMiss).set_UseVisualStyleBackColor(true);
			((Control)chkMiss).add_MouseClick(new MouseEventHandler(chkMiss_MouseClick));
			((Control)chkA).set_AutoSize(true);
			((Control)chkA).set_Location(new Point(179, 62));
			((Control)chkA).set_Name("chkA");
			((Control)chkA).set_Size(new Size(29, 17));
			((Control)chkA).set_TabIndex(24);
			((Control)chkA).set_Text(".");
			((ButtonBase)chkA).set_UseVisualStyleBackColor(true);
			chkA.add_CheckedChanged((EventHandler)chkA_CheckedChanged);
			((Control)chkB).set_AutoSize(true);
			((Control)chkB).set_Location(new Point(179, 84));
			((Control)chkB).set_Name("chkB");
			((Control)chkB).set_Size(new Size(29, 17));
			((Control)chkB).set_TabIndex(23);
			((Control)chkB).set_Text(".");
			((ButtonBase)chkB).set_UseVisualStyleBackColor(true);
			chkB.add_CheckedChanged((EventHandler)chkB_CheckedChanged);
			((Control)chkPA).set_AutoSize(true);
			((Control)chkPA).set_Location(new Point(179, 106));
			((Control)chkPA).set_Name("chkPA");
			((Control)chkPA).set_Size(new Size(29, 17));
			((Control)chkPA).set_TabIndex(22);
			((Control)chkPA).set_Text(".");
			((ButtonBase)chkPA).set_UseVisualStyleBackColor(true);
			chkPA.add_CheckedChanged((EventHandler)chkPA_CheckedChanged);
			((Control)chkX).set_AutoSize(true);
			chkX.set_Checked(true);
			chkX.set_CheckState((CheckState)1);
			((Control)chkX).set_Location(new Point(179, 13));
			((Control)chkX).set_Name("chkX");
			((Control)chkX).set_Size(new Size(29, 17));
			((Control)chkX).set_TabIndex(19);
			((Control)chkX).set_Text(".");
			((ButtonBase)chkX).set_UseVisualStyleBackColor(true);
			chkX.add_CheckedChanged((EventHandler)chkX_CheckedChanged);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_ForeColor(Color.DarkGreen);
			((Control)label9).set_Location(new Point(8, 191));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(107, 15));
			((Control)label9).set_TabIndex(18);
			((Control)label9).set_Text("Quality of the fit");
			((Control)cmbQuality).set_ForeColor(Color.Black);
			((ListControl)cmbQuality).set_FormattingEnabled(true);
			cmbQuality.get_Items().AddRange(new object[7] { "No reliable position or size", "*   No astrometry. Only 1 star of a double", "*   No astrometry. AOTA2B = Possible", "Astrometry only. No reliable size", "Limits on size, but no shape", "Reliable size. Can fit to shape models", "Resolution better than shape models" });
			((Control)cmbQuality).set_Location(new Point(117, 187));
			((Control)cmbQuality).set_Name("cmbQuality");
			((Control)cmbQuality).set_Size(new Size(217, 21));
			((Control)cmbQuality).set_TabIndex(17);
			cmbQuality.add_SelectedIndexChanged((EventHandler)cmbQuality_SelectedIndexChanged);
			((Control)cmbQuality).add_MouseClick(new MouseEventHandler(cmbQuality_MouseClick));
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(59, 37));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(48, 13));
			((Control)label8).set_TabIndex(14);
			((Control)label8).set_Text("Center Y");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(30, 64));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(77, 13));
			((Control)label7).set_TabIndex(13);
			((Control)label7).set_Text("Major axis (km)");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(30, 86));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(77, 13));
			((Control)label6).set_TabIndex(12);
			((Control)label6).set_Text("Minor axis (km)");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(49, 107));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(58, 13));
			((Control)label5).set_TabIndex(11);
			((Control)label5).set_Text("Orientation");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(59, 15));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(48, 13));
			((Control)label1).set_TabIndex(7);
			((Control)label1).set_Text("Center X");
			updnY.set_DecimalPlaces(1);
			((Control)updnY).set_Location(new Point(109, 33));
			updnY.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnY.set_Minimum(new decimal(new int[4] { 100000, 0, 0, -2147483648 }));
			((Control)updnY).set_Name("updnY");
			((Control)updnY).set_Size(new Size(58, 20));
			((Control)updnY).set_TabIndex(6);
			updnY.add_ValueChanged((EventHandler)updnY_ValueChanged);
			updnA.set_DecimalPlaces(1);
			((Control)updnA).set_Location(new Point(109, 60));
			updnA.set_Maximum(new decimal(new int[4] { 160000, 0, 0, 0 }));
			updnA.set_Minimum(new decimal(new int[4] { 160000, 0, 0, -2147483648 }));
			((Control)updnA).set_Name("updnA");
			((Control)updnA).set_Size(new Size(58, 20));
			((Control)updnA).set_TabIndex(5);
			updnA.add_ValueChanged((EventHandler)updnA_ValueChanged);
			updnB.set_DecimalPlaces(1);
			((Control)updnB).set_Location(new Point(109, 82));
			updnB.set_Maximum(new decimal(new int[4] { 160000, 0, 0, 0 }));
			updnB.set_Minimum(new decimal(new int[4] { 160000, 0, 0, -2147483648 }));
			((Control)updnB).set_Name("updnB");
			((Control)updnB).set_Size(new Size(58, 20));
			((Control)updnB).set_TabIndex(4);
			updnB.add_ValueChanged((EventHandler)updnB_ValueChanged);
			updnPA.set_DecimalPlaces(1);
			((Control)updnPA).set_Location(new Point(109, 104));
			updnPA.set_Maximum(new decimal(new int[4] { 365, 0, 0, 0 }));
			updnPA.set_Minimum(new decimal(new int[4] { 360, 0, 0, -2147483648 }));
			((Control)updnPA).set_Name("updnPA");
			((Control)updnPA).set_Size(new Size(58, 20));
			((Control)updnPA).set_TabIndex(3);
			updnPA.add_ValueChanged((EventHandler)updnPA_ValueChanged);
			updnX.set_DecimalPlaces(1);
			updnX.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnX).set_Location(new Point(109, 11));
			updnX.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnX.set_Minimum(new decimal(new int[4] { 100000, 0, 0, -2147483648 }));
			((Control)updnX).set_Name("updnX");
			((Control)updnX).set_Size(new Size(58, 20));
			((Control)updnX).set_TabIndex(0);
			updnX.add_ValueChanged((EventHandler)updnX_ValueChanged);
			((Control)PanelSatellites).set_BackColor(Color.Aquamarine);
			PanelSatellites.set_BorderStyle((BorderStyle)2);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)picDrag);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)pnlCBET);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)cmdEstimateOrbit);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)pnlNames);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label42);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label41);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label40);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label39);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)lblSatelliteNumbers);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label2);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label14);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label38);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label37);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label36);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label28);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)updnSatNum);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)PanelMiriade);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)lblSatelliteQuality);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)lblSatNum);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)cmdHelpOnCompanionID);
			((Control)PanelSatellites).get_Controls().Add((Control)(object)label30);
			((Control)PanelSatellites).set_Location(new Point(25, 62));
			((Control)PanelSatellites).set_Name("PanelSatellites");
			((Control)PanelSatellites).set_Size(new Size(265, 392));
			((Control)PanelSatellites).set_TabIndex(11);
			toolTip.SetToolTip((Control)(object)PanelSatellites, "Use TAB to change fields");
			((Control)PanelSatellites).set_Visible(false);
			((Control)PanelSatellites).add_MouseDown(new MouseEventHandler(panelSatellites_MouseDown));
			((Control)PanelSatellites).add_MouseEnter((EventHandler)PanelSatellites_MouseEnter);
			((Control)PanelSatellites).add_MouseHover((EventHandler)panelSatellites_MouseHover);
			((Control)PanelSatellites).add_MouseMove(new MouseEventHandler(panelSatellites_MouseMove));
			picDrag.set_Image((Image)Resources.DragDrop_26x);
			((Control)picDrag).set_Location(new Point(0, 0));
			((Control)picDrag).set_Name("picDrag");
			((Control)picDrag).set_Size(new Size(32, 31));
			picDrag.set_TabIndex(61);
			picDrag.set_TabStop(false);
			((Control)picDrag).add_MouseDown(new MouseEventHandler(picDrag_MouseDown));
			((Control)picDrag).add_MouseEnter((EventHandler)picDrag_MouseEnter);
			((Control)picDrag).add_MouseHover((EventHandler)picDrag_MouseHover);
			((Control)picDrag).add_MouseMove(new MouseEventHandler(picDrag_MouseMove));
			((Control)pnlCBET).set_BackColor(Color.PaleGoldenrod);
			pnlCBET.set_BorderStyle((BorderStyle)2);
			((Control)pnlCBET).get_Controls().Add((Control)(object)chkEditCBET);
			((Control)pnlCBET).get_Controls().Add((Control)(object)label66);
			((Control)pnlCBET).set_Location(new Point(1, 55));
			((Control)pnlCBET).set_Name("pnlCBET");
			((Control)pnlCBET).set_Size(new Size(258, 26));
			((Control)pnlCBET).set_TabIndex(60);
			toolTip.SetToolTip((Control)(object)pnlCBET, "The fields are Read-Only.\r\nTo edit, admistrator privelages must be set,\r\nand the Edit check box checked");
			((Control)chkEditCBET).set_AutoSize(true);
			chkEditCBET.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkEditCBET).set_Location(new Point(0, 3));
			((Control)chkEditCBET).set_Name("chkEditCBET");
			((Control)chkEditCBET).set_Size(new Size(44, 17));
			((Control)chkEditCBET).set_TabIndex(0);
			((Control)chkEditCBET).set_Text("Edit");
			((ButtonBase)chkEditCBET).set_UseVisualStyleBackColor(true);
			chkEditCBET.add_CheckedChanged((EventHandler)chkEditCBET_CheckedChanged);
			((Control)label66).set_AutoSize(true);
			((Control)label66).set_Location(new Point(61, 5));
			((Control)label66).set_Name("label66");
			((Control)label66).set_Size(new Size(142, 13));
			((Control)label66).set_TabIndex(1);
			((Control)label66).set_Text("Occultation Discovery CBET");
			label66.set_TextAlign(ContentAlignment.TopCenter);
			((Control)cmdEstimateOrbit).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdEstimateOrbit).set_Location(new Point(173, 31));
			((Control)cmdEstimateOrbit).set_Name("cmdEstimateOrbit");
			((Control)cmdEstimateOrbit).set_Size(new Size(87, 22));
			((Control)cmdEstimateOrbit).set_TabIndex(59);
			((Control)cmdEstimateOrbit).set_Text("Estimate orbit");
			((ButtonBase)cmdEstimateOrbit).set_UseVisualStyleBackColor(true);
			((Control)cmdEstimateOrbit).add_Click((EventHandler)cmdEstimateOrbit_Click);
			((Control)pnlNames).set_BackColor(Color.Ivory);
			pnlNames.set_BorderStyle((BorderStyle)2);
			((Control)pnlNames).get_Controls().Add((Control)(object)label43);
			((Control)pnlNames).get_Controls().Add((Control)(object)lblSat4);
			((Control)pnlNames).get_Controls().Add((Control)(object)lblSat1);
			((Control)pnlNames).get_Controls().Add((Control)(object)lblSat3);
			((Control)pnlNames).get_Controls().Add((Control)(object)lblSat2);
			((Control)pnlNames).set_Location(new Point(1, 117));
			((Control)pnlNames).set_Name("pnlNames");
			((Control)pnlNames).set_Size(new Size(258, 55));
			((Control)pnlNames).set_TabIndex(58);
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Location(new Point(14, 3));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(227, 13));
			((Control)label43).set_TabIndex(4);
			((Control)label43).set_Text("Named satellites.   Double click to select name");
			((Control)lblSat4).set_AutoSize(true);
			((Control)lblSat4).set_Location(new Point(129, 35));
			((Control)lblSat4).set_Name("lblSat4");
			((Control)lblSat4).set_Size(new Size(10, 13));
			((Control)lblSat4).set_TabIndex(3);
			((Control)lblSat4).set_Text(".");
			((Control)lblSat4).add_Click((EventHandler)lblSat4_Click);
			((Control)lblSat1).set_AutoSize(true);
			((Control)lblSat1).set_Location(new Point(11, 19));
			((Control)lblSat1).set_Name("lblSat1");
			((Control)lblSat1).set_Size(new Size(10, 13));
			((Control)lblSat1).set_TabIndex(0);
			((Control)lblSat1).set_Text(".");
			((Control)lblSat1).add_DoubleClick((EventHandler)lblsat1_DoubleClick);
			((Control)lblSat3).set_AutoSize(true);
			((Control)lblSat3).set_Location(new Point(129, 19));
			((Control)lblSat3).set_Name("lblSat3");
			((Control)lblSat3).set_Size(new Size(10, 13));
			((Control)lblSat3).set_TabIndex(2);
			((Control)lblSat3).set_Text(".");
			((Control)lblSat3).add_DoubleClick((EventHandler)lblSat3_DoubleClick);
			((Control)lblSat2).set_AutoSize(true);
			((Control)lblSat2).set_Location(new Point(11, 35));
			((Control)lblSat2).set_Name("lblSat2");
			((Control)lblSat2).set_Size(new Size(10, 13));
			((Control)lblSat2).set_TabIndex(1);
			((Control)lblSat2).set_Text(".");
			((Control)lblSat2).add_DoubleClick((EventHandler)lblSat2_DoubleClick);
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Location(new Point(171, 284));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(49, 13));
			((Control)label42).set_TabIndex(57);
			((Control)label42).set_Text("# chords");
			label41.set_BorderStyle((BorderStyle)1);
			((Control)label41).set_Location(new Point(1, 85));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(259, 1));
			((Control)label41).set_TabIndex(56);
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Location(new Point(208, 181));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(39, 13));
			((Control)label40).set_TabIndex(55);
			((Control)label40).set_Text("Uncert");
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Location(new Point(76, 181));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(39, 13));
			((Control)label39).set_TabIndex(54);
			((Control)label39).set_Text("Uncert");
			((Control)lblSatelliteNumbers).set_AutoSize(true);
			((Control)lblSatelliteNumbers).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblSatelliteNumbers).set_Location(new Point(131, 35));
			((Control)lblSatelliteNumbers).set_Name("lblSatelliteNumbers");
			((Control)lblSatelliteNumbers).set_Size(new Size(29, 13));
			((Control)lblSatelliteNumbers).set_TabIndex(51);
			((Control)lblSatelliteNumbers).set_Text("of 1");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(135, 181));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(71, 13));
			((Control)label2).set_TabIndex(50);
			((Control)label2).set_Text("PA to satellite");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(10, 181));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(60, 13));
			((Control)label14).set_TabIndex(49);
			((Control)label14).set_Text("Sepn (mas)");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Location(new Point(165, 239));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(62, 13));
			((Control)label38).set_TabIndex(47);
			((Control)label38).set_Text("PA of Major");
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Location(new Point(88, 239));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(54, 13));
			((Control)label37).set_TabIndex(46);
			((Control)label37).set_Text("Minor axis");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Location(new Point(10, 239));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(54, 13));
			((Control)label36).set_TabIndex(45);
			((Control)label36).set_Text("Major axis");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 11f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(69, 3));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(122, 18));
			((Control)label28).set_TabIndex(44);
			((Control)label28).set_Text("Satellite details");
			((Control)updnSatNum).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)updnSatNum).set_Location(new Point(89, 32));
			updnSatNum.set_Maximum(new decimal(new int[4] { 4, 0, 0, 0 }));
			updnSatNum.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnSatNum).set_Name("updnSatNum");
			((Control)updnSatNum).set_Size(new Size(38, 20));
			((Control)updnSatNum).set_TabIndex(43);
			updnSatNum.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnSatNum.add_ValueChanged((EventHandler)updnSatNum_ValueChanged);
			((Control)PanelMiriade).set_BackColor(Color.PaleTurquoise);
			PanelMiriade.set_BorderStyle((BorderStyle)2);
			((Control)PanelMiriade).get_Controls().Add((Control)(object)label34);
			((Control)PanelMiriade).get_Controls().Add((Control)(object)label33);
			((Control)PanelMiriade).get_Controls().Add((Control)(object)label32);
			((Control)PanelMiriade).get_Controls().Add((Control)(object)label31);
			((Control)PanelMiriade).get_Controls().Add((Control)(object)label29);
			((Control)PanelMiriade).get_Controls().Add((Control)(object)cmdGetSatelliteMotions);
			((Control)PanelMiriade).set_Location(new Point(0, 307));
			((Control)PanelMiriade).set_Name("PanelMiriade");
			((Control)PanelMiriade).set_Size(new Size(258, 79));
			((Control)PanelMiriade).set_TabIndex(0);
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Location(new Point(205, 35));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(40, 26));
			((Control)label34).set_TabIndex(16);
			((Control)label34).set_Text("units: \r\nmas/hr");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Location(new Point(10, 54));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(33, 13));
			((Control)label33).set_TabIndex(15);
			((Control)label33).set_Text("dDec");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Location(new Point(97, 54));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(39, 13));
			((Control)label32).set_TabIndex(14);
			((Control)label32).set_Text("d2Dec");
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Location(new Point(97, 31));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(34, 13));
			((Control)label31).set_TabIndex(13);
			((Control)label31).set_Text("d2RA");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Location(new Point(10, 31));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(28, 13));
			((Control)label29).set_TabIndex(12);
			((Control)label29).set_Text("dRA");
			((Control)cmdGetSatelliteMotions).set_BackColor(Color.DarkTurquoise);
			((ButtonBase)cmdGetSatelliteMotions).set_FlatStyle((FlatStyle)0);
			((Control)cmdGetSatelliteMotions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdGetSatelliteMotions).set_Location(new Point(14, 4));
			((Control)cmdGetSatelliteMotions).set_Name("cmdGetSatelliteMotions");
			((Control)cmdGetSatelliteMotions).set_Size(new Size(226, 22));
			((Control)cmdGetSatelliteMotions).set_TabIndex(7);
			((Control)cmdGetSatelliteMotions).set_Text("Get motion of satellite - from Miriade");
			((ButtonBase)cmdGetSatelliteMotions).set_UseVisualStyleBackColor(false);
			((Control)cmdGetSatelliteMotions).add_Click((EventHandler)cmdGetSatelliteMotions_Click);
			((Control)lblSatelliteQuality).set_AutoSize(true);
			((Control)lblSatelliteQuality).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblSatelliteQuality).set_Location(new Point(11, 284));
			((Control)lblSatelliteQuality).set_Name("lblSatelliteQuality");
			((Control)lblSatelliteQuality).set_Size(new Size(55, 13));
			((Control)lblSatelliteQuality).set_TabIndex(39);
			((Control)lblSatelliteQuality).set_Text("Satellite fit");
			((Control)lblSatNum).set_AutoSize(true);
			((Control)lblSatNum).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblSatNum).set_Location(new Point(22, 36));
			((Control)lblSatNum).set_Name("lblSatNum");
			((Control)lblSatNum).set_Size(new Size(65, 13));
			((Control)lblSatNum).set_TabIndex(42);
			((Control)lblSatNum).set_Text("Satellite #");
			((Control)cmdHelpOnCompanionID).set_BackColor(Color.SkyBlue);
			((ButtonBase)cmdHelpOnCompanionID).set_FlatStyle((FlatStyle)0);
			((Control)cmdHelpOnCompanionID).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdHelpOnCompanionID).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelpOnCompanionID).set_Location(new Point(213, 2));
			((Control)cmdHelpOnCompanionID).set_Name("cmdHelpOnCompanionID");
			((Control)cmdHelpOnCompanionID).set_Size(new Size(45, 22));
			((Control)cmdHelpOnCompanionID).set_TabIndex(5);
			((Control)cmdHelpOnCompanionID).set_Text("Help");
			((ButtonBase)cmdHelpOnCompanionID).set_UseVisualStyleBackColor(false);
			((Control)cmdHelpOnCompanionID).add_Click((EventHandler)cmdHelpOnCompanionID_Click);
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_BackColor(Color.Aquamarine);
			((Control)label30).set_Location(new Point(11, 95));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(82, 13));
			((Control)label30).set_TabIndex(4);
			((Control)label30).set_Text("IAU designation");
			((Control)panelDouble).set_BackColor(Color.Moccasin);
			panelDouble.set_BorderStyle((BorderStyle)2);
			((Control)panelDouble).get_Controls().Add((Control)(object)picDragDouble);
			((Control)panelDouble).get_Controls().Add((Control)(object)panelJDSO);
			((Control)panelDouble).get_Controls().Add((Control)(object)panel7);
			((Control)panelDouble).get_Controls().Add((Control)(object)panelSolve);
			((Control)panelDouble).get_Controls().Add((Control)(object)panel6);
			((Control)panelDouble).get_Controls().Add((Control)(object)cmdHelpDoubles);
			((Control)panelDouble).get_Controls().Add((Control)(object)pnlDoubleStarSolution);
			((Control)panelDouble).get_Controls().Add((Control)(object)lblDoubleStar);
			((Control)panelDouble).set_Location(new Point(18, 236));
			((Control)panelDouble).set_Name("panelDouble");
			((Control)panelDouble).set_Size(new Size(306, 491));
			((Control)panelDouble).set_TabIndex(0);
			((Control)panelDouble).set_Visible(false);
			((Control)panelDouble).add_MouseDown(new MouseEventHandler(panelDouble_MouseDown));
			((Control)panelDouble).add_MouseHover((EventHandler)panelDouble_MouseHover);
			((Control)panelDouble).add_MouseMove(new MouseEventHandler(panelDouble_MouseMove));
			picDragDouble.set_Image((Image)Resources.DragDrop_26x);
			((Control)picDragDouble).set_Location(new Point(-1, -1));
			((Control)picDragDouble).set_Name("picDragDouble");
			((Control)picDragDouble).set_Size(new Size(32, 31));
			picDragDouble.set_TabIndex(62);
			picDragDouble.set_TabStop(false);
			((Control)picDragDouble).add_MouseDown(new MouseEventHandler(picDragDouble_MouseDown));
			((Control)picDragDouble).add_MouseHover((EventHandler)picDragDouble_MouseHover);
			((Control)picDragDouble).add_MouseMove(new MouseEventHandler(picDragDouble_MouseMove));
			((Control)panelJDSO).set_BackColor(Color.FromArgb(144, 220, 144));
			panelJDSO.set_BorderStyle((BorderStyle)2);
			((Control)panelJDSO).get_Controls().Add((Control)(object)panel3);
			((Control)panelJDSO).get_Controls().Add((Control)(object)panel1);
			((Control)panelJDSO).get_Controls().Add((Control)(object)label62);
			((Control)panelJDSO).get_Controls().Add((Control)(object)label61);
			((Control)panelJDSO).get_Controls().Add((Control)(object)txtJDSO_Vol_Num_Pg);
			((Control)panelJDSO).get_Controls().Add((Control)(object)txtJDSOSubmitDate);
			((Control)panelJDSO).get_Controls().Add((Control)(object)label59);
			((Control)panelJDSO).get_Controls().Add((Control)(object)label60);
			((Control)panelJDSO).set_Location(new Point(4, 422));
			((Control)panelJDSO).set_Name("panelJDSO");
			((Control)panelJDSO).set_Size(new Size(295, 63));
			((Control)panelJDSO).set_TabIndex(46);
			toolTip.SetToolTip((Control)(object)panelJDSO, "The fields are Read-Only.\r\nTo edit, admistrator privelages must be set,\r\nand the Edit check box checked");
			((Control)panel3).set_BackColor(Color.YellowGreen);
			((Control)panel3).get_Controls().Add((Control)(object)chkEditJDSO);
			((Control)panel3).set_Location(new Point(-1, -1));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(32, 60));
			((Control)panel3).set_TabIndex(10);
			((Control)chkEditJDSO).set_AutoSize(true);
			chkEditJDSO.set_CheckAlign(ContentAlignment.TopCenter);
			((Control)chkEditJDSO).set_Location(new Point(2, 15));
			((Control)chkEditJDSO).set_Name("chkEditJDSO");
			((Control)chkEditJDSO).set_Size(new Size(29, 31));
			((Control)chkEditJDSO).set_TabIndex(0);
			((Control)chkEditJDSO).set_Text("Edit");
			((ButtonBase)chkEditJDSO).set_UseVisualStyleBackColor(true);
			chkEditJDSO.add_CheckedChanged((EventHandler)chkEditJDSO_CheckedChanged);
			((Control)panel1).set_BackColor(Color.PaleGreen);
			((Control)panel1).get_Controls().Add((Control)(object)txtDoublePairID);
			((Control)panel1).get_Controls().Add((Control)(object)label64);
			((Control)panel1).set_Location(new Point(31, 0));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(261, 21));
			((Control)panel1).set_TabIndex(9);
			((Control)txtDoublePairID).set_BackColor(Color.Honeydew);
			((Control)txtDoublePairID).set_Location(new Point(213, 0));
			((Control)txtDoublePairID).set_Name("txtDoublePairID");
			((TextBoxBase)txtDoublePairID).set_ReadOnly(true);
			((Control)txtDoublePairID).set_Size(new Size(43, 20));
			((Control)txtDoublePairID).set_TabIndex(8);
			((Control)label64).set_AutoSize(true);
			((Control)label64).set_BackColor(Color.PaleGreen);
			((Control)label64).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label64).set_ForeColor(Color.MidnightBlue);
			((Control)label64).set_Location(new Point(1, 4));
			((Control)label64).set_Name("label64");
			((Control)label64).set_Size(new Size(212, 13));
			((Control)label64).set_TabIndex(7);
			((Control)label64).set_Text("Known double at event date.  Pair =");
			((Control)label62).set_AutoSize(true);
			((Control)label62).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label62).set_Location(new Point(198, 22));
			((Control)label62).set_Name("label62");
			((Control)label62).set_Size(new Size(82, 13));
			((Control)label62).set_TabIndex(5);
			((Control)label62).set_Text("Vol **, #*, pg ***");
			((Control)label61).set_AutoSize(true);
			((Control)label61).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label61).set_Location(new Point(78, 21));
			((Control)label61).set_Name("label61");
			((Control)label61).set_Size(new Size(73, 13));
			((Control)label61).set_TabIndex(2);
			((Control)label61).set_Text("yyyy - mm - dd");
			((Control)txtJDSO_Vol_Num_Pg).set_AllowDrop(true);
			((Control)txtJDSO_Vol_Num_Pg).set_BackColor(Color.FromArgb(255, 230, 240));
			((Control)txtJDSO_Vol_Num_Pg).set_Location(new Point(191, 35));
			((TextBoxBase)txtJDSO_Vol_Num_Pg).set_MaxLength(1000);
			((Control)txtJDSO_Vol_Num_Pg).set_Name("txtJDSO_Vol_Num_Pg");
			((Control)txtJDSO_Vol_Num_Pg).set_Size(new Size(98, 20));
			((Control)txtJDSO_Vol_Num_Pg).set_TabIndex(6);
			((Control)txtJDSO_Vol_Num_Pg).add_TextChanged((EventHandler)txtJDSO_Vol_Num_Pg_TextChanged);
			((Control)txtJDSO_Vol_Num_Pg).add_Leave((EventHandler)txtJDSO_Vol_Num_Pg_Leave);
			((Control)txtJDSOSubmitDate).set_AllowDrop(true);
			((Control)txtJDSOSubmitDate).set_BackColor(Color.FromArgb(255, 230, 240));
			((Control)txtJDSOSubmitDate).set_Location(new Point(80, 35));
			((TextBoxBase)txtJDSOSubmitDate).set_MaxLength(1000);
			((Control)txtJDSOSubmitDate).set_Name("txtJDSOSubmitDate");
			((TextBoxBase)txtJDSOSubmitDate).set_ReadOnly(true);
			((Control)txtJDSOSubmitDate).set_Size(new Size(67, 20));
			((Control)txtJDSOSubmitDate).set_TabIndex(3);
			((Control)txtJDSOSubmitDate).add_TextChanged((EventHandler)txtJDSOSubmitDate_TextChanged);
			((Control)txtJDSOSubmitDate).add_Leave((EventHandler)txtJDSOSubmitDate_Leave);
			((Control)label59).set_AutoSize(true);
			((Control)label59).set_Location(new Point(159, 29));
			((Control)label59).set_Name("label59");
			((Control)label59).set_Size(new Size(35, 26));
			((Control)label59).set_TabIndex(4);
			((Control)label59).set_Text("JDSO\r\nref");
			label59.set_TextAlign(ContentAlignment.TopCenter);
			((Control)label60).set_AutoSize(true);
			((Control)label60).set_Location(new Point(31, 29));
			((Control)label60).set_Name("label60");
			((Control)label60).set_Size(new Size(52, 26));
			((Control)label60).set_TabIndex(1);
			((Control)label60).set_Text("JDSO\r\nsubmitted");
			label60.set_TextAlign(ContentAlignment.TopCenter);
			((Control)panel7).set_BackColor(Color.Turquoise);
			panel7.set_BorderStyle((BorderStyle)2);
			((Control)panel7).get_Controls().Add((Control)(object)label58);
			((Control)panel7).get_Controls().Add((Control)(object)label49);
			((Control)panel7).get_Controls().Add((Control)(object)label13);
			((Control)panel7).get_Controls().Add((Control)(object)lblOffset);
			((Control)panel7).get_Controls().Add((Control)(object)txtMag2nd);
			((Control)panel7).get_Controls().Add((Control)(object)txtMagMain);
			((Control)panel7).get_Controls().Add((Control)(object)label56);
			((Control)panel7).get_Controls().Add((Control)(object)label55);
			((Control)panel7).get_Controls().Add((Control)(object)label54);
			((Control)panel7).get_Controls().Add((Control)(object)label53);
			((Control)panel7).get_Controls().Add((Control)(object)txt2ndDrop);
			((Control)panel7).get_Controls().Add((Control)(object)txtMainDrop);
			((Control)panel7).get_Controls().Add((Control)(object)label48);
			((Control)panel7).get_Controls().Add((Control)(object)label47);
			((Control)panel7).get_Controls().Add((Control)(object)updnBrightRatio);
			((Control)panel7).get_Controls().Add((Control)(object)updnBrightnessUncertPerCent);
			((Control)panel7).set_Location(new Point(4, 33));
			((Control)panel7).set_Name("panel7");
			((Control)panel7).set_Size(new Size(295, 71));
			((Control)panel7).set_TabIndex(0);
			toolTip.SetToolTip((Control)(object)panel7, "The brightness ratio is required to correctly report astrometry");
			((Control)label58).set_AutoSize(true);
			((Control)label58).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label58).set_ForeColor(Color.MediumBlue);
			((Control)label58).set_Location(new Point(26, -1));
			((Control)label58).set_Name("label58");
			((Control)label58).set_Size(new Size(238, 13));
			((Control)label58).set_TabIndex(59);
			((Control)label58).set_Text("S e t    r e l a t i v e     b r i g h t n e s s ");
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label49).set_ForeColor(Color.Red);
			((Control)label49).set_Location(new Point(93, 10));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(28, 13));
			((Control)label49).set_TabIndex(58);
			((Control)label49).set_Text("<=>");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_ForeColor(Color.Red);
			((Control)label13).set_Location(new Point(202, 10));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(21, 13));
			((Control)label13).set_TabIndex(57);
			((Control)label13).set_Text("=>");
			((Control)lblOffset).set_BackColor(Color.Turquoise);
			((Control)lblOffset).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblOffset).set_ForeColor(SystemColors.ControlText);
			((Control)lblOffset).set_Location(new Point(122, 10));
			((Control)lblOffset).set_Name("lblOffset");
			((Control)lblOffset).set_Size(new Size(80, 13));
			((Control)lblOffset).set_TabIndex(48);
			((Control)lblOffset).set_Text("Brightness ratio");
			lblOffset.set_TextAlign(ContentAlignment.MiddleCenter);
			toolTip.SetToolTip((Control)(object)lblOffset, componentResourceManager.GetString("lblOffset.ToolTip"));
			((Control)txtMag2nd).set_Location(new Point(237, 46));
			((Control)txtMag2nd).set_Name("txtMag2nd");
			((TextBoxBase)txtMag2nd).set_ReadOnly(true);
			((Control)txtMag2nd).set_Size(new Size(36, 20));
			((Control)txtMag2nd).set_TabIndex(7);
			((Control)txtMag2nd).set_Text("20.0");
			((TextBoxBase)txtMagMain).set_BorderStyle((BorderStyle)1);
			((Control)txtMagMain).set_Location(new Point(237, 25));
			((Control)txtMagMain).set_Name("txtMagMain");
			((TextBoxBase)txtMagMain).set_ReadOnly(true);
			((Control)txtMagMain).set_Size(new Size(36, 20));
			((Control)txtMagMain).set_TabIndex(6);
			((Control)txtMagMain).set_Text("20.0");
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label56).set_Location(new Point(224, 10));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(62, 13));
			((Control)label56).set_TabIndex(54);
			((Control)label56).set_Text("Magnitudes");
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label55).set_Location(new Point(6, 10));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(88, 13));
			((Control)label55).set_TabIndex(53);
			((Control)label55).set_Text("Light curve drops");
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Location(new Point(9, 47));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(45, 13));
			((Control)label54).set_TabIndex(1);
			((Control)label54).set_Text("2nd star");
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Location(new Point(4, 29));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(50, 13));
			((Control)label53).set_TabIndex(0);
			((Control)label53).set_Text("Main star");
			((Control)txt2ndDrop).set_Location(new Point(58, 46));
			((Control)txt2ndDrop).set_Name("txt2ndDrop");
			((TextBoxBase)txt2ndDrop).set_ShortcutsEnabled(false);
			((Control)txt2ndDrop).set_Size(new Size(36, 20));
			((Control)txt2ndDrop).set_TabIndex(3);
			((Control)txt2ndDrop).set_Text("85");
			((Control)txt2ndDrop).add_TextChanged((EventHandler)txt2ndDrop_TextChanged);
			((Control)txt2ndDrop).add_KeyPress(new KeyPressEventHandler(txt2ndDrop_KeyPress));
			((Control)txtMainDrop).set_Location(new Point(58, 25));
			((Control)txtMainDrop).set_Name("txtMainDrop");
			((TextBoxBase)txtMainDrop).set_ShortcutsEnabled(false);
			((Control)txtMainDrop).set_Size(new Size(36, 20));
			((Control)txtMainDrop).set_TabIndex(2);
			((Control)txtMainDrop).set_Text("100");
			((Control)txtMainDrop).add_TextChanged((EventHandler)txtMainDrop_TextChanged);
			((Control)txtMainDrop).add_KeyPress(new KeyPressEventHandler(txtMainDrop_KeyPress));
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label48).set_Location(new Point(134, 47));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(14, 13));
			((Control)label48).set_TabIndex(5);
			((Control)label48).set_Text("±");
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(184, 49));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(16, 13));
			((Control)label47).set_TabIndex(45);
			((Control)label47).set_Text("%");
			updnBrightRatio.set_DecimalPlaces(2);
			updnBrightRatio.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnBrightRatio).set_Location(new Point(138, 24));
			updnBrightRatio.set_Maximum(new decimal(new int[4] { 50, 0, 0, 0 }));
			updnBrightRatio.set_Minimum(new decimal(new int[4] { 2, 0, 0, 131072 }));
			((Control)updnBrightRatio).set_Name("updnBrightRatio");
			((Control)updnBrightRatio).set_Size(new Size(49, 20));
			((Control)updnBrightRatio).set_TabIndex(4);
			toolTip.SetToolTip((Control)(object)updnBrightRatio, componentResourceManager.GetString("updnBrightRatio.ToolTip"));
			updnBrightRatio.set_Value(new decimal(new int[4] { 12, 0, 0, 65536 }));
			updnBrightRatio.add_ValueChanged((EventHandler)updnBrightRatio_ValueChanged);
			updnBrightnessUncertPerCent.set_Increment(new decimal(new int[4] { 2, 0, 0, 0 }));
			((Control)updnBrightnessUncertPerCent).set_Location(new Point(149, 45));
			updnBrightnessUncertPerCent.set_Maximum(new decimal(new int[4] { 40, 0, 0, 0 }));
			updnBrightnessUncertPerCent.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnBrightnessUncertPerCent).set_Name("updnBrightnessUncertPerCent");
			((Control)updnBrightnessUncertPerCent).set_Size(new Size(38, 20));
			((Control)updnBrightnessUncertPerCent).set_TabIndex(44);
			toolTip.SetToolTip((Control)(object)updnBrightnessUncertPerCent, componentResourceManager.GetString("updnBrightnessUncertPerCent.ToolTip"));
			updnBrightnessUncertPerCent.set_Value(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)panelSolve).set_BackColor(Color.Lavender);
			panelSolve.set_BorderStyle((BorderStyle)2);
			((Control)panelSolve).get_Controls().Add((Control)(object)pnlOffset);
			((Control)panelSolve).get_Controls().Add((Control)(object)cmdFit_Doubles);
			((Control)panelSolve).get_Controls().Add((Control)(object)lblSolveSolnNum);
			((Control)panelSolve).get_Controls().Add((Control)(object)label4);
			((Control)panelSolve).get_Controls().Add((Control)(object)label3);
			((Control)panelSolve).get_Controls().Add((Control)(object)chkCompanionPA);
			((Control)panelSolve).get_Controls().Add((Control)(object)chkCompanionSep);
			((Control)panelSolve).set_Location(new Point(4, 313));
			((Control)panelSolve).set_Name("panelSolve");
			((Control)panelSolve).set_Size(new Size(295, 104));
			((Control)panelSolve).set_TabIndex(3);
			((Control)pnlOffset).set_BackColor(Color.FromArgb(244, 233, 255));
			pnlOffset.set_BorderStyle((BorderStyle)1);
			((Control)pnlOffset).get_Controls().Add((Control)(object)panel9);
			((Control)pnlOffset).get_Controls().Add((Control)(object)label51);
			((Control)pnlOffset).get_Controls().Add((Control)(object)panel8);
			((Control)pnlOffset).get_Controls().Add((Control)(object)cmdSetOffsets);
			((Control)pnlOffset).set_Location(new Point(-1, 54));
			((Control)pnlOffset).set_Name("pnlOffset");
			((Control)pnlOffset).set_Size(new Size(293, 50));
			((Control)pnlOffset).set_TabIndex(12);
			panel9.set_BorderStyle((BorderStyle)2);
			((Control)panel9).get_Controls().Add((Control)(object)lbl4);
			((Control)panel9).get_Controls().Add((Control)(object)lbl3);
			((Control)panel9).get_Controls().Add((Control)(object)label57);
			((Control)panel9).set_Location(new Point(190, 14));
			((Control)panel9).set_Name("panel9");
			((Control)panel9).set_Size(new Size(93, 31));
			((Control)panel9).set_TabIndex(56);
			((Control)lbl4).set_AutoSize(true);
			((Control)lbl4).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl4).set_Location(new Point(10, 13));
			((Control)lbl4).set_Name("lbl4");
			((Control)lbl4).set_Size(new Size(43, 13));
			((Control)lbl4).set_TabIndex(4);
			((Control)lbl4).set_Text("#4 (0,0)");
			((Control)lbl3).set_AutoSize(true);
			((Control)lbl3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl3).set_Location(new Point(10, -1));
			((Control)lbl3).set_Name("lbl3");
			((Control)lbl3).set_Size(new Size(43, 13));
			((Control)lbl3).set_TabIndex(3);
			((Control)lbl3).set_Text("#3 (0,0)");
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label57).set_ForeColor(Color.Blue);
			((Control)label57).set_Location(new Point(-2, 7));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(15, 13));
			((Control)label57).set_TabIndex(5);
			((Control)label57).set_Text("S");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_ForeColor(Color.Firebrick);
			((Control)label51).set_Location(new Point(0, 0));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(290, 13));
			((Control)label51).set_TabIndex(54);
			((Control)label51).set_Text("Set the 2 possible positions of the chords for first component");
			panel8.set_BorderStyle((BorderStyle)2);
			((Control)panel8).get_Controls().Add((Control)(object)lbl2);
			((Control)panel8).get_Controls().Add((Control)(object)lbl1);
			((Control)panel8).get_Controls().Add((Control)(object)label52);
			((Control)panel8).set_Location(new Point(89, 14));
			((Control)panel8).set_Name("panel8");
			((Control)panel8).set_Size(new Size(93, 31));
			((Control)panel8).set_TabIndex(55);
			((Control)lbl2).set_AutoSize(true);
			((Control)lbl2).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl2).set_Location(new Point(10, 13));
			((Control)lbl2).set_Name("lbl2");
			((Control)lbl2).set_Size(new Size(43, 13));
			((Control)lbl2).set_TabIndex(2);
			((Control)lbl2).set_Text("#2 (0,0)");
			((Control)lbl1).set_AutoSize(true);
			((Control)lbl1).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl1).set_Location(new Point(10, -1));
			((Control)lbl1).set_Name("lbl1");
			((Control)lbl1).set_Size(new Size(43, 13));
			((Control)lbl1).set_TabIndex(1);
			((Control)lbl1).set_Text("#1 (0,0)");
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label52).set_ForeColor(Color.Blue);
			((Control)label52).set_Location(new Point(-2, 7));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(15, 13));
			((Control)label52).set_TabIndex(3);
			((Control)label52).set_Text("N");
			((Control)cmdSetOffsets).set_BackColor(Color.Chartreuse);
			((ButtonBase)cmdSetOffsets).set_FlatStyle((FlatStyle)0);
			((Control)cmdSetOffsets).set_Location(new Point(7, 18));
			((Control)cmdSetOffsets).set_Name("cmdSetOffsets");
			((Control)cmdSetOffsets).set_Size(new Size(74, 23));
			((Control)cmdSetOffsets).set_TabIndex(1);
			((Control)cmdSetOffsets).set_Text("Set chords");
			((ButtonBase)cmdSetOffsets).set_UseVisualStyleBackColor(false);
			((Control)cmdSetOffsets).add_Click((EventHandler)cmdSetOffsets_Click);
			((Control)cmdFit_Doubles).set_BackgroundImage((Image)Resources.Graph05a);
			((Control)cmdFit_Doubles).set_BackgroundImageLayout((ImageLayout)3);
			((ButtonBase)cmdFit_Doubles).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)cmdFit_Doubles).set_FlatStyle((FlatStyle)1);
			((Control)cmdFit_Doubles).set_Location(new Point(258, 2));
			((Control)cmdFit_Doubles).set_Name("cmdFit_Doubles");
			((Control)cmdFit_Doubles).set_Size(new Size(30, 28));
			((Control)cmdFit_Doubles).set_TabIndex(0);
			toolTip.SetToolTip((Control)(object)cmdFit_Doubles, "This will adjust the Separation and PA\r\nto obtain the best fit. NO adjustments\r\nare made to the main star solution");
			((ButtonBase)cmdFit_Doubles).set_UseVisualStyleBackColor(true);
			((Control)cmdFit_Doubles).add_Click((EventHandler)cmdFit_Doubles_Click);
			((Control)lblSolveSolnNum).set_AutoSize(true);
			((Control)lblSolveSolnNum).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblSolveSolnNum).set_ForeColor(Color.Crimson);
			((Control)lblSolveSolnNum).set_Location(new Point(61, 2));
			((Control)lblSolveSolnNum).set_Name("lblSolveSolnNum");
			((Control)lblSolveSolnNum).set_Size(new Size(166, 13));
			((Control)lblSolveSolnNum).set_TabIndex(1);
			((Control)lblSolveSolnNum).set_Text("S o l v e   s o l u t i o n   #1");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(19, 16));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(72, 13));
			((Control)label4).set_TabIndex(0);
			((Control)label4).set_Text("Sepn (masec)");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(171, 16));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(54, 13));
			((Control)label3).set_TabIndex(3);
			((Control)label3).set_Text("PA of 2nd");
			((Control)chkCompanionPA).set_AutoSize(true);
			((Control)chkCompanionPA).set_Location(new Point(234, 33));
			((Control)chkCompanionPA).set_Name("chkCompanionPA");
			((Control)chkCompanionPA).set_Size(new Size(29, 17));
			((Control)chkCompanionPA).set_TabIndex(4);
			((Control)chkCompanionPA).set_Text(".");
			((ButtonBase)chkCompanionPA).set_UseVisualStyleBackColor(true);
			chkCompanionPA.add_CheckedChanged((EventHandler)chkPA2nd_CheckedChanged);
			((Control)chkCompanionSep).set_AutoSize(true);
			((Control)chkCompanionSep).set_Location(new Point(91, 33));
			((Control)chkCompanionSep).set_Name("chkCompanionSep");
			((Control)chkCompanionSep).set_Size(new Size(29, 17));
			((Control)chkCompanionSep).set_TabIndex(2);
			((Control)chkCompanionSep).set_Text(".");
			((ButtonBase)chkCompanionSep).set_UseVisualStyleBackColor(true);
			chkCompanionSep.add_CheckedChanged((EventHandler)chkSep_CheckedChanged);
			((Control)panel6).set_BackColor(Color.PaleTurquoise);
			panel6.set_BorderStyle((BorderStyle)2);
			((Control)panel6).get_Controls().Add((Control)(object)opt1Component);
			((Control)panel6).get_Controls().Add((Control)(object)textBox1);
			((Control)panel6).get_Controls().Add((Control)(object)label50);
			((Control)panel6).get_Controls().Add((Control)(object)opt4);
			((Control)panel6).get_Controls().Add((Control)(object)optTwo);
			((Control)panel6).get_Controls().Add((Control)(object)optSingle);
			((Control)panel6).set_Location(new Point(4, 110));
			((Control)panel6).set_Name("panel6");
			((Control)panel6).set_Size(new Size(295, 153));
			((Control)panel6).set_TabIndex(1);
			opt1Component.set_AutoCheck(false);
			((Control)opt1Component).set_AutoSize(true);
			((Control)opt1Component).set_Location(new Point(5, 58));
			((Control)opt1Component).set_Name("opt1Component");
			((Control)opt1Component).set_Size(new Size(286, 17));
			((Control)opt1Component).set_TabIndex(54);
			((Control)opt1Component).set_Text("One component only → set mags (auto min. separation)");
			toolTip.SetToolTip((Control)(object)opt1Component, "The main star should be set in the editor to\r\nbeing the one with multiple chords, even if \r\nit is the fainter star. \r\n");
			((ButtonBase)opt1Component).set_UseVisualStyleBackColor(true);
			((Control)opt1Component).add_Click((EventHandler)opt1Component_Click);
			((Control)textBox1).set_BackColor(Color.LightCyan);
			((TextBoxBase)textBox1).set_BorderStyle((BorderStyle)0);
			((Control)textBox1).set_Enabled(false);
			((Control)textBox1).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox1).set_ForeColor(SystemColors.WindowText);
			((Control)textBox1).set_Location(new Point(1, 75));
			((TextBoxBase)textBox1).set_Multiline(true);
			((Control)textBox1).set_Name("textBox1");
			((Control)textBox1).set_Size(new Size(291, 76));
			((Control)textBox1).set_TabIndex(53);
			((Control)textBox1).set_Text(componentResourceManager.GetString("textBox1.Text"));
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label50).set_ForeColor(Color.MediumBlue);
			((Control)label50).set_Location(new Point(44, 1));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(203, 13));
			((Control)label50).set_TabIndex(52);
			((Control)label50).set_Text("S e l e c t    s o l u t i o n    t y p e");
			opt4.set_AutoCheck(false);
			((Control)opt4).set_AutoSize(true);
			opt4.set_Checked(true);
			((Control)opt4).set_Location(new Point(5, 43));
			((Control)opt4).set_Name("opt4");
			((Control)opt4).set_Size(new Size(207, 17));
			((Control)opt4).set_TabIndex(2);
			opt4.set_TabStop(true);
			((Control)opt4).set_Text("Single chords for both → four solutions");
			((ButtonBase)opt4).set_UseVisualStyleBackColor(true);
			((Control)opt4).add_Click((EventHandler)opt4_Click);
			optTwo.set_AutoCheck(false);
			((Control)optTwo).set_AutoSize(true);
			((Control)optTwo).set_Location(new Point(5, 28));
			((Control)optTwo).set_Name("optTwo");
			((Control)optTwo).set_Size(new Size(285, 17));
			((Control)optTwo).set_TabIndex(1);
			((Control)optTwo).set_Text("Multiple chords for one, single for other → two solutions");
			toolTip.SetToolTip((Control)(object)optTwo, "The main star should be set in the editor to\r\nbeing the one with multiple chords, even if \r\nit is the fainter star. \r\n");
			((ButtonBase)optTwo).set_UseVisualStyleBackColor(true);
			((Control)optTwo).add_Click((EventHandler)optTwo_Click);
			optSingle.set_AutoCheck(false);
			((Control)optSingle).set_AutoSize(true);
			((Control)optSingle).set_Location(new Point(5, 13));
			((Control)optSingle).set_Name("optSingle");
			((Control)optSingle).set_Size(new Size(215, 17));
			((Control)optSingle).set_TabIndex(0);
			((Control)optSingle).set_Text("Multiple chords for both → one solution. ");
			((ButtonBase)optSingle).set_UseVisualStyleBackColor(true);
			((Control)optSingle).add_Click((EventHandler)optSingle_Click);
			((Control)cmdHelpDoubles).set_BackColor(Color.SkyBlue);
			((ButtonBase)cmdHelpDoubles).set_FlatStyle((FlatStyle)0);
			((Control)cmdHelpDoubles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdHelpDoubles).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelpDoubles).set_Location(new Point(246, 4));
			((Control)cmdHelpDoubles).set_Name("cmdHelpDoubles");
			((Control)cmdHelpDoubles).set_Size(new Size(52, 24));
			((Control)cmdHelpDoubles).set_TabIndex(41);
			((Control)cmdHelpDoubles).set_Text("Help");
			((ButtonBase)cmdHelpDoubles).set_UseVisualStyleBackColor(false);
			((Control)cmdHelpDoubles).add_Click((EventHandler)cmdHelpDoubles_Click);
			((Control)pnlDoubleStarSolution).set_BackColor(Color.LemonChiffon);
			pnlDoubleStarSolution.set_BorderStyle((BorderStyle)2);
			((Control)pnlDoubleStarSolution).get_Controls().Add((Control)(object)panel10);
			((Control)pnlDoubleStarSolution).get_Controls().Add((Control)(object)lblSelectSep);
			((Control)pnlDoubleStarSolution).get_Controls().Add((Control)(object)lblNumberOfSolutions);
			((Control)pnlDoubleStarSolution).get_Controls().Add((Control)(object)lblSelectSth);
			((Control)pnlDoubleStarSolution).get_Controls().Add((Control)(object)lblSelectNth);
			((Control)pnlDoubleStarSolution).set_Location(new Point(4, 268));
			((Control)pnlDoubleStarSolution).set_Name("pnlDoubleStarSolution");
			((Control)pnlDoubleStarSolution).set_Size(new Size(295, 39));
			((Control)pnlDoubleStarSolution).set_TabIndex(2);
			panel10.set_BorderStyle((BorderStyle)1);
			((Control)panel10).set_Location(new Point(41, -2));
			((Control)panel10).set_Name("panel10");
			((Control)panel10).set_Size(new Size(2, 38));
			((Control)panel10).set_TabIndex(42);
			lblSelectSep.set_BorderStyle((BorderStyle)1);
			((Control)lblSelectSep).set_Location(new Point(167, 0));
			((Control)lblSelectSep).set_Name("lblSelectSep");
			((Control)lblSelectSep).set_Size(new Size(2, 38));
			((Control)lblSelectSep).set_TabIndex(41);
			((Control)lblNumberOfSolutions).set_AutoSize(true);
			((Control)lblNumberOfSolutions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblNumberOfSolutions).set_ForeColor(Color.Crimson);
			((Control)lblNumberOfSolutions).set_Location(new Point(-2, 4));
			((Control)lblNumberOfSolutions).set_Name("lblNumberOfSolutions");
			((Control)lblNumberOfSolutions).set_Size(new Size(44, 26));
			((Control)lblNumberOfSolutions).set_TabIndex(38);
			((Control)lblNumberOfSolutions).set_Text("S e t \r\nS o l n");
			lblNumberOfSolutions.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)lblSelectSth).set_AutoSize(true);
			((Control)lblSelectSth).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblSelectSth).set_ForeColor(Color.Blue);
			((Control)lblSelectSth).set_Location(new Point(220, 7));
			((Control)lblSelectSth).set_Name("lblSelectSth");
			((Control)lblSelectSth).set_Size(new Size(23, 13));
			((Control)lblSelectSth).set_TabIndex(40);
			((Control)lblSelectSth).set_Text("-S-");
			((Control)lblSelectNth).set_AutoSize(true);
			((Control)lblSelectNth).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblSelectNth).set_ForeColor(Color.Blue);
			((Control)lblSelectNth).set_Location(new Point(89, 7));
			((Control)lblSelectNth).set_Name("lblSelectNth");
			((Control)lblSelectNth).set_Size(new Size(23, 13));
			((Control)lblSelectNth).set_TabIndex(39);
			((Control)lblSelectNth).set_Text("-N-");
			((Control)lblDoubleStar).set_AutoSize(true);
			((Control)lblDoubleStar).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblDoubleStar).set_Location(new Point(50, 6));
			((Control)lblDoubleStar).set_Name("lblDoubleStar");
			((Control)lblDoubleStar).set_Size(new Size(170, 20));
			((Control)lblDoubleStar).set_TabIndex(8);
			((Control)lblDoubleStar).set_Text("Double star solution");
			((Control)Vbar).set_Cursor(Cursors.get_SizeNS());
			((ScrollBar)Vbar).set_LargeChange(1);
			((Control)Vbar).set_Location(new Point(435, 27));
			((ScrollBar)Vbar).set_Maximum(1200);
			((ScrollBar)Vbar).set_Minimum(-1200);
			((Control)Vbar).set_Name("Vbar");
			((Control)Vbar).set_Size(new Size(12, 477));
			((Control)Vbar).set_TabIndex(4);
			((ScrollBar)Vbar).add_Scroll(new ScrollEventHandler(Vbar_Scroll));
			((Control)Hbar).set_Cursor(Cursors.get_SizeWE());
			((ScrollBar)Hbar).set_LargeChange(1);
			((Control)Hbar).set_Location(new Point(14, 502));
			((ScrollBar)Hbar).set_Maximum(1200);
			((ScrollBar)Hbar).set_Minimum(-1200);
			((Control)Hbar).set_Name("Hbar");
			((Control)Hbar).set_Size(new Size(432, 12));
			((Control)Hbar).set_TabIndex(5);
			((ScrollBar)Hbar).add_Scroll(new ScrollEventHandler(Hbar_Scroll));
			((Control)lblTag).set_AutoSize(true);
			((Control)lblTag).set_Location(new Point(585, 7));
			((Control)lblTag).set_Name("lblTag");
			((Control)lblTag).set_Size(new Size(89, 13));
			((Control)lblTag).set_TabIndex(6);
			((Control)lblTag).set_Text("{Observer && time}");
			((Control)cmdMissTimes).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMissTimes).set_Location(new Point(425, 0));
			((Control)cmdMissTimes).set_Name("cmdMissTimes");
			((Control)cmdMissTimes).set_Size(new Size(86, 23));
			((Control)cmdMissTimes).set_TabIndex(7);
			((Control)cmdMissTimes).set_Text("Set 'Miss' Times");
			((ButtonBase)cmdMissTimes).set_UseVisualStyleBackColor(true);
			((Control)cmdMissTimes).add_Click((EventHandler)cmdMissTimes_Click);
			((Control)cmdShowEditor).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdShowEditor).set_Location(new Point(518, 0));
			((Control)cmdShowEditor).set_Name("cmdShowEditor");
			((Control)cmdShowEditor).set_Size(new Size(61, 25));
			((Control)cmdShowEditor).set_TabIndex(8);
			((Control)cmdShowEditor).set_Text("→Editor");
			((ButtonBase)cmdShowEditor).set_UseVisualStyleBackColor(true);
			((Control)cmdShowEditor).add_Click((EventHandler)cmdShowEditor_Click);
			toolTip.set_AutoPopDelay(5000);
			toolTip.set_InitialDelay(100);
			toolTip.set_IsBalloon(true);
			toolTip.set_ReshowDelay(100);
			((ListControl)cmbModelFitQuality).set_FormattingEnabled(true);
			cmbModelFitQuality.get_Items().AddRange(new object[8] { "Not fitted", "Bad occn data", "Model wrong", "Minimum dia", "Dia, but no fit", "Poor fit", "Good fit", "Not constrained" });
			((Control)cmbModelFitQuality).set_Location(new Point(1, 13));
			((Control)cmbModelFitQuality).set_Name("cmbModelFitQuality");
			((Control)cmbModelFitQuality).set_Size(new Size(97, 21));
			((Control)cmbModelFitQuality).set_TabIndex(6);
			toolTip.SetToolTip((Control)(object)cmbModelFitQuality, "Mouse click will select \r\nthe displayed value.");
			cmbModelFitQuality.add_SelectedIndexChanged((EventHandler)cmbModelFitQuality_SelectedIndexChanged);
			((Control)cmbModelFitQuality).add_MouseDown(new MouseEventHandler(cmbModelFitQuality_MouseDown));
			((Control)panelGridAngles).set_BackColor(Color.LightSteelBlue);
			panelGridAngles.set_BorderStyle((BorderStyle)1);
			((Control)panelGridAngles).get_Controls().Add((Control)(object)cmdGridMinus);
			((Control)panelGridAngles).get_Controls().Add((Control)(object)cmdGridPlus);
			((Control)panelGridAngles).get_Controls().Add((Control)(object)label17);
			((Control)panelGridAngles).get_Controls().Add((Control)(object)dropAngles);
			((Control)panelGridAngles).get_Controls().Add((Control)(object)cmdCloseGridAngles);
			((Control)panelGridAngles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelGridAngles).set_Location(new Point(9, 140));
			((Control)panelGridAngles).set_Name("panelGridAngles");
			((Control)panelGridAngles).set_Size(new Size(96, 43));
			((Control)panelGridAngles).set_TabIndex(9);
			((Control)panelGridAngles).set_Visible(false);
			((Control)panelGridAngles).add_MouseDown(new MouseEventHandler(panelGridAngles_MouseDown));
			((Control)panelGridAngles).add_MouseMove(new MouseEventHandler(panelGridAngles_MouseMove));
			((Control)cmdGridMinus).set_BackColor(Color.LightCoral);
			((Control)cmdGridMinus).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdGridMinus).set_Location(new Point(72, 16));
			((Control)cmdGridMinus).set_Name("cmdGridMinus");
			((Control)cmdGridMinus).set_Size(new Size(20, 20));
			((Control)cmdGridMinus).set_TabIndex(4);
			((Control)cmdGridMinus).set_Text("-");
			((ButtonBase)cmdGridMinus).set_UseVisualStyleBackColor(false);
			((Control)cmdGridMinus).add_Click((EventHandler)cmdGridMinus_Click);
			((Control)cmdGridPlus).set_BackColor(Color.LawnGreen);
			((Control)cmdGridPlus).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdGridPlus).set_Location(new Point(53, 17));
			((Control)cmdGridPlus).set_Name("cmdGridPlus");
			((Control)cmdGridPlus).set_Size(new Size(20, 20));
			((Control)cmdGridPlus).set_TabIndex(3);
			((Control)cmdGridPlus).set_Text("+");
			((ButtonBase)cmdGridPlus).set_TextAlign(ContentAlignment.TopRight);
			((ButtonBase)cmdGridPlus).set_UseVisualStyleBackColor(false);
			((Control)cmdGridPlus).add_Click((EventHandler)cmdGridPlus_Click);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_BackColor(Color.Azure);
			((Control)label17).set_Location(new Point(0, 3));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(55, 13));
			((Control)label17).set_TabIndex(2);
			((Control)label17).set_Text("Grid angle");
			dropAngles.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)dropAngles).set_FormattingEnabled(true);
			((Control)dropAngles).set_Location(new Point(3, 17));
			((Control)dropAngles).set_Name("dropAngles");
			((Control)dropAngles).set_Size(new Size(47, 21));
			((Control)dropAngles).set_TabIndex(1);
			dropAngles.add_SelectedIndexChanged((EventHandler)dropAngles_SelectedIndexChanged);
			((ButtonBase)cmdCloseGridAngles).set_Image((Image)Resources.error);
			((Control)cmdCloseGridAngles).set_Location(new Point(72, 1));
			((Control)cmdCloseGridAngles).set_Name("cmdCloseGridAngles");
			((Control)cmdCloseGridAngles).set_Size(new Size(17, 16));
			((Control)cmdCloseGridAngles).set_TabIndex(0);
			((ButtonBase)cmdCloseGridAngles).set_UseVisualStyleBackColor(true);
			((Control)cmdCloseGridAngles).add_Click((EventHandler)cmdCloseGridAngles_Click);
			((Control)panelShapeModelControl).set_BackColor(Color.LightSteelBlue);
			panelShapeModelControl.set_BorderStyle((BorderStyle)1);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)lblQualityExplanation);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)cmdForDiameters);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)cmdShapeHelp);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)label35);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)panel2);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)label27);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)txtSurface_Volume_Ratio);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)cmdGetLightCurves);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)label26);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)label24);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)txtVersion);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)label22);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)txtModelID);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)cmdSaveModelInfo);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)label21);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)txtModelSource);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)label12);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)label11);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)cmdCloseShapeModelControl);
			((Control)panelShapeModelControl).get_Controls().Add((Control)(object)tbarScaleAdjust);
			((Control)panelShapeModelControl).set_Location(new Point(111, 42));
			((Control)panelShapeModelControl).set_Name("panelShapeModelControl");
			((Control)panelShapeModelControl).set_Size(new Size(450, 137));
			((Control)panelShapeModelControl).set_TabIndex(10);
			((Control)panelShapeModelControl).add_MouseDown(new MouseEventHandler(panelShapeModelControl_MouseDown));
			((Control)panelShapeModelControl).add_MouseHover((EventHandler)panelShapeModelControl_MouseHover);
			((Control)panelShapeModelControl).add_MouseMove(new MouseEventHandler(panelShapeModelControl_MouseMove));
			((Control)lblQualityExplanation).set_AutoSize(true);
			((Control)lblQualityExplanation).set_BackColor(Color.LightSteelBlue);
			((Control)lblQualityExplanation).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblQualityExplanation).set_ForeColor(Color.Black);
			((Control)lblQualityExplanation).set_Location(new Point(180, 74));
			((Control)lblQualityExplanation).set_Name("lblQualityExplanation");
			((Control)lblQualityExplanation).set_Size(new Size(131, 13));
			((Control)lblQualityExplanation).set_TabIndex(36);
			((Control)lblQualityExplanation).set_Text("Fit quality explanation");
			lblQualityExplanation.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdForDiameters).set_BackColor(Color.LimeGreen);
			((ButtonBase)cmdForDiameters).set_FlatStyle((FlatStyle)1);
			((Control)cmdForDiameters).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdForDiameters).set_Location(new Point(2, 56));
			((Control)cmdForDiameters).set_Name("cmdForDiameters");
			((Control)cmdForDiameters).set_Size(new Size(34, 21));
			((Control)cmdForDiameters).set_TabIndex(35);
			((Control)cmdForDiameters).set_Text("Dia");
			((ButtonBase)cmdForDiameters).set_UseVisualStyleBackColor(false);
			((Control)cmdForDiameters).add_Click((EventHandler)cmdForDiameters_Click);
			((Control)cmdShapeHelp).set_BackColor(Color.DodgerBlue);
			((Control)cmdShapeHelp).set_Font(new Font("Cascadia Code", 9.75f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdShapeHelp).set_ForeColor(Color.Yellow);
			((ButtonBase)cmdShapeHelp).set_Image((Image)Resources.Question_16x_24);
			((Control)cmdShapeHelp).set_Location(new Point(412, 0));
			((Control)cmdShapeHelp).set_Name("cmdShapeHelp");
			((Control)cmdShapeHelp).set_Size(new Size(18, 21));
			((Control)cmdShapeHelp).set_TabIndex(34);
			((ButtonBase)cmdShapeHelp).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)cmdShapeHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdShapeHelp).add_Click((EventHandler)cmdShapeHelp_Click);
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Enabled(false);
			((Control)label35).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label35).set_ForeColor(Color.Indigo);
			((Control)label35).set_Location(new Point(35, 58));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(104, 13));
			((Control)label35).set_TabIndex(33);
			((Control)label35).set_Text("Values in memory =>");
			((Control)panel2).set_BackColor(Color.LemonChiffon);
			panel2.set_BorderStyle((BorderStyle)2);
			((Control)panel2).get_Controls().Add((Control)(object)label23);
			((Control)panel2).get_Controls().Add((Control)(object)cmdSetModelMax);
			((Control)panel2).get_Controls().Add((Control)(object)cmdSetModelMin);
			((Control)panel2).get_Controls().Add((Control)(object)label20);
			((Control)panel2).get_Controls().Add((Control)(object)label19);
			((Control)panel2).get_Controls().Add((Control)(object)lblCurrentPhase);
			((Control)panel2).get_Controls().Add((Control)(object)txtModelMaxDia);
			((Control)panel2).get_Controls().Add((Control)(object)txtModelPhase);
			((Control)panel2).get_Controls().Add((Control)(object)txtModelMinDia);
			((Control)panel2).get_Controls().Add((Control)(object)cmbModelFitQuality);
			((Control)panel2).get_Controls().Add((Control)(object)lblCurrentFit);
			((Control)panel2).get_Controls().Add((Control)(object)lblCurrentMinDia);
			((Control)panel2).get_Controls().Add((Control)(object)lblCurrentMaxDia);
			((Control)panel2).get_Controls().Add((Control)(object)label25);
			((Control)panel2).set_Location(new Point(140, 22));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(303, 51));
			((Control)panel2).set_TabIndex(32);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(19, -1));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(51, 13));
			((Control)label23).set_TabIndex(15);
			((Control)label23).set_Text("Fit quality");
			((Control)cmdSetModelMax).set_BackColor(Color.GreenYellow);
			((Control)cmdSetModelMax).set_Location(new Point(265, 12));
			((Control)cmdSetModelMax).set_Name("cmdSetModelMax");
			((Control)cmdSetModelMax).set_Size(new Size(33, 26));
			((Control)cmdSetModelMax).set_TabIndex(12);
			((Control)cmdSetModelMax).set_Text("Set");
			((ButtonBase)cmdSetModelMax).set_UseVisualStyleBackColor(false);
			((Control)cmdSetModelMax).add_Click((EventHandler)cmdSetModelMax_Click);
			((Control)cmdSetModelMin).set_BackColor(Color.GreenYellow);
			((Control)cmdSetModelMin).set_Location(new Point(187, 12));
			((Control)cmdSetModelMin).set_Name("cmdSetModelMin");
			((Control)cmdSetModelMin).set_Size(new Size(33, 26));
			((Control)cmdSetModelMin).set_TabIndex(11);
			((Control)cmdSetModelMin).set_Text("Set");
			((ButtonBase)cmdSetModelMin).set_UseVisualStyleBackColor(false);
			((Control)cmdSetModelMin).add_Click((EventHandler)cmdSetModelMin_Click);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(224, 0));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(68, 13));
			((Control)label20).set_TabIndex(10);
			((Control)label20).set_Text("Maximum dia");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Location(new Point(148, -1));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(65, 13));
			((Control)label19).set_TabIndex(9);
			((Control)label19).set_Text("Minimum dia");
			((Control)lblCurrentPhase).set_AutoSize(true);
			((Control)lblCurrentPhase).set_ForeColor(Color.Indigo);
			((Control)lblCurrentPhase).set_Location(new Point(101, 33));
			((Control)lblCurrentPhase).set_Name("lblCurrentPhase");
			((Control)lblCurrentPhase).set_Size(new Size(17, 13));
			((Control)lblCurrentPhase).set_TabIndex(23);
			((Control)lblCurrentPhase).set_Text("0°");
			((Control)txtModelMaxDia).set_BackColor(Color.Pink);
			((Control)txtModelMaxDia).set_Location(new Point(226, 13));
			((Control)txtModelMaxDia).set_Name("txtModelMaxDia");
			((TextBoxBase)txtModelMaxDia).set_ReadOnly(true);
			((Control)txtModelMaxDia).set_Size(new Size(39, 20));
			((Control)txtModelMaxDia).set_TabIndex(8);
			((Control)txtModelPhase).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtModelPhase).set_Location(new Point(102, 13));
			((Control)txtModelPhase).set_Name("txtModelPhase");
			((TextBoxBase)txtModelPhase).set_ReadOnly(true);
			((Control)txtModelPhase).set_Size(new Size(39, 19));
			((Control)txtModelPhase).set_TabIndex(21);
			((Control)txtModelMinDia).set_Location(new Point(148, 13));
			((Control)txtModelMinDia).set_Name("txtModelMinDia");
			((TextBoxBase)txtModelMinDia).set_ReadOnly(true);
			((Control)txtModelMinDia).set_Size(new Size(39, 20));
			((Control)txtModelMinDia).set_TabIndex(7);
			((Control)lblCurrentFit).set_AutoSize(true);
			((Control)lblCurrentFit).set_ForeColor(Color.Indigo);
			((Control)lblCurrentFit).set_Location(new Point(0, 34));
			((Control)lblCurrentFit).set_Name("lblCurrentFit");
			((Control)lblCurrentFit).set_Size(new Size(50, 13));
			((Control)lblCurrentFit).set_TabIndex(24);
			((Control)lblCurrentFit).set_Text("Not fitted");
			((Control)lblCurrentMinDia).set_AutoSize(true);
			((Control)lblCurrentMinDia).set_ForeColor(Color.Indigo);
			((Control)lblCurrentMinDia).set_Location(new Point(147, 34));
			((Control)lblCurrentMinDia).set_Name("lblCurrentMinDia");
			((Control)lblCurrentMinDia).set_Size(new Size(28, 13));
			((Control)lblCurrentMinDia).set_TabIndex(25);
			((Control)lblCurrentMinDia).set_Text("- - - -");
			((Control)lblCurrentMaxDia).set_AutoSize(true);
			((Control)lblCurrentMaxDia).set_ForeColor(Color.Indigo);
			((Control)lblCurrentMaxDia).set_Location(new Point(226, 34));
			((Control)lblCurrentMaxDia).set_Name("lblCurrentMaxDia");
			((Control)lblCurrentMaxDia).set_Size(new Size(28, 13));
			((Control)lblCurrentMaxDia).set_TabIndex(26);
			((Control)lblCurrentMaxDia).set_Text("- - - -");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Location(new Point(97, -1));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(48, 13));
			((Control)label25).set_TabIndex(22);
			((Control)label25).set_Text("Ph Corrn");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Enabled(false);
			((Control)label27).set_Location(new Point(97, 22));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(46, 13));
			((Control)label27).set_TabIndex(31);
			((Control)label27).set_Text("Surf/Vol");
			((Control)txtSurface_Volume_Ratio).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSurface_Volume_Ratio).set_Location(new Point(100, 36));
			((Control)txtSurface_Volume_Ratio).set_Name("txtSurface_Volume_Ratio");
			((TextBoxBase)txtSurface_Volume_Ratio).set_ReadOnly(true);
			((Control)txtSurface_Volume_Ratio).set_Size(new Size(39, 19));
			((Control)txtSurface_Volume_Ratio).set_TabIndex(30);
			((Control)cmdGetLightCurves).set_BackColor(Color.Aqua);
			((Control)cmdGetLightCurves).set_Location(new Point(155, 1));
			((Control)cmdGetLightCurves).set_Name("cmdGetLightCurves");
			((Control)cmdGetLightCurves).set_Size(new Size(74, 22));
			((Control)cmdGetLightCurves).set_TabIndex(29);
			((Control)cmdGetLightCurves).set_Text("Light curves");
			((ButtonBase)cmdGetLightCurves).set_UseVisualStyleBackColor(false);
			((Control)cmdGetLightCurves).add_Click((EventHandler)cmdGetLightCurves_Click);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Enabled(false);
			((Control)label26).set_Location(new Point(3, 111));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(64, 13));
			((Control)label26).set_TabIndex(28);
			((Control)label26).set_Text("Image scale");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Enabled(false);
			((Control)label24).set_Location(new Point(4, 90));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(62, 13));
			((Control)label24).set_TabIndex(20);
			((Control)label24).set_Text("Version info");
			((Control)txtVersion).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtVersion).set_Location(new Point(67, 88));
			((Control)txtVersion).set_Name("txtVersion");
			((TextBoxBase)txtVersion).set_ReadOnly(true);
			((Control)txtVersion).set_Size(new Size(270, 19));
			((Control)txtVersion).set_TabIndex(19);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Enabled(false);
			((Control)label22).set_Location(new Point(63, 22));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(18, 13));
			((Control)label22).set_TabIndex(18);
			((Control)label22).set_Text("ID");
			((Control)txtModelID).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtModelID).set_Location(new Point(45, 36));
			((Control)txtModelID).set_Name("txtModelID");
			((TextBoxBase)txtModelID).set_ReadOnly(true);
			((Control)txtModelID).set_Size(new Size(54, 19));
			((Control)txtModelID).set_TabIndex(17);
			((Control)cmdSaveModelInfo).set_BackColor(Color.Pink);
			((Control)cmdSaveModelInfo).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSaveModelInfo).set_Location(new Point(345, 88));
			((Control)cmdSaveModelInfo).set_Name("cmdSaveModelInfo");
			((Control)cmdSaveModelInfo).set_Size(new Size(100, 46));
			((Control)cmdSaveModelInfo).set_TabIndex(16);
			((Control)cmdSaveModelInfo).set_Text("&Save/update\r\nthis model");
			((ButtonBase)cmdSaveModelInfo).set_UseVisualStyleBackColor(false);
			((Control)cmdSaveModelInfo).add_Click((EventHandler)cmdSaveModelInfo_Click);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Enabled(false);
			((Control)label21).set_Location(new Point(4, 22));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(41, 13));
			((Control)label21).set_TabIndex(13);
			((Control)label21).set_Text("Source");
			((Control)txtModelSource).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtModelSource).set_Location(new Point(4, 36));
			((Control)txtModelSource).set_Name("txtModelSource");
			((TextBoxBase)txtModelSource).set_ReadOnly(true);
			((Control)txtModelSource).set_Size(new Size(40, 19));
			((Control)txtModelSource).set_TabIndex(5);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Enabled(false);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(230, 6));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(37, 13));
			((Control)label12).set_TabIndex(4);
			((Control)label12).set_Text("ISAM");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Enabled(false);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(-2, 6));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(46, 13));
			((Control)label11).set_TabIndex(3);
			((Control)label11).set_Text("DAMIT");
			((Control)cmdCloseShapeModelControl).set_BackColor(Color.Crimson);
			((Control)cmdCloseShapeModelControl).set_Font(new Font("Candara", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCloseShapeModelControl).set_ForeColor(Color.White);
			((Control)cmdCloseShapeModelControl).set_Location(new Point(428, 0));
			((Control)cmdCloseShapeModelControl).set_Name("cmdCloseShapeModelControl");
			((Control)cmdCloseShapeModelControl).set_Size(new Size(19, 21));
			((Control)cmdCloseShapeModelControl).set_TabIndex(0);
			((Control)cmdCloseShapeModelControl).set_Text("X");
			((ButtonBase)cmdCloseShapeModelControl).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)cmdCloseShapeModelControl).set_UseVisualStyleBackColor(false);
			((Control)cmdCloseShapeModelControl).add_Click((EventHandler)cmdCloseShapeModelControl_Click);
			((Control)tbarScaleAdjust).set_AutoSize(false);
			((Control)tbarScaleAdjust).set_BackColor(Color.LemonChiffon);
			tbarScaleAdjust.set_LargeChange(1);
			((Control)tbarScaleAdjust).set_Location(new Point(68, 109));
			tbarScaleAdjust.set_Maximum(150);
			tbarScaleAdjust.set_Minimum(25);
			((Control)tbarScaleAdjust).set_Name("tbarScaleAdjust");
			((Control)tbarScaleAdjust).set_Size(new Size(269, 22));
			((Control)tbarScaleAdjust).set_TabIndex(1);
			tbarScaleAdjust.set_TickFrequency(3);
			tbarScaleAdjust.set_TickStyle((TickStyle)1);
			tbarScaleAdjust.set_Value(90);
			tbarScaleAdjust.add_Scroll((EventHandler)tbarScaleAdjust_Scroll);
			((Control)picPlot).set_BackColor(Color.Black);
			picPlot.set_BorderStyle((BorderStyle)2);
			((Control)picPlot).set_Location(new Point(1, 27));
			((Control)picPlot).set_Name("picPlot");
			((Control)picPlot).set_Size(new Size(432, 477));
			picPlot.set_TabIndex(0);
			picPlot.set_TabStop(false);
			((Control)picPlot).add_MouseMove(new MouseEventHandler(picPlot_MouseMove));
			((Control)picPlot).add_MouseUp(new MouseEventHandler(picPlot_MouseUp));
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(802, 515));
			((Control)this).get_Controls().Add((Control)(object)panelDouble);
			((Control)this).get_Controls().Add((Control)(object)PanelSatellites);
			((Control)this).get_Controls().Add((Control)(object)panelShapeModelControl);
			((Control)this).get_Controls().Add((Control)(object)panelGridAngles);
			((Control)this).get_Controls().Add((Control)(object)Vbar);
			((Control)this).get_Controls().Add((Control)(object)Hbar);
			((Control)this).get_Controls().Add((Control)(object)cmdShowEditor);
			((Control)this).get_Controls().Add((Control)(object)cmdMissTimes);
			((Control)this).get_Controls().Add((Control)(object)lblTag);
			((Control)this).get_Controls().Add((Control)(object)grpBestFit);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)picPlot);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterPlotObs", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterPlotObs);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("PlotObservations");
			((Control)this).set_Text("Plot event observations");
			((Form)this).add_Activated((EventHandler)PlotObservations_Activated);
			((Form)this).add_Deactivate((EventHandler)PlotObservations_Deactivate);
			((Form)this).add_FormClosing(new FormClosingEventHandler(PlotObservations_FormClosing));
			((Form)this).add_Load((EventHandler)PlotObservations_Load);
			((Form)this).add_ResizeBegin((EventHandler)PlotObservations_ResizeBegin);
			((Form)this).add_ResizeEnd((EventHandler)PlotObservations_ResizeEnd);
			((Control)this).add_Enter((EventHandler)PlotObservations_Enter);
			((Control)this).add_Resize((EventHandler)PlotObservations_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)grpBestFit).ResumeLayout(false);
			((Control)grpBestFit).PerformLayout();
			((Control)panelLimbFit).ResumeLayout(false);
			((Control)panelLimbFit).PerformLayout();
			((ISupportInitialize)trackLimbFit).EndInit();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((ISupportInitialize)trackOpacity).EndInit();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((ISupportInitialize)SliderScale).EndInit();
			((ISupportInitialize)updnCenterOfMass_Y).EndInit();
			((ISupportInitialize)updnCenterOfMass_X).EndInit();
			((Control)pnlDouble).ResumeLayout(false);
			((Control)pnlDouble).PerformLayout();
			((Control)PanelLegend).ResumeLayout(false);
			((ISupportInitialize)picLegend).EndInit();
			((ISupportInitialize)updnY).EndInit();
			((ISupportInitialize)updnA).EndInit();
			((ISupportInitialize)updnB).EndInit();
			((ISupportInitialize)updnPA).EndInit();
			((ISupportInitialize)updnX).EndInit();
			((Control)PanelSatellites).ResumeLayout(false);
			((Control)PanelSatellites).PerformLayout();
			((ISupportInitialize)picDrag).EndInit();
			((Control)pnlCBET).ResumeLayout(false);
			((Control)pnlCBET).PerformLayout();
			((Control)pnlNames).ResumeLayout(false);
			((Control)pnlNames).PerformLayout();
			((ISupportInitialize)updnSatNum).EndInit();
			((Control)PanelMiriade).ResumeLayout(false);
			((Control)PanelMiriade).PerformLayout();
			((Control)panelDouble).ResumeLayout(false);
			((Control)panelDouble).PerformLayout();
			((ISupportInitialize)picDragDouble).EndInit();
			((Control)panelJDSO).ResumeLayout(false);
			((Control)panelJDSO).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panel7).ResumeLayout(false);
			((Control)panel7).PerformLayout();
			((ISupportInitialize)updnBrightRatio).EndInit();
			((ISupportInitialize)updnBrightnessUncertPerCent).EndInit();
			((Control)panelSolve).ResumeLayout(false);
			((Control)panelSolve).PerformLayout();
			((Control)pnlOffset).ResumeLayout(false);
			((Control)pnlOffset).PerformLayout();
			((Control)panel9).ResumeLayout(false);
			((Control)panel9).PerformLayout();
			((Control)panel8).ResumeLayout(false);
			((Control)panel8).PerformLayout();
			((Control)panel6).ResumeLayout(false);
			((Control)panel6).PerformLayout();
			((Control)pnlDoubleStarSolution).ResumeLayout(false);
			((Control)pnlDoubleStarSolution).PerformLayout();
			((Control)panelGridAngles).ResumeLayout(false);
			((Control)panelGridAngles).PerformLayout();
			((Control)panelShapeModelControl).ResumeLayout(false);
			((Control)panelShapeModelControl).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((ISupportInitialize)tbarScaleAdjust).EndInit();
			((ISupportInitialize)picPlot).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
