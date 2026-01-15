using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Net;
using System.Text.RegularExpressions;
using System.Windows.Forms;
using GifCreator;
using Occult.Properties;
using Shapes;

namespace Occult.Asteroid_Observations
{
	public class DisplayShapeModels : Form
	{
		internal int CurrentImage;

		internal int CurrentPhaseImage;

		internal int ZeroTimeAdjustSetting;

		internal static List<PictureBox> Images = new List<PictureBox>();

		internal static List<PictureBox> PhaseImages = new List<PictureBox>();

		internal static double[] VolumeEquivalent_Diameter_Pixels = new double[12];

		internal static double[] SurfaceEquivalent_Diameter_Pixels = new double[12];

		internal static string[] ModelID = new string[12]
		{
			"0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
			"0", "0"
		};

		internal static bool PlotOnEarthPlane = false;

		internal static Model_Parameters[] ModelParameters = new Model_Parameters[12];

		internal static float ImageSize = 600f;

		internal int[] DamitEquivalent = new int[6] { -1, -1, -1, -1, -1, -1 };

		internal int[] ISAMEquivalent = new int[6] { -1, -1, -1, -1, -1, -1 };

		internal int NumDAMITImages;

		private static double JDforImage = 0.0;

		private static double EventJD = 0.0;

		internal static bool IncludeAxisOfRotation = false;

		private static bool IncludePhaseEffects = false;

		private static bool DrawDAMITMean = false;

		private static bool DrawDamitID = true;

		private static bool InvertImage = false;

		private static int BlackToWhiteBackground = 0;

		internal static Color ShapeModelGrayBackColor = Color.FromArgb(255, 230, 245, 245);

		private static bool BlackWhiteModel = false;

		private static bool PlotModelDark = false;

		private static bool ShowModelFaceEdges = false;

		private int LastDisplayedImage;

		internal static int currently_Displayed_Model = 0;

		internal int CurrentModel = Currently_Displayed_Model;

		private static int CurrentlySelectedAsteroid = 0;

		private static int CurrentlySelectedAsteroid_DAMIT_ID = 0;

		private static int[] CurrentlySelected_DAMITModelNumbers = new int[6];

		private static double PhaseStep = 0.1;

		private static double CurrentPhaseOffset = 0.0;

		internal int NumPhaseImages = 10;

		internal static int dLambda = 0;

		internal static int dBeta = 0;

		private const int MidPointOfPhases = 4;

		internal static string DAMIT_AsteroidID = "";

		private static string PlotLabel = "";

		internal static bool IsTransferring = false;

		private bool CancelPhaseDownloads;

		private static bool IsInitialising = true;

		private int TimerCount;

		private int AnimationInterval = 1000;

		internal float AsteroidDiameterByVol_AsFractionOfPlotWidth;

		internal float AsteroidDiameterBySurf_AsFractionOfPlotWidth;

		internal Label[,] lblDownloadStatus = new Label[6, 2];

		internal Label[,] lblPeriod = new Label[6, 2];

		internal Button[,] cmdGetModel = new Button[6, 2];

		internal Label[] lblPhaseDownloadStatus = (Label[])(object)new Label[10];

		internal Label[] lblPhaseOffset = (Label[])(object)new Label[10];

		internal Button[] cmdPhaseModel = (Button[])(object)new Button[10];

		private DisplayData DataBox;

		private static DisplayImage ImageDisplay;

		private IContainer components;

		private Label label6;

		internal TextBox txtMin;

		internal TextBox txtHrs;

		private Label label5;

		private Label label3;

		private Label label2;

		private Label label1;

		internal TextBox txtDay;

		internal TextBox txtMonth;

		internal TextBox txtYear;

		private MenuStrip menuStrip1;

		private Label lblAvailableAsteroids;

		private Label label7;

		internal PictureBox picDisplay;

		private Label lblModel;

		private Label label8;

		private Label label9;

		private Label label10;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label lblSelect;

		private ToolStripMenuItem withImageToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		internal TrackBar trackBarImageSize;

		internal Label lblPhaseAngle;

		private Label label16;

		internal TextBox txtDeltaT;

		private Label label17;

		private ToolStripMenuItem keepOnTopToolStripMenuItem;

		private ToolStripMenuItem IncludeAxisToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem dAMITIncludeMeanRadiusOnImageToolStripMenuItem;

		private Button btnCancel;

		internal Panel pnlPhaseChange;

		private ToolStripMenuItem listDAMITToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private Button cmdCancel;

		internal Button cmdDownloadModels;

		private Label lblJD;

		private ToolStripMenuItem copyEventJDToolStripMenuItem;

		private Timer timerPhase;

		private Button cmdCancelAnimation;

		private ToolStripMenuItem phaseImagesToolStripMenuItem;

		private ToolStripMenuItem downloadPhaseAt1036StepsToolStripMenuItem;

		private ToolStripMenuItem downloadPhaseAt518StepsToolStripMenuItem;

		private ToolStripMenuItem downloadPhaseAt272StepsToolStripMenuItem;

		private ToolStripMenuItem downloadPhaseAt136StepsToolStripMenuItem;

		private ToolStripMenuItem downloadPhaseAt062StepsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator6;

		private ToolStripMenuItem saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem;

		private ToolStripMenuItem animateDisplayOfPhaseModelsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator7;

		private ToolStripMenuItem animatedGIFSetupToolStripMenuItem;

		private ToolStripMenuItem saveAnimatedGIFOfPhaseModelsToolStripMenuItem;

		private ToolStripMenuItem setAnimationIntervalToolStripMenuItem;

		private ToolStripMenuItem secToolStripMenuItem01;

		private ToolStripMenuItem secToolStripMenuItem027;

		private ToolStripMenuItem secToolStripMenuItem03;

		private ToolStripMenuItem secToolStripMenuItem10;

		private ToolStripMenuItem secToolStripMenuItem20;

		private ToolStripMenuItem secToolStripMenuItem05;

		private ToolStripMenuItem closePhaseModelSelectorToolStripMenuItem;

		private ToolStripMenuItem listSatelliteIRDiametersToolStripMenuItem;

		private ToolStripMenuItem showDAMITFitsToLightCurvesToolStripMenuItem;

		private Panel pnlAdjust;

		private Label label24;

		private Label label23;

		private Label label22;

		private Label label25;

		internal TextBox txtPhaseOffset;

		internal NumericUpDown updnRotnRate;

		internal NumericUpDown updnZeroPhase;

		private Label label11;

		private Label lblDAMITisam;

		internal ToolStripMenuItem plotOnEarthPlaneToolStripMenuItem;

		private ToolStripMenuItem secToolStripMenuItem30;

		private ToolStripMenuItem secToolStripMenuItem40;

		private ToolStripMenuItem showSolarIlluminationPhaseISAMOnlyToolStripMenuItem;

		private ToolStripMenuItem shapeModelMaintenanceToolStripMenuItem;

		private ToolStripMenuItem dAMITToolStripMenuItem;

		private ToolStripMenuItem updateFileOfAvailableModelsToolStripMenuItem;

		private ToolStripMenuItem downloadAllAvailableModelsToolStripMenuItem;

		private ToolStripMenuItem iSAMToolStripMenuItem;

		private ToolStripMenuItem updateListOfISAMShapeModelsToolStripMenuItem;

		private ToolStripMenuItem plotModelInGreyScale;

		private ToolStripMenuItem showFaceEdgesOnModelToolStripMenuItem;

		private ToolStripMenuItem changePolarAxisOrientationToolStripMenuItem;

		private ToolStripMenuItem lambdaToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem4;

		private ToolStripMenuItem toolStripMenuItem2;

		private ToolStripMenuItem toolStripMenuItem3;

		private ToolStripMenuItem toolStripMenuItem5;

		private ToolStripMenuItem toolStripMenuItem6;

		private ToolStripMenuItem toolStripMenuItem7;

		private ToolStripMenuItem toolStripMenuItem8;

		private ToolStripSeparator toolStripSeparator3;

		private ToolStripMenuItem betaToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem9;

		private ToolStripMenuItem toolStripMenuItem10;

		private ToolStripMenuItem toolStripMenuItem11;

		private ToolStripMenuItem toolStripMenuItem12;

		private ToolStripMenuItem toolStripMenuItem13;

		private ToolStripMenuItem toolStripMenuItem14;

		private ToolStripMenuItem toolStripMenuItem15;

		private ToolStripSeparator toolStripSeparator5;

		internal ListBox lstModels;

		private ToolStripMenuItem dwnloadAllmissingShapeModelsToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem16;

		private ToolStripMenuItem toolStripMenuItem17;

		private ToolStripMenuItem toolStripMenuItem18;

		private ToolStripMenuItem toolStripMenuItem19;

		private ToolStripMenuItem convertASingleDAMITSourceFileToolStripMenuItem;

		private Label label4;

		private Panel panel1;

		internal TrackBar trackOpacity;

		private Label label20;

		private Label label13;

		private Label label14;

		private Label label15;

		private Label label18;

		private Panel panel2;

		private ToolStripMenuItem plotModelDarkToolStripMenuItem;

		private ToolStripMenuItem drawPhaseModelsAt16StepsToolStripMenuItem;

		private ToolStripMenuItem getAsteroidFromDAMITToolStripMenuItem;

		internal ToolStripMenuItem mnuWhiteBackground;

		internal ToolStripMenuItem mnuBlackBackground;

		internal ToolStripMenuItem mnuGrayBackground;

		private ToolStripMenuItem showAsteroidAndModelIDOnImageToolStripMenuItem;

		internal static int Currently_Displayed_Model
		{
			get
			{
				return currently_Displayed_Model;
			}
			set
			{
				currently_Displayed_Model = value;
			}
		}

		public DisplayShapeModels()
		{
			InitializeComponent();
		}

		private void DisplayShapeModels_Load(object sender, EventArgs e)
		{
			//IL_0084: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Unknown result type (might be due to invalid IL or missing references)
			//IL_0099: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_00de: Expected O, but got Unknown
			//IL_016f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0174: Unknown result type (might be due to invalid IL or missing references)
			//IL_0184: Unknown result type (might be due to invalid IL or missing references)
			//IL_0191: Unknown result type (might be due to invalid IL or missing references)
			//IL_0199: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c3: Expected O, but got Unknown
			//IL_0224: Unknown result type (might be due to invalid IL or missing references)
			//IL_0229: Unknown result type (might be due to invalid IL or missing references)
			//IL_0239: Unknown result type (might be due to invalid IL or missing references)
			//IL_0246: Unknown result type (might be due to invalid IL or missing references)
			//IL_024e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0256: Unknown result type (might be due to invalid IL or missing references)
			//IL_025d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0272: Unknown result type (might be due to invalid IL or missing references)
			//IL_027d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0288: Unknown result type (might be due to invalid IL or missing references)
			//IL_0294: Expected O, but got Unknown
			//IL_02d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0305: Unknown result type (might be due to invalid IL or missing references)
			//IL_030d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0315: Unknown result type (might be due to invalid IL or missing references)
			//IL_031c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0323: Unknown result type (might be due to invalid IL or missing references)
			//IL_0338: Unknown result type (might be due to invalid IL or missing references)
			//IL_0343: Unknown result type (might be due to invalid IL or missing references)
			//IL_0351: Expected O, but got Unknown
			//IL_038c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0391: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_03be: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0401: Unknown result type (might be due to invalid IL or missing references)
			//IL_0409: Expected O, but got Unknown
			//IL_042a: Unknown result type (might be due to invalid IL or missing references)
			//IL_042f: Unknown result type (might be due to invalid IL or missing references)
			//IL_043e: Unknown result type (might be due to invalid IL or missing references)
			//IL_045d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0465: Unknown result type (might be due to invalid IL or missing references)
			//IL_046d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0474: Unknown result type (might be due to invalid IL or missing references)
			//IL_0489: Unknown result type (might be due to invalid IL or missing references)
			//IL_0494: Unknown result type (might be due to invalid IL or missing references)
			//IL_049f: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a7: Expected O, but got Unknown
			IsInitialising = true;
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			Button obj = cmdCancelAnimation;
			Point location;
			((Control)cmdCancel).set_Location(location = ((Control)cmdDownloadModels).get_Location());
			((Control)obj).set_Location(location);
			((Control)txtDeltaT).set_Text("0");
			for (int i = 0; i < 12; i++)
			{
				ModelParameters[i] = new Model_Parameters();
			}
			for (int j = 0; j < 2; j++)
			{
				for (int k = 0; k < 6; k++)
				{
					Button[,] array = cmdGetModel;
					int num = k;
					int num2 = j;
					Button val = new Button();
					((Control)val).set_Left(290 + k * 55);
					((Control)val).set_Top(52 + 52 * j);
					((Control)val).set_Width(51);
					((Control)val).set_Height(23);
					((Control)val).set_Visible(true);
					((Control)val).set_Enabled(true);
					((Control)val).set_Font(new Font("Microsoft Sans Serif", 7f));
					array[num, num2] = val;
					if (j == 0)
					{
						((Control)cmdGetModel[k, j]).set_Text("...");
					}
					else
					{
						((Control)cmdGetModel[k, j]).set_Text("...");
					}
					((Control)cmdGetModel[k, j]).set_Tag((object)(k + 6 * j).ToString());
					((Control)this).get_Controls().Add((Control)(object)cmdGetModel[k, j]);
					((Control)cmdGetModel[k, j]).add_Click((EventHandler)DisplayModels);
					Label[,] array2 = lblDownloadStatus;
					int num3 = k;
					int num4 = j;
					Label val2 = new Label();
					((Control)val2).set_Left(290 + k * 55);
					((Control)val2).set_Top(39 + 52 * j);
					((Control)val2).set_Width(74);
					((Control)val2).set_Height(31);
					((Control)val2).set_AutoSize(true);
					((Control)val2).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold));
					array2[num3, num4] = val2;
					((Control)lblDownloadStatus[k, j]).set_ForeColor(Color.DarkBlue);
					((Control)lblDownloadStatus[k, j]).set_Text("- - -");
					((Control)lblDownloadStatus[k, j]).set_Visible(true);
					((Control)this).get_Controls().Add((Control)(object)lblDownloadStatus[k, j]);
					Label[,] array3 = lblPeriod;
					int num5 = k;
					int num6 = j;
					Label val3 = new Label();
					((Control)val3).set_Left(290 + k * 55);
					((Control)val3).set_Top(73 + 52 * j);
					((Control)val3).set_Width(74);
					((Control)val3).set_Height(31);
					((Control)val3).set_AutoSize(true);
					((Control)val3).set_Font(new Font("Microsoft Sans Serif", 7f));
					((Control)val3).set_ForeColor(Color.Teal);
					((Control)val3).set_Text("P=0hrs");
					((Control)val3).set_Visible(true);
					array3[num5, num6] = val3;
					((Control)this).get_Controls().Add((Control)(object)lblPeriod[k, j]);
				}
			}
			for (int l = 0; l < 10; l++)
			{
				Button[] array4 = cmdPhaseModel;
				int num7 = l;
				Button val4 = new Button();
				((Control)val4).set_Left(2 + l % 5 * 61);
				((Control)val4).set_Top(14 + 50 * (int)Math.Floor((double)l / 5.0));
				((Control)val4).set_Width(58);
				((Control)val4).set_Height(23);
				((Control)val4).set_Visible(true);
				((Control)val4).set_Enabled(true);
				((Control)val4).set_Font(new Font("Microsoft Sans Serif", 7f));
				((Control)val4).set_Text("Phase");
				((Control)val4).set_Tag((object)l.ToString());
				array4[num7] = val4;
				((Control)pnlPhaseChange).get_Controls().Add((Control)(object)cmdPhaseModel[l]);
				((Control)cmdPhaseModel[l]).add_Click((EventHandler)DisplayPhaseModels);
				Label[] array5 = lblPhaseDownloadStatus;
				int num8 = l;
				Label val5 = new Label();
				((Control)val5).set_Left(2 + l % 5 * 61);
				((Control)val5).set_Top(1 + 50 * (int)Math.Floor((double)l / 5.0));
				((Control)val5).set_Width(58);
				((Control)val5).set_Height(31);
				((Control)val5).set_AutoSize(true);
				((Control)val5).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold));
				((Control)val5).set_ForeColor(Color.DarkBlue);
				((Control)val5).set_Text("  None");
				((Control)val5).set_Visible(true);
				array5[num8] = val5;
				((Control)pnlPhaseChange).get_Controls().Add((Control)(object)lblPhaseDownloadStatus[l]);
				Label[] array6 = lblPhaseOffset;
				int num9 = l;
				Label val6 = new Label();
				((Control)val6).set_Left(6 + l % 5 * 61);
				((Control)val6).set_Top(35 + 50 * (int)Math.Floor((double)l / 5.0));
				((Control)val6).set_Width(58);
				((Control)val6).set_Height(31);
				((Control)val6).set_AutoSize(true);
				((Control)val6).set_Font(new Font("Microsoft Sans Serif", 6.5f));
				((Control)val6).set_ForeColor(Color.Teal);
				((Control)val6).set_Text("0.0h");
				((Control)val6).set_Visible(true);
				array6[num9] = val6;
				((Control)pnlPhaseChange).get_Controls().Add((Control)(object)lblPhaseOffset[l]);
			}
			((Form)this).set_TopMost(Settings.Default.ShapeModel_TopMost);
			if (((Form)this).get_TopMost())
			{
				((ToolStripItem)keepOnTopToolStripMenuItem).set_Text("Return form to &Normal");
			}
			else
			{
				((ToolStripItem)keepOnTopToolStripMenuItem).set_Text("Keep form on &Top");
			}
			ToolStripMenuItem obj2 = phaseImagesToolStripMenuItem;
			bool enabled;
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem).set_Enabled(enabled = false);
			((ToolStripItem)obj2).set_Enabled(enabled);
			NumericUpDown obj3 = updnRotnRate;
			NumericUpDown obj4 = updnZeroPhase;
			decimal value = default(decimal);
			obj4.set_Value(value);
			obj3.set_Value(value);
			((Control)lblPhaseAngle).set_Text("");
			if (((Control)txtYear).get_Text() == "1000")
			{
				((Control)txtYear).set_Text(DateTime.UtcNow.Year.ToString());
				((Control)txtMonth).set_Text(DateTime.UtcNow.Month.ToString());
				((Control)txtDay).set_Text(DateTime.UtcNow.Day.ToString());
				((Control)txtHrs).set_Text(DateTime.UtcNow.Hour.ToString());
				((Control)txtMin).set_Text(DateTime.UtcNow.Minute.ToString());
			}
			mnuBlackBackground.set_Checked(Settings.Default.ShapeModelBackColor == 0);
			mnuWhiteBackground.set_Checked(Settings.Default.ShapeModelBackColor == 1);
			mnuGrayBackground.set_Checked(Settings.Default.ShapeModelBackColor == 2);
			BlackToWhiteBackground = Settings.Default.ShapeModelBackColor;
			Initialise_ShapeModels();
			SetAnimationInterval(Settings.Default.AnimationInterval);
			IncludeAxisOfRotation = Settings.Default.IncludeAxisOfRotation;
			IncludeAxisToolStripMenuItem.set_Checked(IncludeAxisOfRotation);
			DrawDAMITMean = Settings.Default.ShapeModel_DrawMean;
			dAMITIncludeMeanRadiusOnImageToolStripMenuItem.set_Checked(DrawDAMITMean);
			DrawDamitID = Settings.Default.DrawDamitID;
			showAsteroidAndModelIDOnImageToolStripMenuItem.set_Checked(DrawDamitID);
			IncludePhaseEffects = Settings.Default.ISAMshowPhase;
			showSolarIlluminationPhaseISAMOnlyToolStripMenuItem.set_Checked(IncludePhaseEffects);
			BlackWhiteModel = Settings.Default.ModelinBlackWhite;
			plotModelInGreyScale.set_Checked(BlackWhiteModel);
			PlotModelDark = Settings.Default.ModelDark;
			plotModelDarkToolStripMenuItem.set_Checked(PlotModelDark);
			ShowModelFaceEdges = Settings.Default.ModelFaceEdges;
			showFaceEdgesOnModelToolStripMenuItem.set_Checked(ShowModelFaceEdges);
			IsInitialising = false;
			((ListControl)lstModels).set_SelectedIndex(0);
			GetModelsForSelectedAsteroid();
		}

		private void Initialise_ShapeModels()
		{
			if ((((Control)this).get_Left() + ((Control)this).get_Width() <= Screen.GetWorkingArea((Control)(object)this).Left) | (((Control)this).get_Right() >= Screen.GetWorkingArea((Control)(object)this).Right) | (((Control)this).get_Bottom() <= 0) | (((Control)this).get_Top() >= Screen.GetWorkingArea((Control)(object)this).Bottom))
			{
				((Control)this).set_Left((Screen.GetWorkingArea((Control)(object)this).Width - ((Control)this).get_Width()) / 2);
				((Control)this).set_Top((Screen.GetWorkingArea((Control)(object)this).Height - ((Control)this).get_Height()) / 2);
			}
			InitialiseShapeModels();
			((Control)lstModels).set_Height(((Control)this).get_Height() - 190);
			for (int i = 0; i < GetShapeModelData.ShapeModelList.Count; i++)
			{
				lstModels.get_Items().Add((object)GetShapeModelData.ShapeModelList[i]);
			}
			((ListControl)lstModels).set_SelectedIndex(0);
			((Control)lblAvailableAsteroids).set_Text("Models for " + GetShapeModelData.ShapeModelList.Count + " asteroids");
		}

		internal static void InitialiseShapeModels()
		{
			//IL_017a: Unknown result type (might be due to invalid IL or missing references)
			int num = 0;
			bool flag = false;
			GetShapeModelData.InitialiseDAMITShapeModels();
			GetShapeModelData.FillDAMITAsteroidModels();
			Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 0;
			GetShapeModelData.DAMITModelsByAsteroidNumber.Sort();
			GetShapeModelData.Initialise_ISAM_ShapeModels();
			GetShapeModelData.Fill_ISAM_AsteroidModels();
			GetShapeModelData.ShapeModelList.Clear();
			ShapeModelIndex shapeModelIndex = new ShapeModelIndex();
			int num2 = 0;
			int num3 = 0;
			bool flag2 = true;
			do
			{
				if (GetShapeModelData.DAMITModelsByAsteroidNumber[num2].AsteroidNumber != shapeModelIndex.Asteroid_number && num2 > 0)
				{
					GetShapeModelData.ShapeModelList.Add(shapeModelIndex);
					flag2 = true;
				}
				int result;
				if (flag2)
				{
					shapeModelIndex = new ShapeModelIndex
					{
						Asteroid_number = GetShapeModelData.DAMITModelsByAsteroidNumber[num2].AsteroidNumber,
						DAMITModelCount = 1
					};
					int.TryParse(GetShapeModelData.DAMITModelsByAsteroidNumber[num2].ModelNumber_String, out result);
					shapeModelIndex.DAMIT_id[0] = result;
					shapeModelIndex.Asteroid_id = int.Parse(GetShapeModelData.DAMITModelsByAsteroidNumber[num2].DAMIT_Asteroid_ID);
					flag2 = false;
				}
				else
				{
					shapeModelIndex.DAMITModelCount++;
					int.TryParse(GetShapeModelData.DAMITModelsByAsteroidNumber[num2].ModelNumber_String, out result);
					shapeModelIndex.DAMIT_id[shapeModelIndex.DAMITModelCount - 1] = result;
					num3++;
				}
				num2++;
			}
			while (num2 < GetShapeModelData.DAMITModelsByAsteroidNumber.Count);
			GetShapeModelData.ShapeModelList.Add(shapeModelIndex);
			GetShapeModelData.ShapeModelList.Sort();
			if (!File.Exists(Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv"))
			{
				MessageBox.Show("You need a file that identifies those asteroids which have ISAM shape models.\r\n\r\nThere are two options\r\n  1. Create the file using the menu item Shape Model Maintenance, ISAM, Update list of ISAM shape models\r\n  2. Download a recent version of the file from the Maintenance tab, General downloads, download #25 ISAM asteroids\r\n\r\nOption 1 is much slower than option 2, but ensures the data is fully up-to-date", "Missing ISAM file", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\ShapeModels\\ISAM_AvailableAsteroids.csv");
			streamReader.ReadLine();
			do
			{
				string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
				if (array.Length < 2)
				{
					break;
				}
				int num4 = int.Parse(array[0]);
				if (GetShapeModelData.ShapeModelList.Count > 0)
				{
					int num5 = 0;
					int num6 = GetShapeModelData.ShapeModelList.Count - 1;
					flag = false;
					do
					{
						num = (num6 + num5) / 2;
						if (GetShapeModelData.ShapeModelList[num].Asteroid_number == num4)
						{
							flag = true;
							break;
						}
						if (GetShapeModelData.ShapeModelList[num].Asteroid_number > num4)
						{
							num6 = num - 1;
						}
						else
						{
							num5 = num + 1;
						}
					}
					while (num5 <= num6);
				}
				if (flag)
				{
					for (int i = 2; i < array.Length; i += 2)
					{
						GetShapeModelData.ShapeModelList[num].ISAM_id[i / 2 - 1] = int.Parse(array[i]);
						GetShapeModelData.ShapeModelList[num].ISAM_Version[i / 2 - 1] = array[i + 1];
					}
					GetShapeModelData.ShapeModelList[num].ISAMmodelCount = array.Length / 2 - 1;
					continue;
				}
				shapeModelIndex = new ShapeModelIndex
				{
					Asteroid_id = 0,
					Asteroid_number = num4
				};
				for (int j = 0; j < 6; j++)
				{
					shapeModelIndex.DAMIT_id[j] = 0;
				}
				for (int k = 2; k < array.Length; k += 2)
				{
					shapeModelIndex.ISAM_id[k / 2 - 1] = int.Parse(array[k]);
					shapeModelIndex.ISAM_Version[k / 2 - 1] = array[k + 1];
				}
				shapeModelIndex.ISAMmodelCount = array.Length / 2 - 1;
				GetShapeModelData.ShapeModelList.Add(shapeModelIndex);
			}
			while (!streamReader.EndOfStream);
			GetShapeModelData.ShapeModelList.Sort();
		}

		private void DisplayModels(object sender, EventArgs e)
		{
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			if (Data_and_Plots.PlotForm != null)
			{
				Button cmdSetModelMax = Data_and_Plots.PlotForm.cmdSetModelMax;
				bool enabled;
				((Control)Data_and_Plots.PlotForm.cmdSetModelMin).set_Enabled(enabled = true);
				((Control)cmdSetModelMax).set_Enabled(enabled);
			}
			Currently_Displayed_Model = 0;
			for (int i = 0; i < 2; i++)
			{
				for (int j = 0; j < 6; j++)
				{
					if (!(((Control)(Button)sender).get_Tag().ToString() == (j + 6 * i).ToString()))
					{
						continue;
					}
					if (((Control)lblDownloadStatus[j, i]).get_Text() != "Yes")
					{
						return;
					}
					LastDisplayedImage = j;
					Currently_Displayed_Model = j + 6 * i;
					Compute_Phase_Adjustment(j + 6 * i);
					try
					{
						((Control)Data_and_Plots.PlotForm.cmdGetLightCurves).set_Enabled(Currently_Displayed_Model < 6);
					}
					catch
					{
					}
					if (i == 0)
					{
						TransferImage(j);
						break;
					}
					TransferImage(j + NumDAMITImages);
					LastDisplayedImage = j + NumDAMITImages;
					break;
				}
			}
			if (Data_and_Plots.PlotForm != null)
			{
				((Control)Data_and_Plots.PlotForm.cmdSaveModelInfo).set_Text("Save/Update model " + ((Control)Data_and_Plots.PlotForm.txtModelID).get_Text());
				Data_and_Plots.PlotForm.SetTransferButtonColors(Currently_Displayed_Model);
				((Control)Data_and_Plots.PlotForm.cmdSaveModelInfo).set_BackColor(Color.Pink);
				for (int k = 0; k < 6; k++)
				{
					((Control)Data_and_Plots.PlotForm.cmdTransferImage[k]).set_Enabled(((Control)cmdGetModel[k, 0]).get_Enabled());
					((Control)Data_and_Plots.PlotForm.cmdTransferImage[k + 6]).set_Enabled(((Control)cmdGetModel[k, 1]).get_Enabled());
				}
			}
		}

		private void DisplayPhaseModels(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			for (int i = 0; i < NumPhaseImages; i++)
			{
				if (((Control)(Button)sender).get_Tag().ToString()!.Contains(i.ToString()))
				{
					if (((Control)lblPhaseDownloadStatus[i]).get_Text() != "Available")
					{
						return;
					}
					TransferPhaseImage(i);
					break;
				}
			}
			if (Data_and_Plots.PlotForm != null)
			{
				Button cmdSetModelMax = Data_and_Plots.PlotForm.cmdSetModelMax;
				bool enabled;
				((Control)Data_and_Plots.PlotForm.cmdSetModelMin).set_Enabled(enabled = true);
				((Control)cmdSetModelMax).set_Enabled(enabled);
				for (int j = 0; j < 10; j++)
				{
					((Control)Data_and_Plots.PlotForm.cmdTransferImage[j]).set_Enabled(true);
					((Control)Data_and_Plots.PlotForm.cmdTransferImage[j]).set_BackColor(((Control)cmdPhaseModel[j]).get_BackColor());
				}
			}
		}

		internal bool SetAsteroid(int AsteroidNumber)
		{
			((Control)pnlPhaseChange).set_Visible(false);
			((Control)lblSelect).set_Text("S e l e c t   m o d e l   t o   d i s p l a y");
			if (!Validate_DateTime_Entries())
			{
				return false;
			}
			if (((ListControl)lstModels).get_SelectedIndex() >= 0 && GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].Asteroid_number == AsteroidNumber)
			{
				JDforImage = Utilities.JD_from_Date(int.Parse(((Control)txtYear).get_Text()), int.Parse(((Control)txtMonth).get_Text()), double.Parse(((Control)txtDay).get_Text()) + double.Parse(((Control)txtHrs).get_Text()) / 24.0 + double.Parse(((Control)txtDeltaT).get_Text()) / 24.0 + double.Parse(((Control)txtMin).get_Text()) / 1440.0);
				Compute_Phase_Adjustment(Currently_Displayed_Model);
				return true;
			}
			bool flag = false;
			NumDAMITImages = 0;
			Currently_Displayed_Model = 0;
			if (GetShapeModelData.ShapeModelList.Count > 0)
			{
				int num = 0;
				int num2 = GetShapeModelData.ShapeModelList.Count - 1;
				flag = false;
				int num3;
				do
				{
					num3 = (num2 + num) / 2;
					if (GetShapeModelData.ShapeModelList[num3].Asteroid_number == AsteroidNumber)
					{
						flag = true;
						break;
					}
					if (GetShapeModelData.ShapeModelList[num3].Asteroid_number > AsteroidNumber)
					{
						num2 = num3 - 1;
					}
					else
					{
						num = num3 + 1;
					}
				}
				while (num <= num2);
				if (flag)
				{
					((ListControl)lstModels).set_SelectedIndex(num3);
					PlotLabel = AsteroidNumber.ToString();
					AddAsteroidNameToPlotLabel(PlotLabel);
				}
				else
				{
					((ListControl)lstModels).set_SelectedIndex(0);
				}
				ResetPhaseAdust();
			}
			return flag;
		}

		private void lstModels_SelectedIndexChanged(object sender, EventArgs e)
		{
			GetModelsForSelectedAsteroid();
		}

		internal void GetModelsForSelectedAsteroid()
		{
			int selectedIndex = ((ListControl)lstModels).get_SelectedIndex();
			if (selectedIndex < 0)
			{
				return;
			}
			Currently_Displayed_Model = 0;
			Cursor.set_Current(Cursors.get_WaitCursor());
			((ToolStripItem)phaseImagesToolStripMenuItem).set_Enabled(false);
			ClosePhaseChangePanel(0);
			LastDisplayedImage = 0;
			((ToolStripItem)showDAMITFitsToLightCurvesToolStripMenuItem).set_Enabled(false);
			PlotLabel = lstModels.get_Items().get_Item(((ListControl)lstModels).get_SelectedIndex()).ToString()!.Substring(0, 6).Trim();
			AddAsteroidNameToPlotLabel(PlotLabel);
			for (int i = 0; i < 6; i++)
			{
				Label obj = lblDownloadStatus[i, 0];
				bool enabled;
				((Control)cmdGetModel[i, 0]).set_Enabled(enabled = GetShapeModelData.ShapeModelList[selectedIndex].DAMIT_id[i] > 0);
				((Control)obj).set_Enabled(enabled);
				if (((Control)cmdGetModel[i, 0]).get_Enabled())
				{
					((Control)cmdGetModel[i, 0]).set_Text("#" + GetShapeModelData.ShapeModelList[selectedIndex].DAMIT_id[i]);
				}
				else
				{
					((Control)cmdGetModel[i, 0]).set_Text("...");
				}
				((Control)lblDownloadStatus[i, 0]).set_Text("- - -");
				((Control)lblDownloadStatus[i, 0]).set_ForeColor(Color.DarkBlue);
				Label obj2 = lblDownloadStatus[i, 1];
				((Control)cmdGetModel[i, 1]).set_Enabled(enabled = GetShapeModelData.ShapeModelList[selectedIndex].ISAMmodelCount > i);
				((Control)obj2).set_Enabled(enabled);
				if (((Control)cmdGetModel[i, 1]).get_Enabled())
				{
					((Control)cmdGetModel[i, 1]).set_Text("#" + GetShapeModelData.ShapeModelList[selectedIndex].ISAM_id[i]);
				}
				else
				{
					((Control)cmdGetModel[i, 1]).set_Text("...");
				}
				((Control)lblDownloadStatus[i, 1]).set_Text("- - -");
				((Control)lblDownloadStatus[i, 1]).set_ForeColor(Color.DarkBlue);
			}
			if (Data_and_Plots.PlotForm != null)
			{
				for (int j = 0; j < 6; j++)
				{
					((Control)Data_and_Plots.PlotForm.cmdTransferImage[j]).set_Enabled(((Control)cmdGetModel[j, 0]).get_Enabled());
					((Control)Data_and_Plots.PlotForm.cmdTransferImage[j + 6]).set_Enabled(((Control)cmdGetModel[j, 1]).get_Enabled());
				}
			}
			for (int k = 0; k < 2; k++)
			{
				for (int l = 0; l < 6; l++)
				{
					((Control)lblPeriod[l, k]).set_Text("???");
				}
			}
			((Control)lblPhaseAngle).set_Text("");
			Application.DoEvents();
			CurrentlySelectedAsteroid_DAMIT_ID = GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].Asteroid_id;
			CurrentlySelectedAsteroid = GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].Asteroid_number;
			for (int m = 0; m < 6; m++)
			{
				CurrentlySelected_DAMITModelNumbers[m] = GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].DAMIT_id[m];
				if (((Control)cmdGetModel[m, 0]).get_Enabled())
				{
					GetDAMITRotationParameters(CurrentlySelectedAsteroid_DAMIT_ID, CurrentlySelected_DAMITModelNumbers[m], out ModelParameters[m]);
				}
				else
				{
					ModelParameters[m] = new Model_Parameters();
				}
			}
			Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 2;
			GetShapeModelData.ISAMmodelsByAsteroidNumber.Sort();
			for (int n = 0; n < 6; n++)
			{
				GetISAMRotationParameters(GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].Asteroid_number, GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].ISAM_id[n], out ModelParameters[n + 6]);
			}
			for (int num = 0; num < 2; num++)
			{
				for (int num2 = 0; num2 < 6; num2++)
				{
					if (((Control)cmdGetModel[num2, num]).get_Enabled())
					{
						((Control)lblPeriod[num2, num]).set_Text(ModelParameters[num2 + 6 * num].Period.PadRight(9).Substring(0, 7));
					}
					else
					{
						((Control)lblPeriod[num2, num]).set_Text("");
					}
				}
			}
			for (int num3 = 0; num3 < 6; num3++)
			{
				DamitEquivalent[num3] = (ISAMEquivalent[num3] = -1);
			}
			for (int num4 = 0; num4 < 6; num4++)
			{
				((Control)cmdGetModel[num4, 1]).set_Font(new Font("Microsoft Sans Serif", 8f));
				if (!((Control)cmdGetModel[num4, 1]).get_Enabled())
				{
					continue;
				}
				for (int num5 = 0; num5 < 6; num5++)
				{
					if (((Control)cmdGetModel[num5, 0]).get_Enabled() && ((ModelParameters[num5].Lambda == ModelParameters[6 + num4].Lambda) & (ModelParameters[num5].Beta == ModelParameters[6 + num4].Beta) & (ModelParameters[num5].JDat0 == ModelParameters[6 + num4].JDat0)) && ((ModelParameters[num5].Vertices == ModelParameters[6 + num4].Vertices) & (ModelParameters[num5].Faces == ModelParameters[6 + num4].Faces)))
					{
						((Control)cmdGetModel[num4, 1]).set_Font(new Font("Microsoft Sans Serif", 6f));
						DamitEquivalent[num4] = num5;
						ISAMEquivalent[num5] = num4;
					}
				}
			}
			ResetPhaseAdust();
			ClearAllLambdaChecks();
			toolStripMenuItem5.set_Checked(true);
			dLambda = 0;
			ClearAllBetaChecks();
			toolStripMenuItem12.set_Checked(true);
			dBeta = 0;
			Compute_Phase_Adjustment(Currently_Displayed_Model);
			Cursor.set_Current(Cursors.get_Default());
			GetModels();
		}

		private static void AddAsteroidNameToPlotLabel(string Label)
		{
			if (!int.TryParse(Label, out var result))
			{
				result = 0;
			}
			if (Elements.MainAsteroids.AstElements.Count < 1)
			{
				Elements.MainAsteroids.Fill_AllAsteroids();
			}
			int asteroidRecord_fromNumber = Elements.MainAsteroids.GetAsteroidRecord_fromNumber(result);
			if (asteroidRecord_fromNumber >= 0)
			{
				PlotLabel = "(" + Label + ") " + Elements.MainAsteroids.AstElements[asteroidRecord_fromNumber].IDName + " ";
			}
			else
			{
				PlotLabel = "(" + Label + ") .....";
			}
		}

		private void cmdDownloadModels_Click(object sender, EventArgs e)
		{
			if (((Control)pnlPhaseChange).get_Visible())
			{
				GetPhaseAdjustedModels(PhaseStep);
			}
			else
			{
				GetModels();
			}
		}

		internal void ResetDownloadModels_Text()
		{
			((Control)cmdDownloadModels).set_Text("Display\r\nmodels");
		}

		internal bool Validate_DateTime_Entries()
		{
			//IL_0278: Unknown result type (might be due to invalid IL or missing references)
			bool flag = false;
			bool flag2 = false;
			if (!double.TryParse(((Control)txtYear).get_Text(), out var result))
			{
				((Control)txtYear).set_Text(DateTime.UtcNow.Year.ToString());
				flag = true;
			}
			if (result == 1000.0)
			{
				return false;
			}
			if (result < 1850.0 || result > 2050.0)
			{
				((Control)txtYear).set_Text(DateTime.UtcNow.Year.ToString());
				flag = true;
			}
			if (!double.TryParse(((Control)txtMonth).get_Text(), out result))
			{
				((Control)txtMonth).set_Text(DateTime.UtcNow.Month.ToString());
				flag = true;
			}
			if (result < 1.0 || result > 12.0)
			{
				((Control)txtMonth).set_Text(DateTime.UtcNow.Month.ToString());
				flag = true;
			}
			if (!double.TryParse(((Control)txtDay).get_Text(), out result))
			{
				((Control)txtDay).set_Text(DateTime.UtcNow.Day.ToString());
				flag = true;
			}
			if (result < 1.0 || result > 31.0)
			{
				((Control)txtDay).set_Text(DateTime.UtcNow.Day.ToString());
				flag = true;
			}
			if (!double.TryParse(((Control)txtHrs).get_Text(), out result))
			{
				((Control)txtHrs).set_Text("0");
				flag2 = true;
			}
			if (result < 0.0 || result > 23.0)
			{
				((Control)txtHrs).set_Text("0");
				flag2 = true;
			}
			if (!double.TryParse(((Control)txtMin).get_Text(), out result))
			{
				((Control)txtMin).set_Text("0");
				flag2 = true;
			}
			if (result < 0.0 || result > 60.0)
			{
				((Control)txtMin).set_Text("0");
				flag2 = true;
			}
			if (!double.TryParse(((Control)txtDeltaT).get_Text(), out var _))
			{
				((Control)txtDeltaT).set_Text("0");
				flag2 = true;
			}
			if (flag || flag2)
			{
				string text = "";
				if (flag)
				{
					text = "* One or more of the date fields contained non-numeric data, or was outside the range of valid values. A value corresponding to today has been inserted.\r\n\r\n";
				}
				if (flag2)
				{
					text += "* One or more of the time fields contained non-numeric data, or was outside the range of valid values. The value '0' has been inserted.\r\n\r\n";
				}
				MessageBox.Show(text + "You need to review the fields to correct the date and time fields, and correct the values to those you require.", "Data entry error", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			return !(flag || flag2);
		}

		internal void GetModels()
		{
			if (IsInitialising)
			{
				return;
			}
			Images.Clear();
			MessageForm messageForm = new MessageForm();
			((Form)messageForm).set_TopMost(true);
			MessageForm messageForm2 = messageForm;
			((Control)messageForm2).Show();
			NumDAMITImages = 0;
			CurrentImage = 0;
			if (!Validate_DateTime_Entries())
			{
				return;
			}
			((Control)cmdCancel).set_Visible(true);
			CancelPhaseDownloads = false;
			LastDisplayedImage = 0;
			((Control)lblModel).set_Text("Model #");
			for (int i = 0; i < 2; i++)
			{
				for (int j = 0; j < 6; j++)
				{
					((Control)lblDownloadStatus[j, i]).set_Text("- - -");
					((Control)lblDownloadStatus[j, i]).set_ForeColor(Color.DarkBlue);
					((Control)cmdGetModel[j, i]).set_BackColor(SystemColors.Control);
				}
			}
			JDforImage = Utilities.JD_from_Date(int.Parse(((Control)txtYear).get_Text()), int.Parse(((Control)txtMonth).get_Text()), double.Parse(((Control)txtDay).get_Text()) + double.Parse(((Control)txtHrs).get_Text()) / 24.0 + double.Parse(((Control)txtDeltaT).get_Text()) / 24.0 + double.Parse(((Control)txtMin).get_Text()) / 1440.0);
			((Control)lblJD).set_Text(string.Format("Event JD = {0,1:f5}", Utilities.JD_from_Date(int.Parse(((Control)txtYear).get_Text()), int.Parse(((Control)txtMonth).get_Text()), double.Parse(((Control)txtDay).get_Text()) + double.Parse(((Control)txtHrs).get_Text()) / 24.0 + double.Parse(((Control)txtMin).get_Text()) / 1440.0)));
			EventJD = Utilities.JD_from_Date(int.Parse(((Control)txtYear).get_Text()), int.Parse(((Control)txtMonth).get_Text()), double.Parse(((Control)txtDay).get_Text()) + double.Parse(((Control)txtHrs).get_Text()) / 24.0 + double.Parse(((Control)txtMin).get_Text()) / 1440.0);
			for (int k = 0; k < 2; k++)
			{
				Application.DoEvents();
				if (CancelPhaseDownloads)
				{
					break;
				}
				for (int l = 0; l < 6; l++)
				{
					if (((k == 0) & (GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].DAMIT_id[l] == 0)) || ((k == 1) & (GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].ISAM_id[l] == 0)))
					{
						continue;
					}
					Application.DoEvents();
					if (CancelPhaseDownloads)
					{
						break;
					}
					if (!((Control)cmdGetModel[l, k]).get_Enabled())
					{
						continue;
					}
					bool flag = false;
					((Control)lblDownloadStatus[l, k]).set_Text("???");
					((Control)lblDownloadStatus[l, k]).set_ForeColor(Color.DarkRed);
					Application.DoEvents();
					if (k == 0)
					{
						((Control)messageForm2.label).set_Text("Displaying DAMIT " + GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].DAMIT_id[l]);
						flag = GetDAMITModel_Local(GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].DAMIT_id[l], PhaseModels: false);
						if (flag)
						{
							ModelParameters[l].VolumeEquivalentRadius = DrawShapeModel.Vol_Equiv_Radius_OnPlot;
							ModelParameters[l].SurfaceEquivalentRadius = DrawShapeModel.Surf_Equiv_Radius_OnPlot;
						}
						Application.DoEvents();
					}
					else
					{
						((Control)messageForm2.label).set_Text("Displaying ISAM " + GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].Asteroid_number + " #" + GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].ISAM_id[l]);
						Application.DoEvents();
						flag = GetISAMModel_Local(GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].Asteroid_number, GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].ISAM_id[l], PhaseModels: false);
						if (flag)
						{
							ModelParameters[6 + l].VolumeEquivalentRadius = DrawShapeModel.Vol_Equiv_Radius_OnPlot;
							ModelParameters[6 + l].SurfaceEquivalentRadius = DrawShapeModel.Surf_Equiv_Radius_OnPlot;
							Application.DoEvents();
						}
					}
					if (flag)
					{
						((Control)lblDownloadStatus[l, k]).set_Text("Yes");
						((Control)lblDownloadStatus[l, k]).set_ForeColor(Color.DarkGreen);
						if (k == 0)
						{
							NumDAMITImages++;
						}
						TransferImage(Images.Count - 1);
					}
					else
					{
						((Control)lblDownloadStatus[l, k]).set_Text("No");
						((Control)lblDownloadStatus[l, k]).set_ForeColor(Color.Red);
					}
				}
				Application.DoEvents();
				if (CancelPhaseDownloads)
				{
					break;
				}
			}
			if (NumDAMITImages == 0)
			{
				Currently_Displayed_Model = 6;
			}
			TransferImage(LastDisplayedImage);
			((Form)messageForm2).Close();
			((Component)(object)messageForm2).Dispose();
			if (Data_and_Plots.PlotForm != null)
			{
				Data_and_Plots.PlotForm.SetTransferButtons(0);
			}
			((ToolStripItem)showDAMITFitsToLightCurvesToolStripMenuItem).set_Enabled(true);
			((ToolStripItem)phaseImagesToolStripMenuItem).set_Enabled(true);
			CancelPhaseDownloads = false;
			((Control)cmdCancel).set_Visible(false);
		}

		internal void TransferImage(int ImageNumber)
		{
			if (IsTransferring || ((Images.Count < 1) | (Images.Count <= ImageNumber)))
			{
				return;
			}
			IsTransferring = true;
			for (int i = 0; i < 2; i++)
			{
				for (int j = 0; j < 6; j++)
				{
					((Control)cmdGetModel[j, i]).set_BackColor(SystemColors.Control);
				}
			}
			AsteroidDiameterByVol_AsFractionOfPlotWidth = 0f;
			AsteroidDiameterBySurf_AsFractionOfPlotWidth = 0f;
			if (Data_and_Plots.PlotForm != null)
			{
				if (Currently_Displayed_Model < 6)
				{
					((Control)Data_and_Plots.PlotForm.txtModelSource).set_Text("DAMIT");
				}
				else
				{
					((Control)Data_and_Plots.PlotForm.txtModelSource).set_Text("ISAM");
				}
				((Control)Data_and_Plots.PlotForm.txtModelID).set_Text(ModelParameters[Currently_Displayed_Model].ModelNumber_String);
				((Control)Data_and_Plots.PlotForm.txtVersion).set_Text(ModelParameters[Currently_Displayed_Model].VersionComments);
				((Control)Data_and_Plots.PlotForm.txtModelPhase).set_Text("0°");
				((Control)Data_and_Plots.PlotForm.txtSurface_Volume_Ratio).set_Text(string.Format("{0,1:f3}", ModelParameters[Currently_Displayed_Model].SurfaceToVolumeRatio));
			}
			if (ImageNumber < NumDAMITImages)
			{
				((Control)cmdGetModel[ImageNumber, 0]).set_BackColor(Color.LightGreen);
				((Control)lblModel).set_Text(((Control)cmdGetModel[ImageNumber, 0]).get_Text());
				if (ModelParameters[Currently_Displayed_Model].Period_hrs > 0.0)
				{
					double num = (ModelParameters[Currently_Displayed_Model].PhaseAt0 + ModelParameters[Currently_Displayed_Model].DailyRotationRate * (EventJD - ModelParameters[Currently_Displayed_Model].JDat0)) % 360.0;
					((Control)lblPhaseAngle).set_Text(string.Format("Phase angle\r\n {0,1:f1}°", num));
				}
				else
				{
					((Control)lblPhaseAngle).set_Text("");
				}
			}
			else
			{
				((Control)cmdGetModel[ImageNumber - NumDAMITImages, 1]).set_BackColor(Color.LightGreen);
				((Control)lblModel).set_Text(((Control)cmdGetModel[ImageNumber - NumDAMITImages, 1]).get_Text());
				_ = NumDAMITImages;
				((Control)lblPhaseAngle).set_Text("Phase angle\r\n   ?");
			}
			float num2 = (float)trackBarImageSize.get_Value() / 100f;
			((Control)picDisplay).set_Width((int)((float)Images[ImageNumber].get_Image().Width * num2));
			((Control)picDisplay).set_Height((int)((float)Images[ImageNumber].get_Image().Height * num2));
			Point point = new Point(0, 0);
			Point point2 = new Point((int)((float)Images[ImageNumber].get_Image().Width * num2), 0);
			Point point3 = new Point(0, (int)((float)Images[ImageNumber].get_Image().Height * num2));
			Point[] destPoints = new Point[3] { point, point2, point3 };
			Bitmap image = new Bitmap(((Control)picDisplay).get_Width(), ((Control)picDisplay).get_Height());
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Color.Black);
			Image image2 = (Image)Images[ImageNumber].get_Image().Clone();
			if (PlotOnEarthPlane)
			{
				image2.RotateFlip(RotateFlipType.RotateNoneFlipX);
			}
			graphics.DrawImage(image2, destPoints);
			string text = "Sky Plane";
			Font font = new Font("Times New Roman", 9f, FontStyle.Regular);
			Brush white = Brushes.White;
			Brush brush = Brushes.Black;
			white = Brushes.White;
			if (BlackToWhiteBackground > 0)
			{
				white = Brushes.Black;
			}
			if (BlackToWhiteBackground == 1)
			{
				brush = Brushes.White;
			}
			else if (BlackToWhiteBackground == 2)
			{
				brush = new SolidBrush(ShapeModelGrayBackColor);
			}
			text = "Sky Plane";
			if (PlotOnEarthPlane)
			{
				text = "Earth Plane";
			}
			SizeF sizeF = graphics.MeasureString(text, font);
			graphics.FillRectangle(brush, (float)(((Control)picDisplay).get_Width() - 8) - sizeF.Width, 1f, sizeF.Width + 2f, sizeF.Height + 1f);
			graphics.DrawString(text, font, white, (float)(((Control)picDisplay).get_Width() - 8) - sizeF.Width, 1f);
			if (DrawDamitID)
			{
				font = new Font("Times New Roman", 8f, FontStyle.Regular);
				new Font("Arial", 10f);
				if ((double)num2 < 0.5)
				{
					font = new Font("Arial", 7f);
					new Font("Arial", 9f);
				}
				if ((double)num2 < 0.4)
				{
					font = new Font("Arial", 6f);
					new Font("Arial", 8f);
				}
				text = ((Currently_Displayed_Model >= 6) ? "ISAM " : "DAMIT #");
				text += ModelParameters[Currently_Displayed_Model].ModelNumber_String.Trim();
				sizeF = graphics.MeasureString(text, font);
				graphics.FillRectangle(brush, (float)(((Control)picDisplay).get_Width() - 6) - sizeF.Width, 30f, sizeF.Width, sizeF.Height + 1f);
				graphics.DrawString(text, font, white, (float)(((Control)picDisplay).get_Width() - 6) - sizeF.Width, 30f);
				text = PlotLabel;
				sizeF = graphics.MeasureString(text, font);
				graphics.FillRectangle(brush, (float)(((Control)picDisplay).get_Width() - 8) - sizeF.Width, 16f, sizeF.Width + 2f, sizeF.Height + 1f);
				graphics.DrawString(text, font, white, (float)(((Control)picDisplay).get_Width() - 8) - sizeF.Width, 16f);
			}
			if (IncludePhaseEffects)
			{
				text = "Sun-Lit";
				sizeF = graphics.MeasureString(text, font);
				graphics.FillRectangle(Brushes.White, (float)(((Control)picDisplay).get_Width() - 10) - sizeF.Width, 24f, sizeF.Width + 4f, sizeF.Height);
				graphics.DrawString(text, font, white, (float)(((Control)picDisplay).get_Width() - 8) - sizeF.Width, 18f);
			}
			AsteroidDiameterByVol_AsFractionOfPlotWidth = (float)ModelParameters[Currently_Displayed_Model].VolumeEquivalentRadius * DrawShapeModel.ImagePlotScale_1Radius[0];
			AsteroidDiameterBySurf_AsFractionOfPlotWidth = (float)ModelParameters[Currently_Displayed_Model].SurfaceEquivalentRadius * DrawShapeModel.ImagePlotScale_1Radius[0];
			if (DrawDAMITMean)
			{
				Pen pen = ((BlackToWhiteBackground == 0) ? new Pen(Color.White, 1.8f) : ((BlackToWhiteBackground != 0) ? new Pen(Color.Black, 1.8f) : new Pen(Color.Black, 1.8f)));
				float num3 = (float)VolumeEquivalent_Diameter_Pixels[ImageNumber] / ImageSize / 2f;
				float num4 = (float)SurfaceEquivalent_Diameter_Pixels[ImageNumber] / ImageSize / 2f;
				if (num3 > 0f)
				{
					if (num4 == 0f)
					{
						num4 = num3;
					}
					if (num4 <= num3)
					{
						graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f - num3), ((Control)picDisplay).get_Height() - 30, (float)((Control)picDisplay).get_Width() * (0.5f + num3), ((Control)picDisplay).get_Height() - 30);
					}
					else
					{
						graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f - num4), ((Control)picDisplay).get_Height() - 30, (float)((Control)picDisplay).get_Width() * (0.5f + num4), ((Control)picDisplay).get_Height() - 30);
					}
					graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f - num3), ((Control)picDisplay).get_Height() - 34, (float)((Control)picDisplay).get_Width() * (0.5f - num3), ((Control)picDisplay).get_Height() - 30);
					graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f + num3), ((Control)picDisplay).get_Height() - 34, (float)((Control)picDisplay).get_Width() * (0.5f + num3), ((Control)picDisplay).get_Height() - 30);
					graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f - num4), ((Control)picDisplay).get_Height() - 30, (float)((Control)picDisplay).get_Width() * (0.5f - num4), ((Control)picDisplay).get_Height() - 25);
					graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f + num4), ((Control)picDisplay).get_Height() - 30, (float)((Control)picDisplay).get_Width() * (0.5f + num4), ((Control)picDisplay).get_Height() - 25);
					string text2 = " Mean diameter by: Vol↑ Surf↓";
					font = new Font("Times New Roman", 8f, FontStyle.Regular);
					sizeF = graphics.MeasureString(text2, font);
					graphics.FillRectangle(brush, ((float)((Control)picDisplay).get_Width() - sizeF.Width) / 2f, ((Control)picDisplay).get_Height() - 27, sizeF.Width, sizeF.Height);
					graphics.DrawString(text2, font, white, ((float)((Control)picDisplay).get_Width() - sizeF.Width) / 2f - 1f, (float)((Control)picDisplay).get_Height() - 27f);
				}
			}
			picDisplay.set_Image((Image)image);
			CurrentImage = ImageNumber;
			if (Data_and_Plots.PlotForm != null)
			{
				if (Data_and_Plots.PlotForm.addShapeImageFromDAMIT_ISAMToolStripMenuItem.get_Checked())
				{
					Data_and_Plots.PlotEventOnScreen();
				}
				Application.DoEvents();
			}
			IsTransferring = false;
		}

		internal static bool GetDAMITModel_Local(int modelId, bool PhaseModels)
		{
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_007c: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0100: Expected O, but got Unknown
			if (modelId == 0)
			{
				return false;
			}
			if (!File.Exists(GetShapeModelData.ShapeModelDirectory + modelId + ".txt"))
			{
				bool InternetAvailable;
				bool flag = GetShapeModelData.DownloadDAMITShape(modelId.ToString(), out InternetAvailable);
				if (!InternetAvailable)
				{
					MessageBox.Show("DAMIT model " + modelId + " is not available locally, and the internet is not available for downloading it.", "No Internet", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return false;
				}
				if (!flag)
				{
					MessageBox.Show("DAMIT model " + modelId + " is not available locally, and is not available for download", "Model unavailable", (MessageBoxButtons)0, (MessageBoxIcon)64);
					return false;
				}
			}
			GetShapeModelData.InitialiseDAMITShapeModels();
			DrawShapeModel.PopulateDataElementsForTheShapeModel(modelId.ToString(), 0);
			DrawShapeModel.GetDataToOrientTheModel(JDforImage, CurrentlySelectedAsteroid);
			DrawShapeModel.OrientTheVertices(JDforImage, dLambda, dBeta);
			Color background = Color.Black;
			if (BlackToWhiteBackground == 1)
			{
				background = Color.White;
			}
			else if (BlackToWhiteBackground == 2)
			{
				background = ShapeModelGrayBackColor;
			}
			bool earthPlane = false;
			PictureBox val = new PictureBox();
			val.set_Image((Image)new Bitmap((int)ImageSize, (int)ImageSize));
			PictureBox val2 = val;
			DrawShapeModel.PlotShapeModel(Graphics.FromImage(val2.get_Image()), ImageSize, ImageSize, BlackWhiteModel, PlotModelDark, earthPlane, background, ShowModelFaceEdges, IncludeAxisOfRotation);
			if (PhaseModels)
			{
				PhaseImages.Add(val2);
			}
			else
			{
				Images.Add(val2);
			}
			VolumeEquivalent_Diameter_Pixels[Images.Count - 1] = DrawShapeModel.Vol_Equiv_Radius_OnPlot * 2.0;
			SurfaceEquivalent_Diameter_Pixels[Images.Count - 1] = DrawShapeModel.Surf_Equiv_Radius_OnPlot * 2.0;
			return true;
		}

		internal static bool GetISAMModel_Local(int Asteroid, int Model, bool PhaseModels)
		{
			//IL_007d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0082: Unknown result type (might be due to invalid IL or missing references)
			//IL_009b: Expected O, but got Unknown
			//IL_0156: Unknown result type (might be due to invalid IL or missing references)
			string text = GetShapeModelData.ISAM_Model_Number(Asteroid, Model);
			if (!Find_ISAM_ModelEntry(text, out var _))
			{
				GetShapeModelData.Add_Additional_ISAM_Model(Asteroid, Model, initialiseISAM: true);
			}
			if (Find_ISAM_ModelEntry(text, out var Current2))
			{
				DrawShapeModel.PopulateDataElementsForTheShapeModel(text, Current2);
				DrawShapeModel.GetDataToOrientTheModel(JDforImage, CurrentlySelectedAsteroid);
				DrawShapeModel.OrientTheVertices(JDforImage, dLambda, dBeta);
				Color background = Color.Black;
				if (BlackToWhiteBackground == 1)
				{
					background = Color.White;
				}
				else if (BlackToWhiteBackground == 2)
				{
					background = ShapeModelGrayBackColor;
				}
				bool earthPlane = false;
				PictureBox val = new PictureBox();
				val.set_Image((Image)new Bitmap((int)ImageSize, (int)ImageSize));
				PictureBox val2 = val;
				DrawShapeModel.PlotShapeModel(Graphics.FromImage(val2.get_Image()), ImageSize, ImageSize, BlackWhiteModel, PlotModelDark, earthPlane, background, ShowModelFaceEdges, IncludeAxisOfRotation);
				if (PhaseModels)
				{
					PhaseImages.Add(val2);
				}
				else
				{
					Images.Add(val2);
				}
				VolumeEquivalent_Diameter_Pixels[Images.Count - 1] = DrawShapeModel.Vol_Equiv_Radius_OnPlot * 2.0;
				SurfaceEquivalent_Diameter_Pixels[Images.Count - 1] = DrawShapeModel.Surf_Equiv_Radius_OnPlot * 2.0;
				Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 2;
				GetShapeModelData.ISAMmodelsByAsteroidNumber.Sort();
				return true;
			}
			MessageBox.Show("Shape model " + text + " is not available", "No shape model", (MessageBoxButtons)0, (MessageBoxIcon)64);
			return false;
		}

		internal static bool Find_ISAM_ModelEntry(string ModelNumber, out int Current)
		{
			GetShapeModelData.Initialise_ISAM_ShapeModels();
			bool result = false;
			Current = 0;
			if (File.Exists(GetShapeModelData.ShapeModelDirectory + ModelNumber + ".txt"))
			{
				Model_Parameters.Sort_0bysteroidNumber_1byModelNumberNumeric_2forISAM = 2;
				GetShapeModelData.ISAMmodelsByAsteroidNumber.Sort();
				int num = 0;
				int num2 = GetShapeModelData.ISAMmodelsByAsteroidNumber.Count - 1;
				result = false;
				if (num2 < num)
				{
					return result;
				}
				do
				{
					Current = (num2 + num) / 2;
					if (GetShapeModelData.ISAMmodelsByAsteroidNumber[Current].ModelNumber_String == ModelNumber)
					{
						result = true;
						break;
					}
					if (GetShapeModelData.ISAMmodelsByAsteroidNumber[Current].ModelNumber_String.CompareTo(ModelNumber) < 0)
					{
						num = Current + 1;
					}
					else
					{
						num2 = Current - 1;
					}
				}
				while (num <= num2);
			}
			return result;
		}

		private void listDAMITToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ListDAMITLightCurves();
		}

		internal void ListDAMITLightCurves()
		{
			//IL_02ab: Unknown result type (might be due to invalid IL or missing references)
			string input = "";
			string text = "Shape model parameters for Asteroid (" + CurrentlySelectedAsteroid + ")";
			string[] array = new string[2] { "DAMIT ", "ISAM " };
			for (int i = 0; i < 2; i++)
			{
				for (int j = 0; j < 6; j++)
				{
					if (((Control)cmdGetModel[j, i]).get_Enabled())
					{
						text += "\r\n\r\n";
						text = text + "Model " + array[i] + ModelParameters[j + 6 * i].ModelNumber_String;
						text += $"\r\n {'λ'.ToString()} = {ModelParameters[j + 6 * i].Lambda}°, {'β'.ToString()} = {ModelParameters[j + 6 * i].Beta}°, Period = {ModelParameters[j + 6 * i].Period} hrs";
						text = text + "\r\nZero date for Rotation ephemeris: " + Utilities.DateTime_from_JD(ModelParameters[j + 6 * i].JDat0);
						text += string.Format("\r\nRotation angle: {0,5:f1}° + {1,2:f6}° * ( JD - {2,2:f6} )", ModelParameters[j + 6 * i].PhaseAt0, 8640.0 / ModelParameters[j + 6 * i].Period_hrs, ModelParameters[j + 6 * i].JDat0);
						if (ModelParameters[j + 6 * i].YORP != "0")
						{
							text = text + "\r\nYORP coefficient = " + ModelParameters[j + 6 * i].YORP;
						}
					}
				}
			}
			text += "\r\n\r\nLight curve dates\r\n";
			MessageForm messageForm = new MessageForm();
			((Control)messageForm.label).set_Text("Downloading Light curve observations");
			((Control)messageForm).Show();
			Application.DoEvents();
			ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
			HttpWebRequest httpWebRequest = (HttpWebRequest)WebRequest.Create($"https://astro.troja.mff.cuni.cz/projects/damit/LightCurves/lcRef/{CurrentlySelectedAsteroid_DAMIT_ID}/A{CurrentlySelectedAsteroid_DAMIT_ID}2.lc.ref.txt");
			httpWebRequest.Method = "GET";
			try
			{
				using WebResponse webResponse = httpWebRequest.GetResponse();
				using Stream stream = webResponse.GetResponseStream();
				using StreamReader streamReader = new StreamReader(stream);
				input = streamReader.ReadToEnd();
			}
			catch
			{
				MessageBox.Show("The DAMIT site has not responded to the\r\nrequest for references", "DAMIT error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				text += "... are not available\r\n";
			}
			((Form)messageForm).Close();
			try
			{
				((Control)DataBox).Show();
			}
			catch
			{
				DataBox = new DisplayData();
				((Control)DataBox).Show();
			}
			((ToolStripItem)DataBox.cmdCancel).set_Visible(false);
			((ToolStripItem)DataBox.cmdOK).set_Visible(false);
			((Control)DataBox.txtBox).set_Text(text + Regex.Replace(input, "\\r?\\n", "\r\n"));
			((TextBoxBase)DataBox.txtBox).set_SelectionStart(0);
			((TextBoxBase)DataBox.txtBox).Select(0, 0);
			((Control)DataBox).set_Width(500);
			((Control)DataBox).set_Height(700);
			((Control)DataBox).set_Text("Asteroid light curve observations");
			((Control)DataBox).Focus();
		}

		private bool GetDAMITRotationParameters(int AsteroidNumber, int DAMIT_ModelNumber, out Model_Parameters DAMITparameters)
		{
			DAMITparameters = new Model_Parameters();
			if (AsteroidNumber < 1 || DAMIT_ModelNumber < 1)
			{
				return false;
			}
			int num = 0;
			int num2 = GetShapeModelData.DAMITModelsByAsteroidNumber.Count - 1;
			int num3;
			do
			{
				num3 = (num + num2) / 2;
				if (GetShapeModelData.DAMITModelsByAsteroidNumber[num3].AsteroidNumber == CurrentlySelectedAsteroid)
				{
					break;
				}
				if (GetShapeModelData.DAMITModelsByAsteroidNumber[num3].AsteroidNumber > CurrentlySelectedAsteroid)
				{
					num2 = num3 - 1;
				}
				else
				{
					num = num3 + 1;
				}
			}
			while (num2 >= num);
			int num4 = num3 - 6;
			if (num4 < 0)
			{
				num4 = 0;
			}
			for (int i = num4; i < GetShapeModelData.DAMITModelsByAsteroidNumber.Count; i++)
			{
				if (GetShapeModelData.DAMITModelsByAsteroidNumber[i].AsteroidNumber != CurrentlySelectedAsteroid || int.Parse(GetShapeModelData.DAMITModelsByAsteroidNumber[i].ModelNumber_String) != DAMIT_ModelNumber)
				{
					continue;
				}
				DAMITparameters = GetShapeModelData.DAMITModelsByAsteroidNumber[i];
				string path = GetShapeModelData.ShapeModelDirectory + DAMITparameters.ModelNumber_String + ".txt";
				if (File.Exists(path))
				{
					using StreamReader streamReader = new StreamReader(path);
					string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
					DAMITparameters.Vertices = int.Parse(array[0]);
					DAMITparameters.Faces = int.Parse(array[1]);
				}
				return true;
			}
			return false;
		}

		private bool GetISAMRotationParameters(int asteroid, int ModelNumber, out Model_Parameters ISAMparameters)
		{
			string text = GetShapeModelData.ISAM_Model_Number(asteroid, ModelNumber);
			ISAMparameters = new Model_Parameters();
			int num = 0;
			int num2 = GetShapeModelData.ISAMmodelsByAsteroidNumber.Count - 1;
			if (num2 < num)
			{
				return false;
			}
			do
			{
				int num3 = (num + num2) / 2;
				if (GetShapeModelData.ISAMmodelsByAsteroidNumber[num3].ModelNumber_String == text)
				{
					ISAMparameters = GetShapeModelData.ISAMmodelsByAsteroidNumber[num3];
					string path = GetShapeModelData.ShapeModelDirectory + ISAMparameters.ModelNumber_String + ".txt";
					if (File.Exists(path))
					{
						using StreamReader streamReader = new StreamReader(path);
						string[] array = streamReader.ReadLine()!.Split(new char[1] { ',' });
						ISAMparameters.Vertices = int.Parse(array[0]);
						ISAMparameters.Faces = int.Parse(array[1]);
					}
					return true;
				}
				if (GetShapeModelData.ISAMmodelsByAsteroidNumber[num3].ModelNumber_String.CompareTo(text) < 0)
				{
					num = num3 + 1;
				}
				else
				{
					num2 = num3 - 1;
				}
			}
			while (num2 >= num);
			return false;
		}

		private void trackBarImageSize_Scroll(object sender, EventArgs e)
		{
			SetScale();
		}

		private void SetScale()
		{
			if (Data_and_Plots.PlotForm != null)
			{
				Data_and_Plots.PlotForm.tbarScaleAdjust.set_Value(Asteroid_Observations_Reports.Display_ShapeModels.trackBarImageSize.get_Value());
			}
			if (((Control)pnlPhaseChange).get_Visible())
			{
				if (PhaseImages.Count > 0)
				{
					TransferPhaseImage(CurrentPhaseImage);
				}
			}
			else if (Images.Count > 0)
			{
				TransferImage(CurrentImage);
			}
		}

		private void picDisplay_Resize(object sender, EventArgs e)
		{
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			GetShapeModelData.ShapeModelList.Clear();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"DAMIT");
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picDisplay.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.DAMITmodelsSave = Output.SaveGraphic(picDisplay.get_Image(), "Shape Model " + ((Control)lblModel).get_Text(), Settings.Default.DAMITmodelsSave);
		}

		internal void ResetPhaseAdust()
		{
			updnRotnRate.set_Value(0m);
			updnZeroPhase.set_Value(0m);
		}

		private void updnZeroPhase_Leave(object sender, EventArgs e)
		{
			Compute_Phase_Adjustment(Currently_Displayed_Model);
		}

		private void updnZeroPhase_ValueChanged(object sender, EventArgs e)
		{
			decimal[] array = ((!(updnRotnRate.get_Value() != 0m)) ? new decimal[3] { 30m, 90m, 180m } : new decimal[3] { 10m, 20m, 30m });
			if (Math.Abs(updnZeroPhase.get_Value()) < array[0])
			{
				((Control)updnZeroPhase).set_ForeColor(Color.Black);
				((Control)updnZeroPhase).set_BackColor(Color.White);
			}
			else if (Math.Abs(updnZeroPhase.get_Value()) < array[1])
			{
				((Control)updnZeroPhase).set_ForeColor(Color.Yellow);
				((Control)updnZeroPhase).set_BackColor(Color.Blue);
			}
			else if (Math.Abs(updnZeroPhase.get_Value()) < array[2])
			{
				((Control)updnZeroPhase).set_ForeColor(Color.Yellow);
				((Control)updnZeroPhase).set_BackColor(Color.Purple);
			}
			else
			{
				((Control)updnZeroPhase).set_ForeColor(Color.Yellow);
				((Control)updnZeroPhase).set_BackColor(Color.Red);
			}
			Compute_Phase_Adjustment(Currently_Displayed_Model);
		}

		private void updnRotnRate_ValueChanged(object sender, EventArgs e)
		{
			Compute_Phase_Adjustment(Currently_Displayed_Model);
		}

		internal void Compute_Phase_Adjustment(int ModelNo)
		{
			if (Validate_DateTime_Entries())
			{
				EventJD = Utilities.JD_from_Date(int.Parse(((Control)txtYear).get_Text()), int.Parse(((Control)txtMonth).get_Text()), double.Parse(((Control)txtDay).get_Text()) + double.Parse(((Control)txtHrs).get_Text()) / 24.0 + double.Parse(((Control)txtMin).get_Text()) / 1440.0);
				CurrentPhaseOffset = (double)updnZeroPhase.get_Value() + (EventJD - ModelParameters[Currently_Displayed_Model].JDat0) * (double)updnRotnRate.get_Value();
				((Control)txtPhaseOffset).set_Text(string.Format("{0, 1:f0}°", CurrentPhaseOffset));
				if (ModelParameters[Currently_Displayed_Model].Period_hrs < 10000.0)
				{
					((Control)txtDeltaT).set_Text(string.Format("{0,1:f2}", ModelParameters[Currently_Displayed_Model].Period_hrs / 360.0 * CurrentPhaseOffset));
				}
			}
		}

		private void keepOnTopToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DAMIT_OnTop();
		}

		internal void DAMIT_OnTop()
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
			Settings.Default.ShapeModel_TopMost = ((Form)this).get_TopMost();
		}

		private void IncludeAxisToolStripMenuItem_Click(object sender, EventArgs e)
		{
			IncludeAxis();
		}

		internal void IncludeAxis()
		{
			int currentImage = CurrentImage;
			IncludeAxisOfRotation = !IncludeAxisOfRotation;
			IncludeAxisToolStripMenuItem.set_Checked(IncludeAxisOfRotation);
			Settings.Default.IncludeAxisOfRotation = IncludeAxisOfRotation;
			if (((Control)pnlPhaseChange).get_Visible())
			{
				GetPhaseAdjustedModels(PhaseStep);
				return;
			}
			GetModels();
			TransferImage(currentImage);
		}

		private void showSolarIlluminationPhaseISAMOnlyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Invalid comparison between Unknown and I4
			if (showSolarIlluminationPhaseISAMOnlyToolStripMenuItem.get_Checked() || (int)MessageBox.Show("A shape model plotted showing only those\r\nportions that are sun-lit is not suitable\r\nfor matching to occultation observations.\r\n\r\nDo you want to set the Sun-Lit option?", "Warning - Solar illumiation", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) != 7)
			{
				showSolarIlluminationPhaseISAMOnlyToolStripMenuItem.set_Checked(!showSolarIlluminationPhaseISAMOnlyToolStripMenuItem.get_Checked());
				IncludePhaseEffects = showSolarIlluminationPhaseISAMOnlyToolStripMenuItem.get_Checked();
				Settings.Default.ISAMshowPhase = IncludePhaseEffects;
			}
		}

		private void dAMITIncludeMeanRadiusOnImageToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowMeanDiameter();
		}

		internal void ShowMeanDiameter()
		{
			DrawDAMITMean = !DrawDAMITMean;
			dAMITIncludeMeanRadiusOnImageToolStripMenuItem.set_Checked(DrawDAMITMean);
			Settings.Default.ShapeModel_DrawMean = DrawDAMITMean;
			TransferImage(Currently_Displayed_Model);
		}

		private void showAsteroidAndModelIDOnImageToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowModelID();
		}

		internal void ShowModelID()
		{
			DrawDamitID = !DrawDamitID;
			showAsteroidAndModelIDOnImageToolStripMenuItem.set_Checked(DrawDamitID);
			Settings.Default.DrawDamitID = DrawDamitID;
			TransferImage(Currently_Displayed_Model);
		}

		private void btnCancel_Click(object sender, EventArgs e)
		{
			ClosePhaseChangePanel(Currently_Displayed_Model);
		}

		private void closePhaseModelSelectorToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ClosePhaseChangePanel(Currently_Displayed_Model);
		}

		internal void ClosePhaseChangePanel(int ModelToDisplay)
		{
			((Control)pnlPhaseChange).set_Visible(false);
			ToolStripMenuItem obj = animateDisplayOfPhaseModelsToolStripMenuItem;
			ToolStripMenuItem obj2 = saveAnimatedGIFOfPhaseModelsToolStripMenuItem;
			ToolStripMenuItem obj3 = saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem;
			bool flag;
			((ToolStripItem)closePhaseModelSelectorToolStripMenuItem).set_Enabled(flag = false);
			bool flag2;
			((ToolStripItem)obj3).set_Enabled(flag2 = flag);
			bool enabled;
			((ToolStripItem)obj2).set_Enabled(enabled = flag2);
			((ToolStripItem)obj).set_Enabled(enabled);
			((Control)lblSelect).set_Text("S e l e c t   m o d e l   t o   d i s p l a y");
			ResetDownloadModels_Text();
			if (Data_and_Plots.PlotForm != null)
			{
				for (int i = 0; i < 12; i++)
				{
					if (((Control)Data_and_Plots.PlotForm.txtModelID).get_Text() == ModelParameters[i].ModelNumber_String)
					{
						CurrentModel = i;
						break;
					}
				}
			}
			try
			{
				Data_and_Plots.PlotForm.SetTransferButtons(CurrentModel);
			}
			catch
			{
			}
			TransferImage(ModelToDisplay);
			((Control)lblDAMITisam).set_Visible(true);
		}

		private void downloadPhaseAt1036StepsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetPhaseAdjustedModels(36.0);
		}

		private void drawPhaseModelsAt16StepsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetPhaseAdjustedModels(16.0);
		}

		private void downloadPhaseAt518StepsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetPhaseAdjustedModels(8.0);
		}

		private void downloadPhaseAt272StepsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetPhaseAdjustedModels(4.0);
		}

		private void downloadPhaseAt136StepsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetPhaseAdjustedModels(2.0);
		}

		private void downloadPhaseAt062StepsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetPhaseAdjustedModels(1.0);
		}

		internal void GetPhaseAdjustedModels(double PhStep)
		{
			if (Images.Count < 1)
			{
				return;
			}
			((Control)lblDAMITisam).set_Visible(false);
			((Control)cmdDownloadModels).set_Text(string.Format("Displayed at {0,1:f0}°", PhStep));
			PhaseImages.Clear();
			CurrentImage = 0;
			((Control)lblSelect).set_Text("S e l e c t   P h a s e - o f f s e t  m o d e l   t o   d i s p l a y");
			((Control)pnlPhaseChange).set_Visible(true);
			PhaseStep = PhStep;
			MessageForm messageForm = new MessageForm();
			((Form)messageForm).set_TopMost(true);
			MessageForm messageForm2 = messageForm;
			((Control)messageForm2).Show();
			((Control)cmdCancel).set_Visible(true);
			CancelPhaseDownloads = false;
			for (int i = 0; i < NumPhaseImages; i++)
			{
				((Control)lblPhaseDownloadStatus[i]).set_Text("  None");
				((Control)lblPhaseDownloadStatus[i]).set_ForeColor(Color.DarkBlue);
				((Control)lblPhaseOffset[i]).set_Text("Offset " + ((double)(i - 4) * PhStep).ToString("+0;-0").PadLeft(3) + "°");
				((Control)cmdPhaseModel[i]).set_Text(string.Format("Ph: {0,1:f0}°", (double)(i - 4) * PhStep + CurrentPhaseOffset));
				((Control)cmdPhaseModel[i]).set_BackColor(SystemColors.Control);
			}
			for (int j = 0; j < NumPhaseImages; j++)
			{
				Application.DoEvents();
				if (CancelPhaseDownloads)
				{
					break;
				}
				JDforImage = Utilities.JD_from_Date(int.Parse(((Control)txtYear).get_Text()), int.Parse(((Control)txtMonth).get_Text()), double.Parse(((Control)txtDay).get_Text()) + (double.Parse(((Control)txtHrs).get_Text()) + double.Parse(((Control)txtDeltaT).get_Text()) + (double)(j - 4) * PhStep / 360.0 * ModelParameters[Currently_Displayed_Model].Period_hrs) / 24.0 + double.Parse(((Control)txtMin).get_Text()) / 1440.0);
				bool flag = false;
				((Control)lblPhaseDownloadStatus[j]).set_Text("Displaying");
				((Control)lblPhaseDownloadStatus[j]).set_ForeColor(Color.DarkRed);
				if (Currently_Displayed_Model < 6)
				{
					((Control)messageForm2.label).set_Text("Displaying DAMIT " + CurrentlySelected_DAMITModelNumbers[Currently_Displayed_Model] + "   phase " + ((double)(j - 4) * PhStep + CurrentPhaseOffset) + "°");
				}
				else
				{
					((Control)messageForm2.label).set_Text("Displaying ISAM " + GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].Asteroid_number + " #" + GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].ISAM_id[Currently_Displayed_Model - 6] + "   phase " + ((double)(j - 4) * PhStep + CurrentPhaseOffset) + "°");
				}
				Application.DoEvents();
				if ((Currently_Displayed_Model >= 6) ? GetISAMModel_Local(GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].Asteroid_number, GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].ISAM_id[Currently_Displayed_Model - 6], PhaseModels: true) : GetDAMITModel_Local(CurrentlySelected_DAMITModelNumbers[Currently_Displayed_Model], PhaseModels: true))
				{
					((Control)lblPhaseDownloadStatus[j]).set_Text("Available");
					((Control)lblPhaseDownloadStatus[j]).set_ForeColor(Color.DarkGreen);
					TransferPhaseImage(j);
					Application.DoEvents();
				}
				else
				{
					((Control)lblPhaseDownloadStatus[j]).set_Text("Failed");
					((Control)lblPhaseDownloadStatus[j]).set_ForeColor(Color.Red);
				}
			}
			((Form)messageForm2).Close();
			if (CancelPhaseDownloads)
			{
				TransferPhaseImage(0);
			}
			else
			{
				TransferPhaseImage(4);
			}
			ToolStripMenuItem obj = animateDisplayOfPhaseModelsToolStripMenuItem;
			ToolStripMenuItem obj2 = saveAnimatedGIFOfPhaseModelsToolStripMenuItem;
			bool flag2;
			((ToolStripItem)closePhaseModelSelectorToolStripMenuItem).set_Enabled(flag2 = true);
			bool enabled;
			((ToolStripItem)obj2).set_Enabled(enabled = flag2);
			((ToolStripItem)obj).set_Enabled(enabled);
			if (Data_and_Plots.PlotForm != null)
			{
				((ToolStripItem)saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem).set_Enabled(true);
				if (Data_and_Plots.PlotForm != null)
				{
					for (int k = 0; k < 12; k++)
					{
						if (((Control)Data_and_Plots.PlotForm.txtModelID).get_Text() == ModelParameters[k].ModelNumber_String)
						{
							CurrentModel = k;
							break;
						}
					}
					for (int l = 0; l < 6; l++)
					{
						((Control)Data_and_Plots.PlotForm.lblISAMmatch[l]).set_Visible(false);
					}
				}
				Data_and_Plots.PlotForm.SetTransferButtons(4);
			}
			CancelPhaseDownloads = false;
			((Control)cmdCancel).set_Visible(false);
			IsTransferring = false;
		}

		internal void TransferPhaseImage(int ImageNumber)
		{
			if (IsTransferring)
			{
				return;
			}
			IsTransferring = true;
			if ((PhaseImages.Count < 1) | (ImageNumber >= PhaseImages.Count))
			{
				return;
			}
			int num = Currently_Displayed_Model;
			if (num > 5)
			{
				num -= 6 - NumDAMITImages;
			}
			for (int i = 0; i < PhaseImages.Count; i++)
			{
				((Control)cmdPhaseModel[i]).set_BackColor(SystemColors.Control);
			}
			((Control)cmdPhaseModel[ImageNumber]).set_BackColor(Color.LightGreen);
			CurrentPhaseImage = ImageNumber;
			float num2 = (float)trackBarImageSize.get_Value() / 100f;
			((Control)picDisplay).set_Width((int)((float)PhaseImages[ImageNumber].get_Image().Width * num2));
			((Control)picDisplay).set_Height((int)((float)PhaseImages[ImageNumber].get_Image().Height * num2));
			Point point = new Point(0, 0);
			Point point2 = new Point((int)((float)PhaseImages[ImageNumber].get_Image().Width * num2), 0);
			Point point3 = new Point(0, (int)((float)PhaseImages[ImageNumber].get_Image().Height * num2));
			Point[] destPoints = new Point[3] { point, point2, point3 };
			Bitmap image = new Bitmap(((Control)picDisplay).get_Width(), ((Control)picDisplay).get_Height());
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Color.Black);
			Image image2 = (Image)PhaseImages[ImageNumber].get_Image().Clone();
			if (PlotOnEarthPlane)
			{
				image2.RotateFlip(RotateFlipType.RotateNoneFlipX);
			}
			graphics.DrawImage(image2, destPoints);
			string text = "Sky Plane";
			Font font = new Font("Times New Roman", 9f, FontStyle.Regular);
			Brush brush = Brushes.White;
			if (BlackToWhiteBackground > 0)
			{
				brush = Brushes.Black;
			}
			Brush brush2 = Brushes.Black;
			if (BlackToWhiteBackground == 1)
			{
				brush2 = Brushes.White;
			}
			else if (BlackToWhiteBackground == 2)
			{
				brush2 = new SolidBrush(ShapeModelGrayBackColor);
			}
			if (PlotOnEarthPlane)
			{
				text = "Earth Plane";
			}
			SizeF sizeF = graphics.MeasureString(text, font);
			graphics.FillRectangle(brush2, (float)(((Control)picDisplay).get_Width() - 10) - sizeF.Width, 2f, sizeF.Width + 4f, sizeF.Height + 2f);
			graphics.DrawString(text, font, brush, (float)(((Control)picDisplay).get_Width() - 8) - sizeF.Width, 2f);
			if (DrawDamitID)
			{
				if ((double)num2 < 0.5)
				{
					font = new Font("Arial", 7f);
				}
				if ((double)num2 < 0.4)
				{
					font = new Font("Arial", 6f);
				}
				double num3 = double.Parse(((Control)txtHrs).get_Text()) + double.Parse(((Control)txtMin).get_Text()) / 60.0 + PhaseStep * (double)(ImageNumber - 4) / (360.0 / ModelParameters[Currently_Displayed_Model].Period_hrs);
				if (num3 < 0.0)
				{
					num3 += 24.0;
				}
				string text2 = PlotLabel + ((Control)lblModel).get_Text() + ": " + ((Control)lblPhaseOffset[ImageNumber]).get_Text() + " = " + Utilities.DEGtoDMS(num3, 2, 1, MinutesOnly: true).Insert(2, "h") + "m UT";
				sizeF = graphics.MeasureString(text2, font);
				graphics.FillRectangle(brush2, 4f, 3f, sizeF.Width + 2f, sizeF.Height + 2f);
				graphics.DrawString(text2, font, brush, 5f, 4f);
			}
			if (IncludePhaseEffects)
			{
				text = "Sun-Lit";
				sizeF = graphics.MeasureString(text, font);
				graphics.FillRectangle(Brushes.White, (float)(((Control)picDisplay).get_Width() - 10) - sizeF.Width, 18f, sizeF.Width + 4f, sizeF.Height);
				graphics.DrawString(text, font, brush, (float)(((Control)picDisplay).get_Width() - 8) - sizeF.Width, 18f);
			}
			AsteroidDiameterByVol_AsFractionOfPlotWidth = (float)(VolumeEquivalent_Diameter_Pixels[num] / (double)ImageSize);
			AsteroidDiameterBySurf_AsFractionOfPlotWidth = (float)(SurfaceEquivalent_Diameter_Pixels[num] / (double)ImageSize);
			if (DrawDAMITMean)
			{
				Pen pen = new Pen(Color.White, 1.8f);
				if (BlackToWhiteBackground > 0)
				{
					pen = new Pen(Color.Black, 1.8f);
				}
				float num4 = (float)VolumeEquivalent_Diameter_Pixels[num] / ImageSize / 2f;
				float num5 = (float)SurfaceEquivalent_Diameter_Pixels[num] / ImageSize / 2f;
				if (num4 > 0f)
				{
					if (num5 == 0f)
					{
						num5 = num4;
					}
					if (num5 <= num4)
					{
						graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f - num4), ((Control)picDisplay).get_Height() - 30, (float)((Control)picDisplay).get_Width() * (0.5f + num4), ((Control)picDisplay).get_Height() - 30);
					}
					else
					{
						graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f - num5), ((Control)picDisplay).get_Height() - 30, (float)((Control)picDisplay).get_Width() * (0.5f + num5), ((Control)picDisplay).get_Height() - 30);
					}
					graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f - num4), ((Control)picDisplay).get_Height() - 34, (float)((Control)picDisplay).get_Width() * (0.5f - num4), ((Control)picDisplay).get_Height() - 30);
					graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f + num4), ((Control)picDisplay).get_Height() - 34, (float)((Control)picDisplay).get_Width() * (0.5f + num4), ((Control)picDisplay).get_Height() - 30);
					graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f - num5), ((Control)picDisplay).get_Height() - 30, (float)((Control)picDisplay).get_Width() * (0.5f - num5), ((Control)picDisplay).get_Height() - 25);
					graphics.DrawLine(pen, (float)((Control)picDisplay).get_Width() * (0.5f + num5), ((Control)picDisplay).get_Height() - 30, (float)((Control)picDisplay).get_Width() * (0.5f + num5), ((Control)picDisplay).get_Height() - 25);
					string text3 = " Mean diameter by: Vol↑ Surf↓";
					font = new Font("Arial", 7f);
					sizeF = graphics.MeasureString(text3, font);
					graphics.FillRectangle(brush2, ((float)((Control)picDisplay).get_Width() - sizeF.Width) / 2f, ((Control)picDisplay).get_Height() - 27, sizeF.Width, sizeF.Height);
					graphics.DrawString(text3, font, brush, ((float)((Control)picDisplay).get_Width() - sizeF.Width) / 2f - 1f, (float)((Control)picDisplay).get_Height() - 27f);
				}
			}
			picDisplay.set_Image((Image)image);
			CurrentImage = ImageNumber;
			if (Data_and_Plots.PlotForm != null)
			{
				if (Data_and_Plots.PlotForm.addShapeImageFromDAMIT_ISAMToolStripMenuItem.get_Checked())
				{
					Data_and_Plots.PlotEventOnScreen();
				}
				Application.DoEvents();
			}
			IsTransferring = false;
		}

		private void DisplayShapeModels_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Height() > 250)
			{
				((Control)lstModels).set_Height(((Control)this).get_Height() - 205);
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			CancelPhaseDownloads = true;
			Application.DoEvents();
		}

		private void copyEventJDToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText("Model Epoch = " + string.Format("{0,1:f6}", ModelParameters[Currently_Displayed_Model].JDat0) + "\r\nModel Phase at Epoch = " + string.Format("{0,1:f1}°", ModelParameters[Currently_Displayed_Model].PhaseAt0) + "\r\nDaily rotation rate = " + string.Format("{0,1:f7}°/day", ModelParameters[Currently_Displayed_Model].DailyRotationRate) + "\r\nBest fit:\r\n" + ((Control)lblJD).get_Text() + "\r\nPhase correction (revs) = " + updnZeroPhase.get_Value() + "°\r\nRotation rate correction = " + updnRotnRate.get_Value() + "°\r\n\r\n");
		}

		private void animateDisplayOfPhaseModelsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			animateDisplayOfPhaseModelsToolStripMenuItem.set_Checked(!animateDisplayOfPhaseModelsToolStripMenuItem.get_Checked());
			timerPhase.set_Enabled(animateDisplayOfPhaseModelsToolStripMenuItem.get_Checked());
			if (timerPhase.get_Enabled())
			{
				((Control)cmdCancelAnimation).set_Visible(true);
				timerPhase.set_Interval(AnimationInterval);
				TimerCount = CurrentPhaseImage;
			}
		}

		private void timerPhase_Tick(object sender, EventArgs e)
		{
			Application.DoEvents();
			TimerCount++;
			if (TimerCount >= PhaseImages.Count)
			{
				TimerCount = 0;
			}
			TransferPhaseImage(TimerCount);
			int interval = 1500;
			if (AnimationInterval > 800)
			{
				interval = 2 * AnimationInterval;
			}
			if (TimerCount == PhaseImages.Count - 1)
			{
				timerPhase.set_Interval(interval);
			}
			else
			{
				timerPhase.set_Interval(AnimationInterval / 4);
			}
		}

		private void cmdCancelAnimation_Click(object sender, EventArgs e)
		{
			((Control)cmdCancelAnimation).set_Visible(false);
			Timer obj = timerPhase;
			bool enabled;
			animateDisplayOfPhaseModelsToolStripMenuItem.set_Checked(enabled = false);
			obj.set_Enabled(enabled);
		}

		private void animatedGIFSetupToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			GIFsettings gIFsettings = new GIFsettings();
			((Form)gIFsettings).ShowDialog();
			((Component)(object)gIFsettings).Dispose();
		}

		private void saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (Data_and_Plots.PlotForm != null)
			{
				CreateAnimatedGif(AgainstObservations: true);
			}
		}

		private void saveAnimatedGIFOfPhaseModelsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			CreateAnimatedGif(AgainstObservations: false);
		}

		private void CreateAnimatedGif(bool AgainstObservations)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			if (timerPhase.get_Enabled())
			{
				MessageBox.Show("Cannot create an animated GIF file while the shape models are being cyclically displayed", "Can't create Animated GIF", (MessageBoxButtons)0, (MessageBoxIcon)64);
				return;
			}
			int currentPhaseImage = CurrentPhaseImage;
			List<string> list = new List<string>();
			int num = 10000;
			string path = Utilities.AppPath + "\\Predictions\\AnimatedGIF";
			if (!Directory.Exists(path))
			{
				Directory.CreateDirectory(path);
			}
			string[] files = Directory.GetFiles(path, "*.gif");
			foreach (string path2 in files)
			{
				try
				{
					File.Delete(path2);
				}
				catch
				{
				}
			}
			for (int j = 0; j < PhaseImages.Count; j++)
			{
				TransferPhaseImage(j);
				if (AgainstObservations)
				{
					((Control)Data_and_Plots.PlotForm).Focus();
				}
				string text = Utilities.AppPath + "\\Predictions\\AnimatedGIF\\" + num + ".gif";
				try
				{
					if (AgainstObservations)
					{
						Data_and_Plots.PlotForm.picPlot.get_Image().Save(text, ImageFormat.Gif);
					}
					else
					{
						picDisplay.get_Image().Save(text, ImageFormat.Gif);
					}
					list.Add(text);
				}
				catch
				{
				}
				num++;
			}
			global::GifCreator.GifCreator.Create_Animated_Gif(list, string.Concat(str2: (Data_and_Plots.PlotForm == null) ? (((Control)txtYear).get_Text() + ((Control)txtMonth).get_Text().PadLeft(2, '_') + ((Control)txtDay).get_Text().PadLeft(2, '_') + ((Control)txtHrs).get_Text().PadLeft(2, '_') + ((Control)txtMin).get_Text().PadLeft(2, '_') + ((Control)lblModel).get_Text() + "_" + ((Control)cmdDownloadModels).get_Text().Substring(12)) : (Data_and_Plots.PlotLabel.Replace("  ", "_").Replace(" ", "") + ((Control)lblModel).get_Text() + "_" + ((Control)cmdDownloadModels).get_Text().Substring(12)), str0: Utilities.AppPath, str1: "\\Asteroids\\", str3: ".gif"));
			TransferPhaseImage(currentPhaseImage);
		}

		private void secToolStripMenuItem01_Click(object sender, EventArgs e)
		{
			SetAnimationInterval(100);
		}

		private void secToolStripMenuItem02_Click(object sender, EventArgs e)
		{
			SetAnimationInterval(200);
		}

		private void secToolStripMenuItem03_Click(object sender, EventArgs e)
		{
			SetAnimationInterval(300);
		}

		private void secToolStripMenuItem05_Click(object sender, EventArgs e)
		{
			SetAnimationInterval(500);
		}

		private void secToolStripMenuItem10_Click(object sender, EventArgs e)
		{
			SetAnimationInterval(1000);
		}

		private void secToolStripMenuItem20_Click(object sender, EventArgs e)
		{
			SetAnimationInterval(2000);
		}

		private void secToolStripMenuItem30_Click(object sender, EventArgs e)
		{
			SetAnimationInterval(3000);
		}

		private void secToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			SetAnimationInterval(4000);
		}

		private void SetAnimationInterval(int Interval)
		{
			Settings.Default.AnimationInterval = (AnimationInterval = Interval);
			((ToolStripItem)setAnimationIntervalToolStripMenuItem).set_Text(string.Format("Set animation interval [ {0,1:f1} sec ]", (double)Interval / 1000.0));
		}

		private void listSatelliteIRDiametersToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((ListControl)lstModels).get_SelectedIndex() >= 0)
			{
				Utilities.Display_IR_AsteroidDiameter(GetShapeModelData.ShapeModelList[((ListControl)lstModels).get_SelectedIndex()].Asteroid_number, ShowInForm: true, out var _);
			}
		}

		private void showDAMITFitsToLightCurvesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DisplayDAMITLightCurveFits();
		}

		internal static void DisplayDAMITLightCurveFits()
		{
			//IL_019d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a4: Expected O, but got Unknown
			//IL_0320: Unknown result type (might be due to invalid IL or missing references)
			int num = 1;
			int num2 = 1;
			int num3 = 0;
			int num4 = 25;
			int i = 0;
			ArrayList arrayList = new ArrayList();
			Bitmap bitmap = new Bitmap(1, 1);
			Graphics graphics = Graphics.FromImage(bitmap);
			string text = "Light curve fits";
			if (Currently_Displayed_Model >= Asteroid_Observations_Reports.Display_ShapeModels.NumDAMITImages)
			{
				return;
			}
			Utilities.SetAnimatedCursor(ref i);
			string text2 = "";
			ServicePointManager.SecurityProtocol = SecurityProtocolType.Ssl3 | SecurityProtocolType.Tls | SecurityProtocolType.Tls11 | SecurityProtocolType.Tls12;
			HttpWebRequest httpWebRequest = (HttpWebRequest)WebRequest.Create($"https://astro.troja.mff.cuni.cz/projects/damit/asteroid_models/lcfit/{CurrentlySelected_DAMITModelNumbers[Currently_Displayed_Model]}");
			httpWebRequest.Method = "GET";
			try
			{
				using WebResponse webResponse = httpWebRequest.GetResponse();
				using Stream stream = webResponse.GetResponseStream();
				using StreamReader streamReader = new StreamReader(stream);
				try
				{
					text2 = streamReader.ReadToEnd();
					int num5 = 0;
					int num6 = 0;
					int num7 = 0;
					do
					{
						num5 = text2.IndexOf("<img src=", num6);
						if (num5 < 0)
						{
							break;
						}
						num5 = text2.IndexOf("/", num5);
						num6 = text2.IndexOf(".png", num5);
						if (num6 < 0)
						{
							break;
						}
						if (!text2.Substring(num5, num6 - num5 + 4).Contains("logo"))
						{
							arrayList.Add(text2.Substring(num5, num6 - num5 + 4));
							num7++;
						}
					}
					while (num6 > 0);
					if (num7 > 0)
					{
						PBar pBar = new PBar();
						((Control)pBar).set_Text($"Downloading {num7} light curve fits");
						pBar.pBarFTP.set_Minimum(0);
						pBar.pBarFTP.set_Maximum(num7);
						((Control)pBar).Show();
						num3 = (num7 - 1) / 3 + 1;
						for (int j = 0; j < num7; j++)
						{
							Utilities.SetAnimatedCursor(ref i);
							pBar.pBarFTP.set_Value(j);
							PictureBox val = new PictureBox();
							val.Load("https://astro.troja.mff.cuni.cz" + arrayList[j]!.ToString());
							if (j == 0)
							{
								num2 = (int)(3.3 * (double)((Control)val).get_Width() + 20.0);
								num = (int)(5.5 * (double)((Control)val).get_Height());
								bitmap = new Bitmap(3 * num2, num3 * num + num4);
								graphics = Graphics.FromImage(bitmap);
								graphics.Clear(Color.White);
								text = $"Light curve fits for asteroid ({CurrentlySelectedAsteroid}), Model {CurrentlySelected_DAMITModelNumbers[Currently_Displayed_Model]}";
								Font font = new Font("Courier New", 15f, FontStyle.Bold);
								graphics.DrawString(text, font, Brushes.DarkGreen, ((float)bitmap.Width - graphics.MeasureString(text, font).Width) / 2f, 2f);
							}
							graphics.DrawImage(val.get_Image(), new Point(j % 3 * num2, j / 3 * num + num4));
							Application.DoEvents();
							((Component)(object)val).Dispose();
						}
						((Form)pBar).Close();
						((Component)(object)pBar).Dispose();
						graphics.Dispose();
					}
				}
				catch
				{
				}
			}
			catch
			{
				MessageBox.Show("The DAMIT site has not responded to the\r\nrequest for light curve data", "DAMIT error", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			Cursor.set_Current(Cursors.get_Default());
			try
			{
				((Control)ImageDisplay).Show();
			}
			catch
			{
				ImageDisplay = new DisplayImage();
				((Control)ImageDisplay).Show();
			}
			ImageDisplay.picBox.set_Image((Image)bitmap);
			((Control)ImageDisplay.picBox).set_Size(bitmap.Size);
			((Control)ImageDisplay).set_Width(bitmap.Width);
			((Control)ImageDisplay).set_Text(text);
			ImageDisplay.SaveName = $"Shape_LC_asteroid_{CurrentlySelectedAsteroid_DAMIT_ID}_Model_{CurrentlySelected_DAMITModelNumbers[Currently_Displayed_Model]}";
			ImageDisplay.Path = Utilities.AppPath + "/Downloaded Files/";
		}

		private void txtYear_Leave(object sender, EventArgs e)
		{
			Compute_Phase_Adjustment(Currently_Displayed_Model);
		}

		private void txtMonth_Leave(object sender, EventArgs e)
		{
			Compute_Phase_Adjustment(Currently_Displayed_Model);
		}

		private void txtDay_Leave(object sender, EventArgs e)
		{
			Compute_Phase_Adjustment(Currently_Displayed_Model);
		}

		private void txtHrs_Leave(object sender, EventArgs e)
		{
			Compute_Phase_Adjustment(Currently_Displayed_Model);
		}

		private void txtMin_Leave(object sender, EventArgs e)
		{
			Compute_Phase_Adjustment(Currently_Displayed_Model);
		}

		private void plotOnEarthPlaneToolStripMenuItem_Click(object sender, EventArgs e)
		{
			plotOnEarthPlaneToolStripMenuItem.set_Checked(!plotOnEarthPlaneToolStripMenuItem.get_Checked());
			PlotOnEarthPlane = plotOnEarthPlaneToolStripMenuItem.get_Checked();
			TransferImage(CurrentImage);
		}

		internal void mnuWhiteBackground_Click(object sender, EventArgs e)
		{
			mnuWhiteBackground.set_Checked(true);
			ToolStripMenuItem obj = mnuBlackBackground;
			bool @checked;
			mnuGrayBackground.set_Checked(@checked = false);
			obj.set_Checked(@checked);
			SetBackground_White_Black(1);
		}

		private void mnuBlackBackground_Click(object sender, EventArgs e)
		{
			mnuBlackBackground.set_Checked(true);
			ToolStripMenuItem obj = mnuWhiteBackground;
			bool @checked;
			mnuGrayBackground.set_Checked(@checked = false);
			obj.set_Checked(@checked);
			SetBackground_White_Black(0);
		}

		private void mnuGrayBackground_Click(object sender, EventArgs e)
		{
			mnuGrayBackground.set_Checked(true);
			ToolStripMenuItem obj = mnuBlackBackground;
			bool @checked;
			mnuWhiteBackground.set_Checked(@checked = false);
			obj.set_Checked(@checked);
			SetBackground_White_Black(2);
		}

		internal void SetBackgroundChecks(int ColorIndex)
		{
			mnuBlackBackground.set_Checked(ColorIndex == 0);
			mnuWhiteBackground.set_Checked(ColorIndex == 1);
			mnuGrayBackground.set_Checked(ColorIndex == 2);
		}

		internal void SetBackground_White_Black(int ColorIndex)
		{
			int currentImage = CurrentImage;
			int num2 = (BlackToWhiteBackground = (Settings.Default.ShapeModelBackColor = ColorIndex));
			if (((Control)pnlPhaseChange).get_Visible())
			{
				GetPhaseAdjustedModels(PhaseStep);
				return;
			}
			GetModels();
			TransferImage(currentImage);
		}

		private void plotModelInGreyScale_Click(object sender, EventArgs e)
		{
			GreyScalePlotModel();
		}

		internal void GreyScalePlotModel()
		{
			int currentImage = CurrentImage;
			Settings @default = Settings.Default;
			bool blackWhiteModel;
			plotModelInGreyScale.set_Checked(blackWhiteModel = !plotModelInGreyScale.get_Checked());
			@default.ModelinBlackWhite = (BlackWhiteModel = blackWhiteModel);
			if (((Control)pnlPhaseChange).get_Visible())
			{
				GetPhaseAdjustedModels(PhaseStep);
				return;
			}
			GetModels();
			TransferImage(currentImage);
		}

		private void plotModelDarkToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DarkPlotModel();
		}

		internal void DarkPlotModel()
		{
			int currentImage = CurrentImage;
			Settings @default = Settings.Default;
			bool plotModelDark;
			plotModelDarkToolStripMenuItem.set_Checked(plotModelDark = !plotModelDarkToolStripMenuItem.get_Checked());
			@default.ModelDark = (PlotModelDark = plotModelDark);
			if (((Control)pnlPhaseChange).get_Visible())
			{
				GetPhaseAdjustedModels(PhaseStep);
				return;
			}
			GetModels();
			TransferImage(currentImage);
		}

		private void ShowFaceEdgesOnModelToolStripMenuItem_Click(object sender, EventArgs e)
		{
			ShowModelFaces();
		}

		internal void ShowModelFaces()
		{
			int currentImage = CurrentImage;
			Settings @default = Settings.Default;
			bool showModelFaceEdges;
			showFaceEdgesOnModelToolStripMenuItem.set_Checked(showModelFaceEdges = !showFaceEdgesOnModelToolStripMenuItem.get_Checked());
			@default.ModelFaceEdges = (ShowModelFaceEdges = showModelFaceEdges);
			if (((Control)pnlPhaseChange).get_Visible())
			{
				GetPhaseAdjustedModels(PhaseStep);
				return;
			}
			GetModels();
			TransferImage(currentImage);
		}

		private void updateFileOfAvailableModelsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			GetShapeModelData.CreateDAMITModelList();
			GetShapeModelData.FillDAMITAsteroidModels();
			InitialiseShapeModels();
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void downloadAllAvailableModelsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetShapeModelData.DownloadAllDAMITModels();
			InitialiseShapeModels();
		}

		private void updateListOfISAMShapeModelsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			if (http.Get_ISAM_Asteroids(ForceUpdate: true))
			{
				MessageBox.Show("ISAM file update successful", "ISAM Download");
			}
			InitialiseShapeModels();
		}

		private void dwnloadAllmissingShapeModelsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetShapeModelData.RefreshISAMDownoadedModels();
			InitialiseShapeModels();
		}

		private void DisplayShapeModels_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
			//IL_005a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0060: Invalid comparison between Unknown and I4
			bool flag = false;
			foreach (Form item in (ReadOnlyCollectionBase)(object)Application.get_OpenForms())
			{
				if (item is ObservationsEditor)
				{
					flag = true;
					break;
				}
			}
			if (flag && (int)MessageBox.Show("This shape model display form should not be closed while the Asteroid Observations Editor is being used.\r\n\r\nDo you want to close this form?", "Confirm form closure", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				((CancelEventArgs)(object)e).Cancel = true;
				return;
			}
			try
			{
				Settings.Default.LocationIRDiameter = ((Form)Utilities.DataBoxDia).get_Location();
				((Form)Utilities.DataBoxDia).Close();
			}
			catch
			{
			}
			try
			{
				((Form)ImageDisplay).Close();
				((Component)(object)ImageDisplay).Dispose();
			}
			catch
			{
			}
		}

		private void toolStripMenuItem16_Click(object sender, EventArgs e)
		{
			ClearAllLambdaChecks();
			toolStripMenuItem16.set_Checked(true);
			dLambda = 16;
			Replot();
		}

		private void toolStripMenuItem2_Click(object sender, EventArgs e)
		{
			ClearAllLambdaChecks();
			toolStripMenuItem2.set_Checked(true);
			dLambda = 12;
			Replot();
		}

		private void toolStripMenuItem3_Click(object sender, EventArgs e)
		{
			ClearAllLambdaChecks();
			toolStripMenuItem3.set_Checked(true);
			dLambda = 8;
			Replot();
		}

		private void toolStripMenuItem4_Click(object sender, EventArgs e)
		{
			ClearAllLambdaChecks();
			toolStripMenuItem4.set_Checked(true);
			dLambda = 4;
			Replot();
		}

		private void toolStripMenuItem5_Click(object sender, EventArgs e)
		{
			ClearAllLambdaChecks();
			toolStripMenuItem5.set_Checked(true);
			dLambda = 0;
			Replot();
		}

		private void toolStripMenuItem6_Click(object sender, EventArgs e)
		{
			ClearAllLambdaChecks();
			toolStripMenuItem6.set_Checked(true);
			dLambda = -4;
			Replot();
		}

		private void toolStripMenuItem7_Click(object sender, EventArgs e)
		{
			ClearAllLambdaChecks();
			toolStripMenuItem7.set_Checked(true);
			dLambda = -8;
			Replot();
		}

		private void toolStripMenuItem8_Click(object sender, EventArgs e)
		{
			ClearAllLambdaChecks();
			toolStripMenuItem8.set_Checked(true);
			dLambda = -12;
			Replot();
		}

		private void toolStripMenuItem17_Click(object sender, EventArgs e)
		{
			ClearAllLambdaChecks();
			toolStripMenuItem17.set_Checked(true);
			dLambda = -16;
			Replot();
		}

		internal void ClearAllLambdaChecks()
		{
			ToolStripMenuItem obj = toolStripMenuItem2;
			ToolStripMenuItem obj2 = toolStripMenuItem3;
			ToolStripMenuItem obj3 = toolStripMenuItem4;
			ToolStripMenuItem obj4 = toolStripMenuItem5;
			ToolStripMenuItem obj5 = toolStripMenuItem6;
			ToolStripMenuItem obj6 = toolStripMenuItem7;
			ToolStripMenuItem obj7 = toolStripMenuItem8;
			ToolStripMenuItem obj8 = toolStripMenuItem16;
			bool flag;
			toolStripMenuItem17.set_Checked(flag = false);
			bool flag2;
			obj8.set_Checked(flag2 = flag);
			bool flag3;
			obj7.set_Checked(flag3 = flag2);
			bool flag4;
			obj6.set_Checked(flag4 = flag3);
			bool flag5;
			obj5.set_Checked(flag5 = flag4);
			bool flag6;
			obj4.set_Checked(flag6 = flag5);
			bool flag7;
			obj3.set_Checked(flag7 = flag6);
			bool @checked;
			obj2.set_Checked(@checked = flag7);
			obj.set_Checked(@checked);
		}

		internal void Replot()
		{
			int currentImage = CurrentImage;
			if (((Control)pnlPhaseChange).get_Visible())
			{
				GetPhaseAdjustedModels(PhaseStep);
				return;
			}
			GetModels();
			TransferImage(currentImage);
		}

		private void toolStripMenuItem18_Click(object sender, EventArgs e)
		{
			ClearAllBetaChecks();
			toolStripMenuItem18.set_Checked(true);
			dBeta = 16;
			Replot();
		}

		private void toolStripMenuItem9_Click(object sender, EventArgs e)
		{
			ClearAllBetaChecks();
			toolStripMenuItem9.set_Checked(true);
			dBeta = 12;
			Replot();
		}

		private void toolStripMenuItem10_Click(object sender, EventArgs e)
		{
			ClearAllBetaChecks();
			toolStripMenuItem10.set_Checked(true);
			dBeta = 8;
			Replot();
		}

		private void toolStripMenuItem11_Click(object sender, EventArgs e)
		{
			ClearAllBetaChecks();
			toolStripMenuItem11.set_Checked(true);
			dBeta = 4;
			Replot();
		}

		private void toolStripMenuItem12_Click(object sender, EventArgs e)
		{
			ClearAllBetaChecks();
			toolStripMenuItem12.set_Checked(true);
			dBeta = 0;
			Replot();
		}

		private void toolStripMenuItem13_Click(object sender, EventArgs e)
		{
			ClearAllBetaChecks();
			toolStripMenuItem13.set_Checked(true);
			dBeta = -4;
			Replot();
		}

		private void toolStripMenuItem14_Click(object sender, EventArgs e)
		{
			ClearAllBetaChecks();
			toolStripMenuItem14.set_Checked(true);
			dBeta = -8;
			Replot();
		}

		private void toolStripMenuItem15_Click(object sender, EventArgs e)
		{
			ClearAllBetaChecks();
			toolStripMenuItem15.set_Checked(true);
			dBeta = -12;
			Replot();
		}

		private void toolStripMenuItem19_Click(object sender, EventArgs e)
		{
			ClearAllBetaChecks();
			toolStripMenuItem19.set_Checked(true);
			dBeta = -16;
			Replot();
		}

		internal void ClearAllBetaChecks()
		{
			ToolStripMenuItem obj = toolStripMenuItem9;
			ToolStripMenuItem obj2 = toolStripMenuItem10;
			ToolStripMenuItem obj3 = toolStripMenuItem11;
			ToolStripMenuItem obj4 = toolStripMenuItem12;
			ToolStripMenuItem obj5 = toolStripMenuItem13;
			ToolStripMenuItem obj6 = toolStripMenuItem14;
			ToolStripMenuItem obj7 = toolStripMenuItem15;
			ToolStripMenuItem obj8 = toolStripMenuItem18;
			bool flag;
			toolStripMenuItem19.set_Checked(flag = false);
			bool flag2;
			obj8.set_Checked(flag2 = flag);
			bool flag3;
			obj7.set_Checked(flag3 = flag2);
			bool flag4;
			obj6.set_Checked(flag4 = flag3);
			bool flag5;
			obj5.set_Checked(flag5 = flag4);
			bool flag6;
			obj4.set_Checked(flag6 = flag5);
			bool flag7;
			obj3.set_Checked(flag7 = flag6);
			bool @checked;
			obj2.set_Checked(@checked = flag7);
			obj.set_Checked(@checked);
		}

		private void trackOpacity_ValueChanged(object sender, EventArgs e)
		{
			((Form)this).set_Opacity((double)trackOpacity.get_Value() / 100.0);
			if (((Form)this).get_Opacity() < 0.98)
			{
				((Control)this).set_BackColor(Color.White);
				Label obj = lblDAMITisam;
				Label obj2 = lblAvailableAsteroids;
				Panel obj3 = pnlAdjust;
				bool flag;
				((Control)lstModels).set_Visible(flag = false);
				bool flag2;
				((Control)obj3).set_Visible(flag2 = flag);
				bool visible;
				((Control)obj2).set_Visible(visible = flag2);
				((Control)obj).set_Visible(visible);
				picDisplay.set_BorderStyle((BorderStyle)0);
				TrackBar obj4 = trackBarImageSize;
				Color darkRed;
				((Control)trackOpacity).set_BackColor(darkRed = Color.DarkRed);
				((Control)obj4).set_BackColor(darkRed);
				((ToolStrip)menuStrip1).set_BackColor(Color.DarkBlue);
				MenuStrip obj5 = menuStrip1;
				TrackBar obj6 = trackBarImageSize;
				Color white;
				((Control)trackOpacity).set_ForeColor(white = Color.White);
				((Control)obj6).set_ForeColor(darkRed = white);
				((ToolStrip)obj5).set_ForeColor(darkRed);
			}
			else
			{
				MenuStrip obj7 = menuStrip1;
				Color darkRed;
				((Control)this).set_BackColor(darkRed = SystemColors.Control);
				((ToolStrip)obj7).set_BackColor(darkRed);
				Label obj8 = lblDAMITisam;
				Label obj9 = lblAvailableAsteroids;
				Panel obj10 = pnlAdjust;
				bool flag;
				((Control)lstModels).set_Visible(flag = true);
				bool flag2;
				((Control)obj10).set_Visible(flag2 = flag);
				bool visible;
				((Control)obj9).set_Visible(visible = flag2);
				((Control)obj8).set_Visible(visible);
				picDisplay.set_BorderStyle((BorderStyle)2);
				TrackBar obj11 = trackBarImageSize;
				((Control)trackOpacity).set_BackColor(darkRed = Color.LemonChiffon);
				((Control)obj11).set_BackColor(darkRed);
				MenuStrip obj12 = menuStrip1;
				TrackBar obj13 = trackBarImageSize;
				Color white;
				((Control)trackOpacity).set_ForeColor(white = Color.Black);
				((Control)obj13).set_ForeColor(darkRed = white);
				((ToolStrip)obj12).set_ForeColor(darkRed);
			}
		}

		private void getAsteroidFromDAMITToolStripMenuItem_Click(object sender, EventArgs e)
		{
			GetShapeModelData.ShowDAMITtoAsteroid();
		}

		private void DisplayShapeModels_Activated(object sender, EventArgs e)
		{
			IncludeAxisToolStripMenuItem.set_Checked(IncludeAxisOfRotation = Settings.Default.IncludeAxisOfRotation);
			showAsteroidAndModelIDOnImageToolStripMenuItem.set_Checked(DrawDamitID = Settings.Default.DrawDamitID);
			showFaceEdgesOnModelToolStripMenuItem.set_Checked(ShowModelFaceEdges = Settings.Default.ModelFaceEdges);
			plotModelDarkToolStripMenuItem.set_Checked(PlotModelDark = Settings.Default.ModelDark);
			plotModelInGreyScale.set_Checked(BlackWhiteModel = Settings.Default.ModelinBlackWhite);
		}

		private void convertASingleDAMITSourceFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			if (GetShapeModelData.ManuallyConvertDAMITmodel())
			{
				Initialise_ShapeModels();
				MessageBox.Show("Shape model file has been created and incorporated into the Occult files");
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
			//IL_04b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c0: Expected O, but got Unknown
			//IL_04c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04cb: Expected O, but got Unknown
			//IL_04cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d6: Expected O, but got Unknown
			//IL_04d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e1: Expected O, but got Unknown
			//IL_04e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ec: Expected O, but got Unknown
			//IL_04ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f7: Expected O, but got Unknown
			//IL_04f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0502: Expected O, but got Unknown
			//IL_0503: Unknown result type (might be due to invalid IL or missing references)
			//IL_050d: Expected O, but got Unknown
			//IL_050e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0518: Expected O, but got Unknown
			//IL_0519: Unknown result type (might be due to invalid IL or missing references)
			//IL_0523: Expected O, but got Unknown
			//IL_0524: Unknown result type (might be due to invalid IL or missing references)
			//IL_052e: Expected O, but got Unknown
			//IL_052f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0539: Expected O, but got Unknown
			//IL_053a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0544: Expected O, but got Unknown
			//IL_0545: Unknown result type (might be due to invalid IL or missing references)
			//IL_054f: Expected O, but got Unknown
			//IL_0550: Unknown result type (might be due to invalid IL or missing references)
			//IL_055a: Expected O, but got Unknown
			//IL_055b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0565: Expected O, but got Unknown
			//IL_0566: Unknown result type (might be due to invalid IL or missing references)
			//IL_0570: Expected O, but got Unknown
			//IL_0571: Unknown result type (might be due to invalid IL or missing references)
			//IL_057b: Expected O, but got Unknown
			//IL_057c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0586: Expected O, but got Unknown
			//IL_0587: Unknown result type (might be due to invalid IL or missing references)
			//IL_0591: Expected O, but got Unknown
			//IL_0592: Unknown result type (might be due to invalid IL or missing references)
			//IL_059c: Expected O, but got Unknown
			//IL_2945: Unknown result type (might be due to invalid IL or missing references)
			//IL_294f: Expected O, but got Unknown
			//IL_404a: Unknown result type (might be due to invalid IL or missing references)
			//IL_4054: Expected O, but got Unknown
			//IL_406b: Unknown result type (might be due to invalid IL or missing references)
			//IL_4075: Expected O, but got Unknown
			//IL_40df: Unknown result type (might be due to invalid IL or missing references)
			//IL_40e9: Expected O, but got Unknown
			components = new Container();
			label6 = new Label();
			txtMin = new TextBox();
			txtHrs = new TextBox();
			label5 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			txtDay = new TextBox();
			txtMonth = new TextBox();
			txtYear = new TextBox();
			menuStrip1 = new MenuStrip();
			withImageToolStripMenuItem = new ToolStripMenuItem();
			dAMITIncludeMeanRadiusOnImageToolStripMenuItem = new ToolStripMenuItem();
			showAsteroidAndModelIDOnImageToolStripMenuItem = new ToolStripMenuItem();
			plotModelDarkToolStripMenuItem = new ToolStripMenuItem();
			plotModelInGreyScale = new ToolStripMenuItem();
			mnuBlackBackground = new ToolStripMenuItem();
			mnuGrayBackground = new ToolStripMenuItem();
			mnuWhiteBackground = new ToolStripMenuItem();
			showFaceEdgesOnModelToolStripMenuItem = new ToolStripMenuItem();
			showSolarIlluminationPhaseISAMOnlyToolStripMenuItem = new ToolStripMenuItem();
			IncludeAxisToolStripMenuItem = new ToolStripMenuItem();
			plotOnEarthPlaneToolStripMenuItem = new ToolStripMenuItem();
			changePolarAxisOrientationToolStripMenuItem = new ToolStripMenuItem();
			lambdaToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem16 = new ToolStripMenuItem();
			toolStripMenuItem2 = new ToolStripMenuItem();
			toolStripMenuItem3 = new ToolStripMenuItem();
			toolStripMenuItem4 = new ToolStripMenuItem();
			toolStripMenuItem5 = new ToolStripMenuItem();
			toolStripMenuItem6 = new ToolStripMenuItem();
			toolStripMenuItem7 = new ToolStripMenuItem();
			toolStripMenuItem8 = new ToolStripMenuItem();
			toolStripMenuItem17 = new ToolStripMenuItem();
			toolStripSeparator3 = new ToolStripSeparator();
			toolStripSeparator5 = new ToolStripSeparator();
			betaToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem18 = new ToolStripMenuItem();
			toolStripMenuItem9 = new ToolStripMenuItem();
			toolStripMenuItem10 = new ToolStripMenuItem();
			toolStripMenuItem11 = new ToolStripMenuItem();
			toolStripMenuItem12 = new ToolStripMenuItem();
			toolStripMenuItem13 = new ToolStripMenuItem();
			toolStripMenuItem14 = new ToolStripMenuItem();
			toolStripMenuItem15 = new ToolStripMenuItem();
			toolStripMenuItem19 = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			listSatelliteIRDiametersToolStripMenuItem = new ToolStripMenuItem();
			listDAMITToolStripMenuItem = new ToolStripMenuItem();
			showDAMITFitsToLightCurvesToolStripMenuItem = new ToolStripMenuItem();
			getAsteroidFromDAMITToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			copyEventJDToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			phaseImagesToolStripMenuItem = new ToolStripMenuItem();
			downloadPhaseAt1036StepsToolStripMenuItem = new ToolStripMenuItem();
			drawPhaseModelsAt16StepsToolStripMenuItem = new ToolStripMenuItem();
			downloadPhaseAt518StepsToolStripMenuItem = new ToolStripMenuItem();
			downloadPhaseAt272StepsToolStripMenuItem = new ToolStripMenuItem();
			downloadPhaseAt136StepsToolStripMenuItem = new ToolStripMenuItem();
			downloadPhaseAt062StepsToolStripMenuItem = new ToolStripMenuItem();
			closePhaseModelSelectorToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator6 = new ToolStripSeparator();
			animateDisplayOfPhaseModelsToolStripMenuItem = new ToolStripMenuItem();
			setAnimationIntervalToolStripMenuItem = new ToolStripMenuItem();
			secToolStripMenuItem01 = new ToolStripMenuItem();
			secToolStripMenuItem027 = new ToolStripMenuItem();
			secToolStripMenuItem03 = new ToolStripMenuItem();
			secToolStripMenuItem05 = new ToolStripMenuItem();
			secToolStripMenuItem10 = new ToolStripMenuItem();
			secToolStripMenuItem20 = new ToolStripMenuItem();
			secToolStripMenuItem30 = new ToolStripMenuItem();
			secToolStripMenuItem40 = new ToolStripMenuItem();
			toolStripSeparator7 = new ToolStripSeparator();
			animatedGIFSetupToolStripMenuItem = new ToolStripMenuItem();
			saveAnimatedGIFOfPhaseModelsToolStripMenuItem = new ToolStripMenuItem();
			saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem = new ToolStripMenuItem();
			keepOnTopToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			shapeModelMaintenanceToolStripMenuItem = new ToolStripMenuItem();
			dAMITToolStripMenuItem = new ToolStripMenuItem();
			updateFileOfAvailableModelsToolStripMenuItem = new ToolStripMenuItem();
			downloadAllAvailableModelsToolStripMenuItem = new ToolStripMenuItem();
			convertASingleDAMITSourceFileToolStripMenuItem = new ToolStripMenuItem();
			iSAMToolStripMenuItem = new ToolStripMenuItem();
			updateListOfISAMShapeModelsToolStripMenuItem = new ToolStripMenuItem();
			dwnloadAllmissingShapeModelsToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lblAvailableAsteroids = new Label();
			lstModels = new ListBox();
			label7 = new Label();
			cmdDownloadModels = new Button();
			trackBarImageSize = new TrackBar();
			lblModel = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			lblSelect = new Label();
			lblPhaseAngle = new Label();
			label16 = new Label();
			txtDeltaT = new TextBox();
			label17 = new Label();
			pnlPhaseChange = new Panel();
			btnCancel = new Button();
			cmdCancel = new Button();
			lblJD = new Label();
			picDisplay = new PictureBox();
			timerPhase = new Timer(components);
			cmdCancelAnimation = new Button();
			pnlAdjust = new Panel();
			label11 = new Label();
			txtPhaseOffset = new TextBox();
			label24 = new Label();
			label25 = new Label();
			label23 = new Label();
			label22 = new Label();
			updnRotnRate = new NumericUpDown();
			updnZeroPhase = new NumericUpDown();
			lblDAMITisam = new Label();
			label4 = new Label();
			panel1 = new Panel();
			trackOpacity = new TrackBar();
			label20 = new Label();
			label13 = new Label();
			label14 = new Label();
			label15 = new Label();
			label18 = new Label();
			panel2 = new Panel();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)trackBarImageSize).BeginInit();
			((Control)pnlPhaseChange).SuspendLayout();
			((ISupportInitialize)picDisplay).BeginInit();
			((Control)pnlAdjust).SuspendLayout();
			((ISupportInitialize)updnRotnRate).BeginInit();
			((ISupportInitialize)updnZeroPhase).BeginInit();
			((ISupportInitialize)trackOpacity).BeginInit();
			((Control)this).SuspendLayout();
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(109, 40));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(24, 13));
			((Control)label6).set_TabIndex(72);
			((Control)label6).set_Text("Min");
			((Control)txtMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMin).set_Location(new Point(112, 53));
			((Control)txtMin).set_Name("txtMin");
			((Control)txtMin).set_Size(new Size(19, 20));
			((Control)txtMin).set_TabIndex(73);
			((Control)txtMin).set_Text("0");
			txtMin.set_TextAlign((HorizontalAlignment)2);
			((Control)txtMin).add_Leave((EventHandler)txtMin_Leave);
			((Control)txtHrs).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtHrs).set_Location(new Point(89, 53));
			((Control)txtHrs).set_Name("txtHrs");
			((Control)txtHrs).set_Size(new Size(19, 20));
			((Control)txtHrs).set_TabIndex(71);
			((Control)txtHrs).set_Text("0");
			txtHrs.set_TextAlign((HorizontalAlignment)2);
			((Control)txtHrs).add_Leave((EventHandler)txtHrs_Leave);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(89, 40));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(18, 13));
			((Control)label5).set_TabIndex(70);
			((Control)label5).set_Text("Hr");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(62, 40));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(26, 13));
			((Control)label3).set_TabIndex(68);
			((Control)label3).set_Text("Day");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(40, 40));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(25, 13));
			((Control)label2).set_TabIndex(66);
			((Control)label2).set_Text("Mth");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(9, 40));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(29, 13));
			((Control)label1).set_TabIndex(64);
			((Control)label1).set_Text("Year");
			((Control)txtDay).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDay).set_Location(new Point(66, 53));
			((Control)txtDay).set_Name("txtDay");
			((Control)txtDay).set_Size(new Size(19, 20));
			((Control)txtDay).set_TabIndex(69);
			((Control)txtDay).set_Text("1");
			txtDay.set_TextAlign((HorizontalAlignment)2);
			((Control)txtDay).add_Leave((EventHandler)txtDay_Leave);
			((Control)txtMonth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMonth).set_Location(new Point(43, 53));
			((Control)txtMonth).set_Name("txtMonth");
			((Control)txtMonth).set_Size(new Size(19, 20));
			((Control)txtMonth).set_TabIndex(67);
			((Control)txtMonth).set_Text("1");
			txtMonth.set_TextAlign((HorizontalAlignment)2);
			((Control)txtMonth).add_Leave((EventHandler)txtMonth_Leave);
			((Control)txtYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtYear).set_Location(new Point(7, 53));
			((Control)txtYear).set_Name("txtYear");
			((Control)txtYear).set_Size(new Size(32, 20));
			((Control)txtYear).set_TabIndex(65);
			((Control)txtYear).set_Text("1000");
			((Control)txtYear).add_Leave((EventHandler)txtYear_Leave);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)withImageToolStripMenuItem,
				(ToolStripItem)phaseImagesToolStripMenuItem,
				(ToolStripItem)keepOnTopToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)shapeModelMaintenanceToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(849, 24));
			((Control)menuStrip1).set_TabIndex(77);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withImageToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[21]
			{
				(ToolStripItem)IncludeAxisToolStripMenuItem,
				(ToolStripItem)showAsteroidAndModelIDOnImageToolStripMenuItem,
				(ToolStripItem)dAMITIncludeMeanRadiusOnImageToolStripMenuItem,
				(ToolStripItem)plotModelDarkToolStripMenuItem,
				(ToolStripItem)plotModelInGreyScale,
				(ToolStripItem)mnuBlackBackground,
				(ToolStripItem)mnuGrayBackground,
				(ToolStripItem)mnuWhiteBackground,
				(ToolStripItem)showFaceEdgesOnModelToolStripMenuItem,
				(ToolStripItem)showSolarIlluminationPhaseISAMOnlyToolStripMenuItem,
				(ToolStripItem)plotOnEarthPlaneToolStripMenuItem,
				(ToolStripItem)changePolarAxisOrientationToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)listSatelliteIRDiametersToolStripMenuItem,
				(ToolStripItem)listDAMITToolStripMenuItem,
				(ToolStripItem)showDAMITFitsToLightCurvesToolStripMenuItem,
				(ToolStripItem)getAsteroidFromDAMITToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)copyEventJDToolStripMenuItem,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withImageToolStripMenuItem).set_Name("withImageToolStripMenuItem");
			((ToolStripItem)withImageToolStripMenuItem).set_Size(new Size(136, 20));
			((ToolStripItem)withImageToolStripMenuItem).set_Text("with Model image...    ");
			((ToolStripItem)dAMITIncludeMeanRadiusOnImageToolStripMenuItem).set_Name("dAMITIncludeMeanRadiusOnImageToolStripMenuItem");
			((ToolStripItem)dAMITIncludeMeanRadiusOnImageToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)dAMITIncludeMeanRadiusOnImageToolStripMenuItem).set_Text("Draw mean Diameter on image ");
			((ToolStripItem)dAMITIncludeMeanRadiusOnImageToolStripMenuItem).add_Click((EventHandler)dAMITIncludeMeanRadiusOnImageToolStripMenuItem_Click);
			showAsteroidAndModelIDOnImageToolStripMenuItem.set_Checked(true);
			showAsteroidAndModelIDOnImageToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)showAsteroidAndModelIDOnImageToolStripMenuItem).set_Name("showAsteroidAndModelIDOnImageToolStripMenuItem");
			((ToolStripItem)showAsteroidAndModelIDOnImageToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)showAsteroidAndModelIDOnImageToolStripMenuItem).set_Text("Show asteroid and Model ID on image");
			((ToolStripItem)showAsteroidAndModelIDOnImageToolStripMenuItem).add_Click((EventHandler)showAsteroidAndModelIDOnImageToolStripMenuItem_Click);
			((ToolStripItem)plotModelDarkToolStripMenuItem).set_Name("plotModelDarkToolStripMenuItem");
			((ToolStripItem)plotModelDarkToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)plotModelDarkToolStripMenuItem).set_Text("Plot model Dark");
			((ToolStripItem)plotModelDarkToolStripMenuItem).add_Click((EventHandler)plotModelDarkToolStripMenuItem_Click);
			((ToolStripItem)plotModelInGreyScale).set_Name("plotModelInGreyScale");
			((ToolStripItem)plotModelInGreyScale).set_Size(new Size(296, 22));
			((ToolStripItem)plotModelInGreyScale).set_Text("Plot model in Grey scale");
			((ToolStripItem)plotModelInGreyScale).add_Click((EventHandler)plotModelInGreyScale_Click);
			((ToolStripItem)mnuBlackBackground).set_Name("mnuBlackBackground");
			((ToolStripItem)mnuBlackBackground).set_Size(new Size(296, 22));
			((ToolStripItem)mnuBlackBackground).set_Text("Plot on BLACK background");
			((ToolStripItem)mnuBlackBackground).add_Click((EventHandler)mnuBlackBackground_Click);
			((ToolStripItem)mnuGrayBackground).set_Name("mnuGrayBackground");
			((ToolStripItem)mnuGrayBackground).set_Size(new Size(296, 22));
			((ToolStripItem)mnuGrayBackground).set_Text("Plot on GRAY background");
			((ToolStripItem)mnuGrayBackground).add_Click((EventHandler)mnuGrayBackground_Click);
			((ToolStripItem)mnuWhiteBackground).set_Name("mnuWhiteBackground");
			((ToolStripItem)mnuWhiteBackground).set_Size(new Size(296, 22));
			((ToolStripItem)mnuWhiteBackground).set_Text("Plot on WHITE background");
			((ToolStripItem)mnuWhiteBackground).add_Click((EventHandler)mnuWhiteBackground_Click);
			((ToolStripItem)showFaceEdgesOnModelToolStripMenuItem).set_Name("showFaceEdgesOnModelToolStripMenuItem");
			((ToolStripItem)showFaceEdgesOnModelToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)showFaceEdgesOnModelToolStripMenuItem).set_Text("Show face edges on model");
			((ToolStripItem)showFaceEdgesOnModelToolStripMenuItem).add_Click((EventHandler)ShowFaceEdgesOnModelToolStripMenuItem_Click);
			((ToolStripItem)showSolarIlluminationPhaseISAMOnlyToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)showSolarIlluminationPhaseISAMOnlyToolStripMenuItem).set_Name("showSolarIlluminationPhaseISAMOnlyToolStripMenuItem");
			((ToolStripItem)showSolarIlluminationPhaseISAMOnlyToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)showSolarIlluminationPhaseISAMOnlyToolStripMenuItem).set_Text("Show as lit by the Sun");
			((ToolStripItem)showSolarIlluminationPhaseISAMOnlyToolStripMenuItem).add_Click((EventHandler)showSolarIlluminationPhaseISAMOnlyToolStripMenuItem_Click);
			((ToolStripItem)IncludeAxisToolStripMenuItem).set_Name("IncludeAxisToolStripMenuItem");
			((ToolStripItem)IncludeAxisToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)IncludeAxisToolStripMenuItem).set_Text("Show polar axes");
			((ToolStripItem)IncludeAxisToolStripMenuItem).add_Click((EventHandler)IncludeAxisToolStripMenuItem_Click);
			((ToolStripItem)plotOnEarthPlaneToolStripMenuItem).set_Name("plotOnEarthPlaneToolStripMenuItem");
			((ToolStripItem)plotOnEarthPlaneToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)plotOnEarthPlaneToolStripMenuItem).set_Text("Plot on Earth Plane");
			((ToolStripItem)plotOnEarthPlaneToolStripMenuItem).add_Click((EventHandler)plotOnEarthPlaneToolStripMenuItem_Click);
			((ToolStripDropDownItem)changePolarAxisOrientationToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[22]
			{
				(ToolStripItem)lambdaToolStripMenuItem,
				(ToolStripItem)toolStripMenuItem16,
				(ToolStripItem)toolStripMenuItem2,
				(ToolStripItem)toolStripMenuItem3,
				(ToolStripItem)toolStripMenuItem4,
				(ToolStripItem)toolStripMenuItem5,
				(ToolStripItem)toolStripMenuItem6,
				(ToolStripItem)toolStripMenuItem7,
				(ToolStripItem)toolStripMenuItem8,
				(ToolStripItem)toolStripMenuItem17,
				(ToolStripItem)toolStripSeparator3,
				(ToolStripItem)toolStripSeparator5,
				(ToolStripItem)betaToolStripMenuItem,
				(ToolStripItem)toolStripMenuItem18,
				(ToolStripItem)toolStripMenuItem9,
				(ToolStripItem)toolStripMenuItem10,
				(ToolStripItem)toolStripMenuItem11,
				(ToolStripItem)toolStripMenuItem12,
				(ToolStripItem)toolStripMenuItem13,
				(ToolStripItem)toolStripMenuItem14,
				(ToolStripItem)toolStripMenuItem15,
				(ToolStripItem)toolStripMenuItem19
			});
			((ToolStripItem)changePolarAxisOrientationToolStripMenuItem).set_Name("changePolarAxisOrientationToolStripMenuItem");
			((ToolStripItem)changePolarAxisOrientationToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)changePolarAxisOrientationToolStripMenuItem).set_Text("Change polar axis orientation");
			((ToolStripItem)lambdaToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)lambdaToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold | FontStyle.Italic));
			((ToolStripItem)lambdaToolStripMenuItem).set_Name("lambdaToolStripMenuItem");
			((ToolStripItem)lambdaToolStripMenuItem).set_Size(new Size(119, 22));
			((ToolStripItem)lambdaToolStripMenuItem).set_Text("Lambda");
			((ToolStripItem)toolStripMenuItem16).set_Name("toolStripMenuItem16");
			((ToolStripItem)toolStripMenuItem16).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem16).set_Text("+16°");
			((ToolStripItem)toolStripMenuItem16).add_Click((EventHandler)toolStripMenuItem16_Click);
			((ToolStripItem)toolStripMenuItem2).set_Name("toolStripMenuItem2");
			((ToolStripItem)toolStripMenuItem2).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem2).set_Text("+12°");
			((ToolStripItem)toolStripMenuItem2).add_Click((EventHandler)toolStripMenuItem2_Click);
			((ToolStripItem)toolStripMenuItem3).set_Name("toolStripMenuItem3");
			((ToolStripItem)toolStripMenuItem3).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem3).set_Text("+8°");
			((ToolStripItem)toolStripMenuItem3).add_Click((EventHandler)toolStripMenuItem3_Click);
			((ToolStripItem)toolStripMenuItem4).set_Name("toolStripMenuItem4");
			((ToolStripItem)toolStripMenuItem4).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem4).set_Text("+4°");
			((ToolStripItem)toolStripMenuItem4).add_Click((EventHandler)toolStripMenuItem4_Click);
			toolStripMenuItem5.set_Checked(true);
			toolStripMenuItem5.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem5).set_Name("toolStripMenuItem5");
			((ToolStripItem)toolStripMenuItem5).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem5).set_Text("0°");
			((ToolStripItem)toolStripMenuItem5).add_Click((EventHandler)toolStripMenuItem5_Click);
			((ToolStripItem)toolStripMenuItem6).set_Name("toolStripMenuItem6");
			((ToolStripItem)toolStripMenuItem6).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem6).set_Text("-4°");
			((ToolStripItem)toolStripMenuItem6).add_Click((EventHandler)toolStripMenuItem6_Click);
			((ToolStripItem)toolStripMenuItem7).set_Name("toolStripMenuItem7");
			((ToolStripItem)toolStripMenuItem7).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem7).set_Text("-8°");
			((ToolStripItem)toolStripMenuItem7).add_Click((EventHandler)toolStripMenuItem7_Click);
			((ToolStripItem)toolStripMenuItem8).set_Name("toolStripMenuItem8");
			((ToolStripItem)toolStripMenuItem8).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem8).set_Text("-12°");
			((ToolStripItem)toolStripMenuItem8).add_Click((EventHandler)toolStripMenuItem8_Click);
			((ToolStripItem)toolStripMenuItem17).set_Name("toolStripMenuItem17");
			((ToolStripItem)toolStripMenuItem17).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem17).set_Text("-16°");
			((ToolStripItem)toolStripMenuItem17).add_Click((EventHandler)toolStripMenuItem17_Click);
			((ToolStripItem)toolStripSeparator3).set_Name("toolStripSeparator3");
			((ToolStripItem)toolStripSeparator3).set_Size(new Size(116, 6));
			((ToolStripItem)toolStripSeparator5).set_Name("toolStripSeparator5");
			((ToolStripItem)toolStripSeparator5).set_Size(new Size(116, 6));
			((ToolStripItem)betaToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)betaToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold | FontStyle.Italic));
			((ToolStripItem)betaToolStripMenuItem).set_Name("betaToolStripMenuItem");
			((ToolStripItem)betaToolStripMenuItem).set_Size(new Size(119, 22));
			((ToolStripItem)betaToolStripMenuItem).set_Text("Beta");
			((ToolStripItem)toolStripMenuItem18).set_Name("toolStripMenuItem18");
			((ToolStripItem)toolStripMenuItem18).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem18).set_Text("+16°");
			((ToolStripItem)toolStripMenuItem18).add_Click((EventHandler)toolStripMenuItem18_Click);
			((ToolStripItem)toolStripMenuItem9).set_Name("toolStripMenuItem9");
			((ToolStripItem)toolStripMenuItem9).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem9).set_Text("+12°");
			((ToolStripItem)toolStripMenuItem9).add_Click((EventHandler)toolStripMenuItem9_Click);
			((ToolStripItem)toolStripMenuItem10).set_Name("toolStripMenuItem10");
			((ToolStripItem)toolStripMenuItem10).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem10).set_Text("+8°");
			((ToolStripItem)toolStripMenuItem10).add_Click((EventHandler)toolStripMenuItem10_Click);
			((ToolStripItem)toolStripMenuItem11).set_Name("toolStripMenuItem11");
			((ToolStripItem)toolStripMenuItem11).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem11).set_Text("+4°");
			((ToolStripItem)toolStripMenuItem11).add_Click((EventHandler)toolStripMenuItem11_Click);
			toolStripMenuItem12.set_Checked(true);
			toolStripMenuItem12.set_CheckState((CheckState)1);
			((ToolStripItem)toolStripMenuItem12).set_Name("toolStripMenuItem12");
			((ToolStripItem)toolStripMenuItem12).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem12).set_Text("0");
			((ToolStripItem)toolStripMenuItem12).add_Click((EventHandler)toolStripMenuItem12_Click);
			((ToolStripItem)toolStripMenuItem13).set_Name("toolStripMenuItem13");
			((ToolStripItem)toolStripMenuItem13).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem13).set_Text("-4°");
			((ToolStripItem)toolStripMenuItem13).add_Click((EventHandler)toolStripMenuItem13_Click);
			((ToolStripItem)toolStripMenuItem14).set_Name("toolStripMenuItem14");
			((ToolStripItem)toolStripMenuItem14).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem14).set_Text("-8°");
			((ToolStripItem)toolStripMenuItem14).add_Click((EventHandler)toolStripMenuItem14_Click);
			((ToolStripItem)toolStripMenuItem15).set_Name("toolStripMenuItem15");
			((ToolStripItem)toolStripMenuItem15).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem15).set_Text("-12°");
			((ToolStripItem)toolStripMenuItem15).add_Click((EventHandler)toolStripMenuItem15_Click);
			((ToolStripItem)toolStripMenuItem19).set_Name("toolStripMenuItem19");
			((ToolStripItem)toolStripMenuItem19).set_Size(new Size(119, 22));
			((ToolStripItem)toolStripMenuItem19).set_Text("-16°");
			((ToolStripItem)toolStripMenuItem19).add_Click((EventHandler)toolStripMenuItem19_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(293, 6));
			((ToolStripItem)listSatelliteIRDiametersToolStripMenuItem).set_Name("listSatelliteIRDiametersToolStripMenuItem");
			((ToolStripItem)listSatelliteIRDiametersToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)listSatelliteIRDiametersToolStripMenuItem).set_Text("List Diameters from IR-satellites");
			((ToolStripItem)listSatelliteIRDiametersToolStripMenuItem).add_Click((EventHandler)listSatelliteIRDiametersToolStripMenuItem_Click);
			((ToolStripItem)listDAMITToolStripMenuItem).set_Name("listDAMITToolStripMenuItem");
			((ToolStripItem)listDAMITToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)listDAMITToolStripMenuItem).set_Text("List model parameters && light-curve dates");
			((ToolStripItem)listDAMITToolStripMenuItem).add_Click((EventHandler)listDAMITToolStripMenuItem_Click);
			((ToolStripItem)showDAMITFitsToLightCurvesToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)showDAMITFitsToLightCurvesToolStripMenuItem).set_Name("showDAMITFitsToLightCurvesToolStripMenuItem");
			((ToolStripItem)showDAMITFitsToLightCurvesToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)showDAMITFitsToLightCurvesToolStripMenuItem).set_Text("Display model fit to light curves (DAMIT)");
			((ToolStripItem)showDAMITFitsToLightCurvesToolStripMenuItem).add_Click((EventHandler)showDAMITFitsToLightCurvesToolStripMenuItem_Click);
			((ToolStripItem)getAsteroidFromDAMITToolStripMenuItem).set_Name("getAsteroidFromDAMITToolStripMenuItem");
			((ToolStripItem)getAsteroidFromDAMITToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)getAsteroidFromDAMITToolStripMenuItem).set_Text("Get asteroid from DAMIT #");
			((ToolStripItem)getAsteroidFromDAMITToolStripMenuItem).add_Click((EventHandler)getAsteroidFromDAMITToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(293, 6));
			((ToolStripItem)copyEventJDToolStripMenuItem).set_Name("copyEventJDToolStripMenuItem");
			((ToolStripItem)copyEventJDToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)copyEventJDToolStripMenuItem).set_Text("Copy EventJD, model epoch, && period");
			((ToolStripItem)copyEventJDToolStripMenuItem).add_Click((EventHandler)copyEventJDToolStripMenuItem_Click);
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy image");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(296, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save image");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)phaseImagesToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[14]
			{
				(ToolStripItem)downloadPhaseAt1036StepsToolStripMenuItem,
				(ToolStripItem)drawPhaseModelsAt16StepsToolStripMenuItem,
				(ToolStripItem)downloadPhaseAt518StepsToolStripMenuItem,
				(ToolStripItem)downloadPhaseAt272StepsToolStripMenuItem,
				(ToolStripItem)downloadPhaseAt136StepsToolStripMenuItem,
				(ToolStripItem)downloadPhaseAt062StepsToolStripMenuItem,
				(ToolStripItem)closePhaseModelSelectorToolStripMenuItem,
				(ToolStripItem)toolStripSeparator6,
				(ToolStripItem)animateDisplayOfPhaseModelsToolStripMenuItem,
				(ToolStripItem)setAnimationIntervalToolStripMenuItem,
				(ToolStripItem)toolStripSeparator7,
				(ToolStripItem)animatedGIFSetupToolStripMenuItem,
				(ToolStripItem)saveAnimatedGIFOfPhaseModelsToolStripMenuItem,
				(ToolStripItem)saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem
			});
			((ToolStripItem)phaseImagesToolStripMenuItem).set_Name("phaseImagesToolStripMenuItem");
			((ToolStripItem)phaseImagesToolStripMenuItem).set_Size(new Size(136, 20));
			((ToolStripItem)phaseImagesToolStripMenuItem).set_Text("with Phase models...   ");
			((ToolStripItem)downloadPhaseAt1036StepsToolStripMenuItem).set_Name("downloadPhaseAt1036StepsToolStripMenuItem");
			((ToolStripItem)downloadPhaseAt1036StepsToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)downloadPhaseAt1036StepsToolStripMenuItem).set_Text("Draw phase models at 36° steps");
			((ToolStripItem)downloadPhaseAt1036StepsToolStripMenuItem).add_Click((EventHandler)downloadPhaseAt1036StepsToolStripMenuItem_Click);
			((ToolStripItem)drawPhaseModelsAt16StepsToolStripMenuItem).set_Name("drawPhaseModelsAt16StepsToolStripMenuItem");
			((ToolStripItem)drawPhaseModelsAt16StepsToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)drawPhaseModelsAt16StepsToolStripMenuItem).set_Text("Draw phase models at 16° steps");
			((ToolStripItem)drawPhaseModelsAt16StepsToolStripMenuItem).add_Click((EventHandler)drawPhaseModelsAt16StepsToolStripMenuItem_Click);
			((ToolStripItem)downloadPhaseAt518StepsToolStripMenuItem).set_Name("downloadPhaseAt518StepsToolStripMenuItem");
			((ToolStripItem)downloadPhaseAt518StepsToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)downloadPhaseAt518StepsToolStripMenuItem).set_Text("Draw phase models at 8° steps");
			((ToolStripItem)downloadPhaseAt518StepsToolStripMenuItem).add_Click((EventHandler)downloadPhaseAt518StepsToolStripMenuItem_Click);
			((ToolStripItem)downloadPhaseAt272StepsToolStripMenuItem).set_Name("downloadPhaseAt272StepsToolStripMenuItem");
			((ToolStripItem)downloadPhaseAt272StepsToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)downloadPhaseAt272StepsToolStripMenuItem).set_Text("Draw phase models at 4° steps");
			((ToolStripItem)downloadPhaseAt272StepsToolStripMenuItem).add_Click((EventHandler)downloadPhaseAt272StepsToolStripMenuItem_Click);
			((ToolStripItem)downloadPhaseAt136StepsToolStripMenuItem).set_Name("downloadPhaseAt136StepsToolStripMenuItem");
			((ToolStripItem)downloadPhaseAt136StepsToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)downloadPhaseAt136StepsToolStripMenuItem).set_Text("Draw phase models at 2° steps");
			((ToolStripItem)downloadPhaseAt136StepsToolStripMenuItem).add_Click((EventHandler)downloadPhaseAt136StepsToolStripMenuItem_Click);
			((ToolStripItem)downloadPhaseAt062StepsToolStripMenuItem).set_Name("downloadPhaseAt062StepsToolStripMenuItem");
			((ToolStripItem)downloadPhaseAt062StepsToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)downloadPhaseAt062StepsToolStripMenuItem).set_Text("Draw phase models at 1° steps");
			((ToolStripItem)downloadPhaseAt062StepsToolStripMenuItem).add_Click((EventHandler)downloadPhaseAt062StepsToolStripMenuItem_Click);
			((ToolStripItem)closePhaseModelSelectorToolStripMenuItem).set_Name("closePhaseModelSelectorToolStripMenuItem");
			((ToolStripItem)closePhaseModelSelectorToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)closePhaseModelSelectorToolStripMenuItem).set_Text("Close phase model display control");
			((ToolStripItem)closePhaseModelSelectorToolStripMenuItem).add_Click((EventHandler)closePhaseModelSelectorToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator6).set_ForeColor(SystemColors.ControlText);
			((ToolStripItem)toolStripSeparator6).set_Name("toolStripSeparator6");
			((ToolStripItem)toolStripSeparator6).set_Size(new Size(437, 6));
			((ToolStripItem)animateDisplayOfPhaseModelsToolStripMenuItem).set_BackColor(Color.Cornsilk);
			((ToolStripItem)animateDisplayOfPhaseModelsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)animateDisplayOfPhaseModelsToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)animateDisplayOfPhaseModelsToolStripMenuItem).set_Name("animateDisplayOfPhaseModelsToolStripMenuItem");
			((ToolStripItem)animateDisplayOfPhaseModelsToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)animateDisplayOfPhaseModelsToolStripMenuItem).set_Text("Animate display of Phase models");
			((ToolStripItem)animateDisplayOfPhaseModelsToolStripMenuItem).add_Click((EventHandler)animateDisplayOfPhaseModelsToolStripMenuItem_Click);
			((ToolStripItem)setAnimationIntervalToolStripMenuItem).set_BackColor(Color.Cornsilk);
			((ToolStripDropDownItem)setAnimationIntervalToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)secToolStripMenuItem01,
				(ToolStripItem)secToolStripMenuItem027,
				(ToolStripItem)secToolStripMenuItem03,
				(ToolStripItem)secToolStripMenuItem05,
				(ToolStripItem)secToolStripMenuItem10,
				(ToolStripItem)secToolStripMenuItem20,
				(ToolStripItem)secToolStripMenuItem30,
				(ToolStripItem)secToolStripMenuItem40
			});
			((ToolStripItem)setAnimationIntervalToolStripMenuItem).set_Name("setAnimationIntervalToolStripMenuItem");
			((ToolStripItem)setAnimationIntervalToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)setAnimationIntervalToolStripMenuItem).set_Text("Set animation interval");
			((ToolStripItem)secToolStripMenuItem01).set_Name("secToolStripMenuItem01");
			((ToolStripItem)secToolStripMenuItem01).set_Size(new Size(109, 22));
			((ToolStripItem)secToolStripMenuItem01).set_Text("0.1 sec");
			((ToolStripItem)secToolStripMenuItem01).add_Click((EventHandler)secToolStripMenuItem01_Click);
			((ToolStripItem)secToolStripMenuItem027).set_Name("secToolStripMenuItem027");
			((ToolStripItem)secToolStripMenuItem027).set_Size(new Size(109, 22));
			((ToolStripItem)secToolStripMenuItem027).set_Text("0.2 sec");
			((ToolStripItem)secToolStripMenuItem027).add_Click((EventHandler)secToolStripMenuItem02_Click);
			((ToolStripItem)secToolStripMenuItem03).set_Name("secToolStripMenuItem03");
			((ToolStripItem)secToolStripMenuItem03).set_Size(new Size(109, 22));
			((ToolStripItem)secToolStripMenuItem03).set_Text("0.3 sec");
			((ToolStripItem)secToolStripMenuItem03).add_Click((EventHandler)secToolStripMenuItem03_Click);
			((ToolStripItem)secToolStripMenuItem05).set_Name("secToolStripMenuItem05");
			((ToolStripItem)secToolStripMenuItem05).set_Size(new Size(109, 22));
			((ToolStripItem)secToolStripMenuItem05).set_Text("0.5 sec");
			((ToolStripItem)secToolStripMenuItem05).add_Click((EventHandler)secToolStripMenuItem05_Click);
			((ToolStripItem)secToolStripMenuItem10).set_Name("secToolStripMenuItem10");
			((ToolStripItem)secToolStripMenuItem10).set_Size(new Size(109, 22));
			((ToolStripItem)secToolStripMenuItem10).set_Text("1.0 sec");
			((ToolStripItem)secToolStripMenuItem10).add_Click((EventHandler)secToolStripMenuItem10_Click);
			((ToolStripItem)secToolStripMenuItem20).set_Name("secToolStripMenuItem20");
			((ToolStripItem)secToolStripMenuItem20).set_Size(new Size(109, 22));
			((ToolStripItem)secToolStripMenuItem20).set_Text("2.0 sec");
			((ToolStripItem)secToolStripMenuItem20).add_Click((EventHandler)secToolStripMenuItem20_Click);
			((ToolStripItem)secToolStripMenuItem30).set_Name("secToolStripMenuItem30");
			((ToolStripItem)secToolStripMenuItem30).set_Size(new Size(109, 22));
			((ToolStripItem)secToolStripMenuItem30).set_Text("3.0 sec");
			((ToolStripItem)secToolStripMenuItem30).add_Click((EventHandler)secToolStripMenuItem30_Click);
			((ToolStripItem)secToolStripMenuItem40).set_Name("secToolStripMenuItem40");
			((ToolStripItem)secToolStripMenuItem40).set_Size(new Size(109, 22));
			((ToolStripItem)secToolStripMenuItem40).set_Text("4.0 sec");
			((ToolStripItem)secToolStripMenuItem40).add_Click((EventHandler)secToolStripMenuItem1_Click);
			((ToolStripItem)toolStripSeparator7).set_Name("toolStripSeparator7");
			((ToolStripItem)toolStripSeparator7).set_Size(new Size(437, 6));
			((ToolStripItem)animatedGIFSetupToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)animatedGIFSetupToolStripMenuItem).set_Name("animatedGIFSetupToolStripMenuItem");
			((ToolStripItem)animatedGIFSetupToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)animatedGIFSetupToolStripMenuItem).set_Text("Animated GIF  -  Setup");
			((ToolStripItem)animatedGIFSetupToolStripMenuItem).add_Click((EventHandler)animatedGIFSetupToolStripMenuItem_Click);
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsToolStripMenuItem).set_Name("saveAnimatedGIFOfPhaseModelsToolStripMenuItem");
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsToolStripMenuItem).set_Text("Save phase models as an animated GIF");
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsToolStripMenuItem).add_Click((EventHandler)saveAnimatedGIFOfPhaseModelsToolStripMenuItem_Click);
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem).set_Name("saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem");
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem).set_Size(new Size(440, 22));
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem).set_Text("Save phase models as an animated GIF, against plot of occultations");
			((ToolStripItem)saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem).add_Click((EventHandler)saveAnimatedGIFOfPhaseModelsAgainstObservationsToolStripMenuItem_Click);
			((ToolStripItem)keepOnTopToolStripMenuItem).set_Image((Image)Resources.FillUpHS);
			((ToolStripItem)keepOnTopToolStripMenuItem).set_Name("keepOnTopToolStripMenuItem");
			((ToolStripItem)keepOnTopToolStripMenuItem).set_Size(new Size(137, 20));
			((ToolStripItem)keepOnTopToolStripMenuItem).set_Text("Keep form on top   ");
			((ToolStripItem)keepOnTopToolStripMenuItem).add_Click((EventHandler)keepOnTopToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(75, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help     ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripDropDownItem)shapeModelMaintenanceToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)dAMITToolStripMenuItem,
				(ToolStripItem)iSAMToolStripMenuItem
			});
			((ToolStripItem)shapeModelMaintenanceToolStripMenuItem).set_Name("shapeModelMaintenanceToolStripMenuItem");
			((ToolStripItem)shapeModelMaintenanceToolStripMenuItem).set_Size(new Size(172, 20));
			((ToolStripItem)shapeModelMaintenanceToolStripMenuItem).set_Text("Shape Model Maintenance    ");
			((ToolStripDropDownItem)dAMITToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)updateFileOfAvailableModelsToolStripMenuItem,
				(ToolStripItem)downloadAllAvailableModelsToolStripMenuItem,
				(ToolStripItem)convertASingleDAMITSourceFileToolStripMenuItem
			});
			((ToolStripItem)dAMITToolStripMenuItem).set_Name("dAMITToolStripMenuItem");
			((ToolStripItem)dAMITToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)dAMITToolStripMenuItem).set_Text("DAMIT");
			((ToolStripItem)updateFileOfAvailableModelsToolStripMenuItem).set_Name("updateFileOfAvailableModelsToolStripMenuItem");
			((ToolStripItem)updateFileOfAvailableModelsToolStripMenuItem).set_Size(new Size(277, 22));
			((ToolStripItem)updateFileOfAvailableModelsToolStripMenuItem).set_Text("Update list of available  DAMIT models");
			((ToolStripItem)updateFileOfAvailableModelsToolStripMenuItem).add_Click((EventHandler)updateFileOfAvailableModelsToolStripMenuItem_Click);
			((ToolStripItem)downloadAllAvailableModelsToolStripMenuItem).set_Name("downloadAllAvailableModelsToolStripMenuItem");
			((ToolStripItem)downloadAllAvailableModelsToolStripMenuItem).set_Size(new Size(277, 22));
			((ToolStripItem)downloadAllAvailableModelsToolStripMenuItem).set_Text("Download/update shape-model files");
			((ToolStripItem)downloadAllAvailableModelsToolStripMenuItem).add_Click((EventHandler)downloadAllAvailableModelsToolStripMenuItem_Click);
			((ToolStripItem)convertASingleDAMITSourceFileToolStripMenuItem).set_Name("convertASingleDAMITSourceFileToolStripMenuItem");
			((ToolStripItem)convertASingleDAMITSourceFileToolStripMenuItem).set_Size(new Size(277, 22));
			((ToolStripItem)convertASingleDAMITSourceFileToolStripMenuItem).set_Text("Convert && add a single DAMIT model");
			((ToolStripItem)convertASingleDAMITSourceFileToolStripMenuItem).add_Click((EventHandler)convertASingleDAMITSourceFileToolStripMenuItem_Click);
			((ToolStripDropDownItem)iSAMToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)updateListOfISAMShapeModelsToolStripMenuItem,
				(ToolStripItem)dwnloadAllmissingShapeModelsToolStripMenuItem
			});
			((ToolStripItem)iSAMToolStripMenuItem).set_Name("iSAMToolStripMenuItem");
			((ToolStripItem)iSAMToolStripMenuItem).set_Size(new Size(110, 22));
			((ToolStripItem)iSAMToolStripMenuItem).set_Text("ISAM");
			((ToolStripItem)updateListOfISAMShapeModelsToolStripMenuItem).set_Name("updateListOfISAMShapeModelsToolStripMenuItem");
			((ToolStripItem)updateListOfISAMShapeModelsToolStripMenuItem).set_Size(new Size(372, 22));
			((ToolStripItem)updateListOfISAMShapeModelsToolStripMenuItem).set_Text("Create  list of available ISAM shape models not in DAMIT");
			((ToolStripItem)updateListOfISAMShapeModelsToolStripMenuItem).add_Click((EventHandler)updateListOfISAMShapeModelsToolStripMenuItem_Click);
			((ToolStripItem)dwnloadAllmissingShapeModelsToolStripMenuItem).set_Name("dwnloadAllmissingShapeModelsToolStripMenuItem");
			((ToolStripItem)dwnloadAllmissingShapeModelsToolStripMenuItem).set_Size(new Size(372, 22));
			((ToolStripItem)dwnloadAllmissingShapeModelsToolStripMenuItem).set_Text("Download/Add ISAM shape model files");
			((ToolStripItem)dwnloadAllmissingShapeModelsToolStripMenuItem).add_Click((EventHandler)dwnloadAllmissingShapeModelsToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lblAvailableAsteroids).set_AutoSize(true);
			((Control)lblAvailableAsteroids).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblAvailableAsteroids).set_Location(new Point(3, 131));
			((Control)lblAvailableAsteroids).set_Name("lblAvailableAsteroids");
			((Control)lblAvailableAsteroids).set_Size(new Size(95, 13));
			((Control)lblAvailableAsteroids).set_TabIndex(80);
			((Control)lblAvailableAsteroids).set_Text("Available asteroids");
			((Control)lstModels).set_Font(new Font("Courier New", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstModels).set_FormattingEnabled(true);
			lstModels.set_ItemHeight(14);
			((Control)lstModels).set_Location(new Point(2, 145));
			((Control)lstModels).set_Name("lstModels");
			((Control)lstModels).set_Size(new Size(231, 592));
			((Control)lstModels).set_TabIndex(79);
			lstModels.add_SelectedIndexChanged((EventHandler)lstModels_SelectedIndexChanged);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(4, 26));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(180, 13));
			((Control)label7).set_TabIndex(83);
			((Control)label7).set_Text("Date and time for shape model");
			((Control)cmdDownloadModels).set_BackColor(Color.Honeydew);
			((Control)cmdDownloadModels).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDownloadModels).set_Location(new Point(619, 28));
			((Control)cmdDownloadModels).set_Name("cmdDownloadModels");
			((Control)cmdDownloadModels).set_Size(new Size(72, 51));
			((Control)cmdDownloadModels).set_TabIndex(87);
			((Control)cmdDownloadModels).set_Text("Display \nmodels");
			((ButtonBase)cmdDownloadModels).set_UseVisualStyleBackColor(false);
			((Control)cmdDownloadModels).add_Click((EventHandler)cmdDownloadModels_Click);
			((Control)trackBarImageSize).set_AutoSize(false);
			((Control)trackBarImageSize).set_BackColor(Color.LemonChiffon);
			((Control)trackBarImageSize).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "ShapeModel_Scale", true, (DataSourceUpdateMode)1));
			trackBarImageSize.set_LargeChange(1);
			((Control)trackBarImageSize).set_Location(new Point(644, 86));
			trackBarImageSize.set_Maximum(150);
			trackBarImageSize.set_Minimum(15);
			((Control)trackBarImageSize).set_Name("trackBarImageSize");
			((Control)trackBarImageSize).set_Size(new Size(200, 36));
			((Control)trackBarImageSize).set_TabIndex(91);
			trackBarImageSize.set_TickFrequency(5);
			trackBarImageSize.set_TickStyle((TickStyle)1);
			trackBarImageSize.set_Value(Settings.Default.ShapeModel_Scale);
			trackBarImageSize.add_Scroll((EventHandler)trackBarImageSize_Scroll);
			((Control)lblModel).set_AutoSize(true);
			((Control)lblModel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblModel).set_ForeColor(Color.SaddleBrown);
			((Control)lblModel).set_Location(new Point(680, 128));
			((Control)lblModel).set_Name("lblModel");
			((Control)lblModel).set_Size(new Size(53, 13));
			((Control)lblModel).set_TabIndex(92);
			((Control)lblModel).set_Text("Model #");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_BackColor(Color.LemonChiffon);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_ForeColor(Color.DarkGreen);
			((Control)label8).set_Location(new Point(707, 109));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(75, 13));
			((Control)label8).set_TabIndex(93);
			((Control)label8).set_Text("Image scale");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_BackColor(Color.LemonChiffon);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_ForeColor(Color.DarkGreen);
			((Control)label9).set_Location(new Point(818, 109));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(25, 13));
			((Control)label9).set_TabIndex(94);
			((Control)label9).set_Text("1.5");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_BackColor(Color.LemonChiffon);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_ForeColor(Color.DarkGreen);
			((Control)label10).set_Location(new Point(646, 110));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(32, 13));
			((Control)label10).set_TabIndex(95);
			((Control)label10).set_Text("0.15");
			((Control)lblSelect).set_AutoSize(true);
			((Control)lblSelect).set_Location(new Point(337, 26));
			((Control)lblSelect).set_Name("lblSelect");
			((Control)lblSelect).set_Size(new Size(181, 13));
			((Control)lblSelect).set_TabIndex(98);
			((Control)lblSelect).set_Text("S e l e c t   m o d e l   t o   d i s p l a y");
			lblSelect.set_TextAlign(ContentAlignment.TopCenter);
			((Control)lblPhaseAngle).set_AutoSize(true);
			((Control)lblPhaseAngle).set_Location(new Point(205, 56));
			((Control)lblPhaseAngle).set_Name("lblPhaseAngle");
			((Control)lblPhaseAngle).set_Size(new Size(66, 13));
			((Control)lblPhaseAngle).set_TabIndex(128);
			((Control)lblPhaseAngle).set_Text("Phase angle");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(163, 40));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(23, 13));
			((Control)label16).set_TabIndex(129);
			((Control)label16).set_Text("Hrs");
			((Control)txtDeltaT).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtDeltaT).set_Location(new Point(148, 53));
			((Control)txtDeltaT).set_Name("txtDeltaT");
			((TextBoxBase)txtDeltaT).set_ReadOnly(true);
			((Control)txtDeltaT).set_Size(new Size(42, 20));
			((Control)txtDeltaT).set_TabIndex(130);
			((Control)txtDeltaT).set_Text("0");
			txtDeltaT.set_TextAlign((HorizontalAlignment)1);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Symbol", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(153, 40));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(12, 13));
			((Control)label17).set_TabIndex(131);
			((Control)label17).set_Text("d");
			pnlPhaseChange.set_BorderStyle((BorderStyle)2);
			((Control)pnlPhaseChange).get_Controls().Add((Control)(object)btnCancel);
			((Control)pnlPhaseChange).set_Location(new Point(288, 39));
			((Control)pnlPhaseChange).set_Name("pnlPhaseChange");
			((Control)pnlPhaseChange).set_Size(new Size(328, 104));
			((Control)pnlPhaseChange).set_TabIndex(142);
			((Control)pnlPhaseChange).set_Visible(false);
			((Control)btnCancel).set_BackgroundImage((Image)Resources.error);
			((Control)btnCancel).set_Location(new Point(307, 0));
			((Control)btnCancel).set_Name("btnCancel");
			((Control)btnCancel).set_Size(new Size(17, 16));
			((Control)btnCancel).set_TabIndex(0);
			((ButtonBase)btnCancel).set_UseVisualStyleBackColor(true);
			((Control)btnCancel).add_Click((EventHandler)btnCancel_Click);
			((Control)cmdCancel).set_BackColor(Color.LightCoral);
			((Control)cmdCancel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCancel).set_Location(new Point(683, 160));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(72, 45));
			((Control)cmdCancel).set_TabIndex(143);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(false);
			((Control)cmdCancel).set_Visible(false);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)lblJD).set_AutoSize(true);
			((Control)lblJD).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblJD).set_Location(new Point(9, 73));
			((Control)lblJD).set_Name("lblJD");
			((Control)lblJD).set_Size(new Size(116, 12));
			((Control)lblJD).set_TabIndex(147);
			((Control)lblJD).set_Text("Event JD = 2415020.50000");
			picDisplay.set_BorderStyle((BorderStyle)2);
			((Control)picDisplay).set_Location(new Point(237, 145));
			((Control)picDisplay).set_Name("picDisplay");
			((Control)picDisplay).set_Size(new Size(600, 600));
			picDisplay.set_TabIndex(86);
			picDisplay.set_TabStop(false);
			((Control)picDisplay).add_Resize((EventHandler)picDisplay_Resize);
			timerPhase.add_Tick((EventHandler)timerPhase_Tick);
			((Control)cmdCancelAnimation).set_BackColor(Color.SandyBrown);
			((Control)cmdCancelAnimation).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCancelAnimation).set_Location(new Point(683, 211));
			((Control)cmdCancelAnimation).set_Name("cmdCancelAnimation");
			((Control)cmdCancelAnimation).set_Size(new Size(72, 45));
			((Control)cmdCancelAnimation).set_TabIndex(148);
			((Control)cmdCancelAnimation).set_Text("Cancel animation");
			((ButtonBase)cmdCancelAnimation).set_UseVisualStyleBackColor(false);
			((Control)cmdCancelAnimation).set_Visible(false);
			((Control)cmdCancelAnimation).add_Click((EventHandler)cmdCancelAnimation_Click);
			pnlAdjust.set_BorderStyle((BorderStyle)1);
			((Control)pnlAdjust).get_Controls().Add((Control)(object)label11);
			((Control)pnlAdjust).get_Controls().Add((Control)(object)txtPhaseOffset);
			((Control)pnlAdjust).get_Controls().Add((Control)(object)label24);
			((Control)pnlAdjust).get_Controls().Add((Control)(object)label25);
			((Control)pnlAdjust).get_Controls().Add((Control)(object)label23);
			((Control)pnlAdjust).get_Controls().Add((Control)(object)label22);
			((Control)pnlAdjust).get_Controls().Add((Control)(object)updnRotnRate);
			((Control)pnlAdjust).get_Controls().Add((Control)(object)updnZeroPhase);
			((Control)pnlAdjust).set_Location(new Point(5, 85));
			((Control)pnlAdjust).set_Name("pnlAdjust");
			((Control)pnlAdjust).set_Size(new Size(219, 47));
			((Control)pnlAdjust).set_TabIndex(149);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(167, 0));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(49, 13));
			((Control)label11).set_TabIndex(7);
			((Control)label11).set_Text("Total at");
			((Control)txtPhaseOffset).set_Location(new Point(175, 23));
			((Control)txtPhaseOffset).set_Name("txtPhaseOffset");
			((TextBoxBase)txtPhaseOffset).set_ReadOnly(true);
			((Control)txtPhaseOffset).set_Size(new Size(34, 20));
			((Control)txtPhaseOffset).set_TabIndex(4);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(171, 11));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(48, 13));
			((Control)label24).set_TabIndex(5);
			((Control)label24).set_Text("event °");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(7, -1));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(145, 13));
			((Control)label25).set_TabIndex(6);
			((Control)label25).set_Text("A d j u s t   r o t a t i o n");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(9, 11));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(73, 13));
			((Control)label23).set_TabIndex(3);
			((Control)label23).set_Text("Rotation °/day");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(93, 11));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(67, 13));
			((Control)label22).set_TabIndex(2);
			((Control)label22).set_Text("Zero phase °");
			updnRotnRate.set_DecimalPlaces(6);
			updnRotnRate.set_Increment(new decimal(new int[4] { 5, 0, 0, 393216 }));
			((Control)updnRotnRate).set_Location(new Point(7, 24));
			updnRotnRate.set_Maximum(new decimal(new int[4] { 1, 0, 0, 196608 }));
			updnRotnRate.set_Minimum(new decimal(new int[4] { 1, 0, 0, -2147287040 }));
			((Control)updnRotnRate).set_Name("updnRotnRate");
			((Control)updnRotnRate).set_Size(new Size(71, 20));
			((Control)updnRotnRate).set_TabIndex(1);
			((UpDownBase)updnRotnRate).set_TextAlign((HorizontalAlignment)1);
			updnRotnRate.add_ValueChanged((EventHandler)updnRotnRate_ValueChanged);
			((Control)updnZeroPhase).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)updnZeroPhase).set_Location(new Point(103, 24));
			updnZeroPhase.set_Maximum(new decimal(new int[4] { 360, 0, 0, 0 }));
			updnZeroPhase.set_Minimum(new decimal(new int[4] { 360, 0, 0, -2147483648 }));
			((Control)updnZeroPhase).set_Name("updnZeroPhase");
			((Control)updnZeroPhase).set_Size(new Size(47, 20));
			((Control)updnZeroPhase).set_TabIndex(0);
			updnZeroPhase.set_Value(new decimal(new int[4] { 359, 0, 0, -2147483648 }));
			updnZeroPhase.add_ValueChanged((EventHandler)updnZeroPhase_ValueChanged);
			((Control)updnZeroPhase).add_Leave((EventHandler)updnZeroPhase_Leave);
			((Control)lblDAMITisam).set_AutoSize(true);
			lblDAMITisam.set_BorderStyle((BorderStyle)2);
			((Control)lblDAMITisam).set_Font(new Font("Microsoft Sans Serif", 6f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDAMITisam).set_Location(new Point(274, 42));
			((Control)lblDAMITisam).set_Name("lblDAMITisam");
			((Control)lblDAMITisam).set_Size(new Size(14, 92));
			((Control)lblDAMITisam).set_TabIndex(150);
			((Control)lblDAMITisam).set_Text("D\r\nA\r\nM\r\nI\r\nT\r\n\r\nI\r\nS\r\nA\r\nM");
			lblDAMITisam.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_ForeColor(Color.DarkGreen);
			((Control)label4).set_Location(new Point(729, 25));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(50, 13));
			((Control)label4).set_TabIndex(152);
			((Control)label4).set_Text("Opacity");
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).set_Location(new Point(695, 40));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(151, 23));
			((Control)panel1).set_TabIndex(160);
			((Control)trackOpacity).set_AutoSize(false);
			((Control)trackOpacity).set_BackColor(Color.LemonChiffon);
			((Control)trackOpacity).set_Location(new Point(697, 41));
			trackOpacity.set_Maximum(100);
			trackOpacity.set_Minimum(20);
			((Control)trackOpacity).set_Name("trackOpacity");
			((Control)trackOpacity).set_Size(new Size(147, 21));
			trackOpacity.set_SmallChange(5);
			((Control)trackOpacity).set_TabIndex(39);
			trackOpacity.set_TickFrequency(10);
			trackOpacity.set_Value(100);
			trackOpacity.add_ValueChanged((EventHandler)trackOpacity_ValueChanged);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(696, 64));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(26, 12));
			((Control)label20).set_TabIndex(158);
			((Control)label20).set_Text("20%");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(760, 64));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(26, 12));
			((Control)label13).set_TabIndex(157);
			((Control)label13).set_Text("60%");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(792, 64));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(26, 12));
			((Control)label14).set_TabIndex(156);
			((Control)label14).set_Text("80%");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(818, 64));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(32, 12));
			((Control)label15).set_TabIndex(155);
			((Control)label15).set_Text("100%");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(728, 64));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(26, 12));
			((Control)label18).set_TabIndex(154);
			((Control)label18).set_Text("40%");
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).set_Location(new Point(642, 84));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(204, 40));
			((Control)panel2).set_TabIndex(161);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(849, 733));
			((Control)this).get_Controls().Add((Control)(object)lblSelect);
			((Control)this).get_Controls().Add((Control)(object)lblDAMITisam);
			((Control)this).get_Controls().Add((Control)(object)trackOpacity);
			((Control)this).get_Controls().Add((Control)(object)pnlAdjust);
			((Control)this).get_Controls().Add((Control)(object)label20);
			((Control)this).get_Controls().Add((Control)(object)lblJD);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)txtDeltaT);
			((Control)this).get_Controls().Add((Control)(object)label14);
			((Control)this).get_Controls().Add((Control)(object)label16);
			((Control)this).get_Controls().Add((Control)(object)label15);
			((Control)this).get_Controls().Add((Control)(object)lblPhaseAngle);
			((Control)this).get_Controls().Add((Control)(object)label18);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)cmdCancelAnimation);
			((Control)this).get_Controls().Add((Control)(object)lblAvailableAsteroids);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)txtMin);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)txtHrs);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)trackBarImageSize);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)cmdDownloadModels);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)picDisplay);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtDay);
			((Control)this).get_Controls().Add((Control)(object)lstModels);
			((Control)this).get_Controls().Add((Control)(object)txtMonth);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)txtYear);
			((Control)this).get_Controls().Add((Control)(object)lblModel);
			((Control)this).get_Controls().Add((Control)(object)label17);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)pnlPhaseChange);
			((Control)this).get_DataBindings().Add(new Binding("TopMost", (object)Settings.Default, "ShapeModel_TopMost", true, (DataSourceUpdateMode)1));
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationShapeModels", true, (DataSourceUpdateMode)1));
			((Form)this).set_Location(Settings.Default.LocationShapeModels);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("DisplayShapeModels");
			((Form)this).set_SizeGripStyle((SizeGripStyle)1);
			((Control)this).set_Text("DAMIT and ISAM shape models");
			((Form)this).set_TopMost(Settings.Default.ShapeModel_TopMost);
			((Form)this).add_Activated((EventHandler)DisplayShapeModels_Activated);
			((Form)this).add_FormClosing(new FormClosingEventHandler(DisplayShapeModels_FormClosing));
			((Form)this).add_Load((EventHandler)DisplayShapeModels_Load);
			((Control)this).add_Resize((EventHandler)DisplayShapeModels_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)trackBarImageSize).EndInit();
			((Control)pnlPhaseChange).ResumeLayout(false);
			((ISupportInitialize)picDisplay).EndInit();
			((Control)pnlAdjust).ResumeLayout(false);
			((Control)pnlAdjust).PerformLayout();
			((ISupportInitialize)updnRotnRate).EndInit();
			((ISupportInitialize)updnZeroPhase).EndInit();
			((ISupportInitialize)trackOpacity).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
