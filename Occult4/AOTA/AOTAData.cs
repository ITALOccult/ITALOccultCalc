using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using LightCurves;
using Occult;
using Occult.Properties;
using Occult.SDK;

namespace AOTA
{
	public class AOTAData
	{
		internal static PlotForm PlotForm;

		internal static VerifyTimes VerifyTimes;

		internal static IntegrityCheck Integrity;

		internal static DisplayData BadReadMessage;

		internal const int ArrayDimension = 50000;

		internal static float[] Star = new float[50000];

		internal static bool[] StarValid = new bool[50000];

		internal static float[] StarForeground = new float[50000];

		internal static float[] StarForegroundAperture = new float[50000];

		internal static float[] StarBackground = new float[50000];

		internal static float[] Comp1 = new float[50000];

		internal static float[] Comp2 = new float[50000];

		internal static float[] Comp3 = new float[50000];

		internal static float[] FrameID = new float[50000];

		internal static double[] FrameTimeSecs = new double[50000];

		internal static double[] FrameTimeCorrnsD = new double[5];

		internal static double[] FrameTimeCorrnsR = new double[5];

		internal static double[] FourierReal;

		internal static double[] FourierImaginary;

		internal static double[] FourierReal1;

		internal static double[] FourierImaginary1;

		internal static double[] FourierReal2;

		internal static double[] FourierImaginary2;

		internal static double[] FourierReal3;

		internal static double[] FourierImaginary3;

		internal static int StarCount = 0;

		internal static int Comp1Count = 0;

		internal static int Comp2Count = 0;

		internal static int Comp3Count = 0;

		internal static bool Comp1Used = false;

		internal static bool Comp2Used = false;

		internal static bool Comp3Used = false;

		internal static bool AutoProcess = false;

		internal static bool IntegrationPreset = false;

		internal static string TangraSourceFile = "";

		internal static int FramesInIntegration_fromTangra = 0;

		internal static string Camera_from_Tangra = "";

		internal static bool SetCameraDelays_AAV_Files = false;

		internal static bool IsADVSFile = false;

		internal static bool IsAAVFile = false;

		internal static string PlotFormBaseText = "";

		internal static bool IsPlotting = false;

		internal static bool FirstTimeAnalysed = false;

		internal static int FramesAdded = 0;

		internal static int TimeSource = 0;

		internal static int OccStar = 0;

		internal static float[] StarToPlot = new float[50000];

		internal static bool[] StarValidToPlot = new bool[50000];

		internal static float[] Comp1ToPlot = new float[50000];

		internal static float[] Comp2ToPlot = new float[50000];

		internal static float[] Comp3ToPlot = new float[50000];

		internal static float[] FrameIDofPlot = new float[50000];

		internal static float[] StarToPlot_FromAve = new float[50000];

		internal static float[] Comp1ToPlot_FromAve = new float[50000];

		internal static float[] Comp2ToPlot_FromAve = new float[50000];

		internal static float[] Comp3ToPlot_FromAve = new float[50000];

		internal static float[] StarBackground_FromAve = new float[50000];

		internal static float[] Normalisation = new float[50000];

		internal static float[] RunningAverageValues = new float[50000];

		internal static int NormalisationStarRef = 0;

		internal static int RunningAverage = 9;

		internal static int PlotRunningAverage = 0;

		internal static double[] CrossStar = new double[50000];

		internal static double[] CrossCorrR = new double[50000];

		internal static int[] CrossCorrWidth = new int[50000];

		internal static int MaxFrames = 100;

		private static float SumStar;

		private static float SumComp1;

		private static float SumComp2;

		private static float SumComp3;

		private static float SumBackground;

		private static float SumAperture;

		internal static int X0;

		internal static int X1;

		internal static int X2;

		internal static int X3;

		internal static int X4;

		internal static int X5;

		internal static int X6;

		internal static int X7;

		internal static int XFull;

		internal static int XOcc;

		internal static double YFull = 0.0;

		internal static double YOcc = 0.0;

		internal static double YFullSD = 0.0;

		internal static double YOccSD = 0.0;

		internal static double YFullVariance = 0.0;

		internal static double YOccVariance = 0.0;

		internal static double YExpected = 0.0;

		internal static int StarCountToPlot = 0;

		internal static int Comp1CountToPlot = 0;

		internal static int Comp2CountToPlot = 0;

		internal static int Comp3CountToPlot = 0;

		internal static float Comp1Level = 0f;

		internal static float Comp2Level = 0f;

		internal static float Comp3Level = 0f;

		internal static float StarLevel = 0f;

		internal static float Background = 0f;

		internal static double HalfFrame_Interval = 0.04;

		internal static bool LiMovie_FieldsUsed = false;

		internal static bool CameraCorrectionsHaveBeenApplied = false;

		internal static int NumberOfFramesBinned = 1;

		internal static int FirstFrameUsedInAnalysis = 0;

		internal static bool CancelIntegration = false;

		internal static bool TimePresentInSourceData = false;

		internal static bool OnlySomeTimesPresentInSourceData = false;

		internal static bool TwoAndOnlyTwoTimesPresentInSourceData = false;

		internal static bool DataHasIntegrityIssues = false;

		internal static bool IsPAL = false;

		internal static bool CrossCorrDone = false;

		private static double FOMforNoOccn = 0.0;

		internal static bool CancelFind;

		internal static bool Background_Point = true;

		internal static bool Background_Average = false;

		internal static bool AllowBothPoint_Average = false;

		internal static List<Maxima> MaxValues = new List<Maxima>();

		internal static int ExtraPointsUsedInSolution = 12;

		internal static bool UpdatingExtraPointsUsedInSolution = false;

		internal static double MeanBefore = 1.0;

		internal static double MeanDuring = 1.0;

		internal static double MeanAfter = 1.0;

		internal static double SDBefore = 1.0;

		internal static double SDduring = 1.0;

		internal static double SDAfter = 1.0;

		internal static double ExpectedSN_Miss = 0.0;

		internal static int CurrentlySelectedEvent = -1;

		internal static double VarianceBefore = 1.0;

		internal static double VarianceDuring = 1.0;

		internal static double VarianceAfter = 1.0;

		internal static float VerticalScaleAdjustment = 1f;

		internal static double[] TheoreticalSignal = new double[500];

		internal static int TheoreticalLength = 0;

		internal static double MinimumChi2DValue = 1000.0;

		internal static double MinimumChi2RValue = 1000.0;

		internal static int MinimumChi2DPosition = 0;

		internal static int MinimumChi2RPosition = 0;

		internal static int Chi2D_Selected = 0;

		internal static int Chi2R_Selected = 0;

		internal static float ChiPlot_HorizontalScale = 1f;

		internal static float[] PlusLimit = new float[2];

		internal static float[] MinusLimit = new float[2];

		internal static int MaximimumTransitions = 10;

		internal static string CurrentFileName = "";

		internal static string MeasurementEngine = "";

		internal static double ConfidenceLevelD = 0.85;

		internal static double ConfidenceLevelR = 0.85;

		internal static double[] LastMaxima = new double[5];

		internal static int[] LastPos = new int[5];

		internal static int[] LastWidth = new int[5];

		internal static int LastMaximaCount = 0;

		internal static bool CheckForPrimaryEventFound = true;

		internal static string EventID = "0";

		internal static int EventIDnumber = 0;

		internal static int CurrentSolution = 0;

		internal static bool[] MissEvent = new bool[5];

		internal static bool[] EventAnalysed = new bool[5];

		internal static EventResults[] Results = new EventResults[5];

		internal static Camera CameraSpecs = default(Camera);

		internal static double[] AppliedConfidenceD = new double[5] { 0.9, 0.9, 0.9, 0.9, 0.9 };

		internal static double[] AppliedConfidenceR = new double[5] { 0.9, 0.9, 0.9, 0.9, 0.9 };

		internal static int[] InvalidNearD = new int[5];

		internal static int[] InvalidNearR = new int[5];

		internal static bool IntegrationFromLightCurve = false;

		internal static int RunningAverageLC = 9;

		internal static int PlotRunningAverageLC = 0;

		internal static float VerticalScaleAdjustmentLC = 1f;

		internal static int StartOfLightCurveSelection = 0;

		internal static int EndOfLightCurveSelection = 0;

		internal static bool LightCurveReportHasBeenSaved = false;

		internal static float LightCurve_ZeroHeight = 0f;

		internal static float LightCurve_Height_100 = 0f;

		internal static float AOTA_ZeroHeight = 0f;

		internal static float AOTA_Height_100 = 0f;

		internal static float AOTA_VerticalScale = 1f;

		public static void ShowPlotForm(IWin32Window parentWindow, bool Modal)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_0029: Unknown result type (might be due to invalid IL or missing references)
			if (Modal)
			{
				try
				{
					((Form)PlotForm).ShowDialog(parentWindow);
					return;
				}
				catch
				{
					PlotForm plotForm = new PlotForm();
					((Form)plotForm).set_StartPosition((FormStartPosition)4);
					PlotForm = plotForm;
					((Form)PlotForm).ShowDialog(parentWindow);
					return;
				}
			}
			try
			{
				((Control)PlotForm).Show();
			}
			catch
			{
				PlotForm plotForm2 = new PlotForm();
				((Form)plotForm2).set_StartPosition((FormStartPosition)4);
				PlotForm = plotForm2;
				((Control)PlotForm).Show();
			}
		}

		internal static void ShowIntegrityCheck(bool FromLightCurve)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				((Form)Integrity).ShowDialog();
			}
			catch
			{
				IntegrityCheck integrityCheck = new IntegrityCheck();
				((Control)integrityCheck).set_Enabled(true);
				Integrity = integrityCheck;
				((Form)Integrity).ShowDialog();
			}
			Set_DataToPlot(IntegrationFromLightCurve);
			IntegrationFromLightCurve = FromLightCurve;
		}

		internal static void ShowVerifyTimes()
		{
			try
			{
				((Control)VerifyTimes).Show();
			}
			catch
			{
				VerifyTimes = new VerifyTimes();
				((Control)VerifyTimes).Show();
			}
		}

		internal static void ResetOutputs(bool JustOne, int EventNo)
		{
			CurrentSolution = 0;
			for (int i = 0; i < 5; i++)
			{
				if (!(JustOne && i != EventNo))
				{
					Results[i].D_Frame = (Results[i].R_Frame = (Results[i].D_FrameUncertPlus = (Results[i].D_FrameUncertMinus = (Results[i].R_FrameUncertPlus = (Results[i].R_FrameUncertMinus = (Results[i].D_DurationFrames = (Results[i].R_DurationFrames = -1f)))))));
					Results[i].D_UTC = (Results[i].R_UTC = "-1");
					Results[i].D_SN = (Results[i].R_SN = 0f);
					((Control)PlotForm.lblSNR).set_Text("SnR = 0.0");
					Results[i].IsNonEvent = true;
					MissEvent[i] = false;
					AppliedConfidenceD[i] = (double)PlotForm.updnConfidenceD.get_Value();
					AppliedConfidenceR[i] = (double)PlotForm.updnConfidenceR.get_Value();
					InvalidNearD[i] = (InvalidNearR[i] = 0);
				}
			}
			for (int j = 0; j < 5; j++)
			{
				FrameTimeCorrnsD[j] = (FrameTimeCorrnsR[j] = 0.0);
			}
			ExpectedSN_Miss = 0.0;
		}

		private void VerifyTimes_Load(object sender, EventArgs e)
		{
			((Form)PlotForm).set_WindowState((FormWindowState)1);
		}

		internal static void OpenCSVfile(bool FromLightCurve, string Unistellar_scopeID, bool FrameInsertions_Allow)
		{
			//IL_0021: Unknown result type (might be due to invalid IL or missing references)
			//IL_0028: Expected O, but got Unknown
			//IL_00c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cc: Invalid comparison between Unknown and I4
			//IL_029a: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a0: Invalid comparison between Unknown and I4
			string dataBlock = "";
			string text = "";
			if (Unistellar_scopeID.Length > 0)
			{
				text = "  Unstellar_scope SN = " + Unistellar_scopeID;
			}
			OpenFileDialog val = new OpenFileDialog();
			try
			{
				((FileDialog)val).set_Title("Select CSV file to open" + text);
				((FileDialog)val).set_FileName(Path.GetFileName(Settings.Default.LastFileOpenedAOTA));
				try
				{
					((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.LastFileOpenedAOTA));
				}
				catch
				{
				}
				if (Unistellar_scopeID.Length == 3)
				{
					((FileDialog)val).set_Filter("CSV files (*" + Unistellar_scopeID + "*.csv)|*" + Unistellar_scopeID + "*.csv|All files (*.*)|*.*");
					((FileDialog)val).set_FilterIndex(0);
				}
				else
				{
					((FileDialog)val).set_Filter("CSV files (*.csv)|*.csv|All files (*.*)|*.*");
					((FileDialog)val).set_FilterIndex(1);
				}
				if ((int)((CommonDialog)val).ShowDialog() != 1)
				{
					return;
				}
				string text2 = (CurrentFileName = (Settings.Default.LastFileOpenedAOTA = ((FileDialog)val).get_FileName()));
			}
			finally
			{
				((IDisposable)val)?.Dispose();
			}
			StarToPlot = new float[50000];
			StarValidToPlot = new bool[50000];
			Comp1ToPlot = new float[50000];
			Comp2ToPlot = new float[50000];
			Comp3ToPlot = new float[50000];
			FrameIDofPlot = new float[50000];
			FrameTimeSecs = new double[50000];
			StarToPlot_FromAve = new float[50000];
			Comp1ToPlot_FromAve = new float[50000];
			Comp2ToPlot_FromAve = new float[50000];
			Comp3ToPlot_FromAve = new float[50000];
			StarBackground_FromAve = new float[50000];
			Normalisation = new float[50000];
			RunningAverageValues = new float[50000];
			using (StreamReader streamReader = new StreamReader(CurrentFileName))
			{
				dataBlock = streamReader.ReadToEnd();
			}
			DataHasIntegrityIssues = !CheckDataIntegrity(dataBlock, FromLightCurve, out var ErrorMessage, out var ErrorCount);
			if (DataHasIntegrityIssues)
			{
				BadReadMessage = new DisplayData();
				((Control)BadReadMessage.txtBox).set_Text(ErrorMessage);
				((Control)BadReadMessage).set_Text("Bad .csv file data");
				((Control)BadReadMessage).set_Width(700);
				if (ErrorCount < 25)
				{
					((Control)BadReadMessage).set_Height(16 * ErrorCount + 200);
				}
				else
				{
					((Control)BadReadMessage).set_Height(600);
				}
				((ToolStripItem)BadReadMessage.cmdOK).set_Text("Continue");
				((ToolStripItem)BadReadMessage.cmdCancel).Select();
				bool flag = true;
				if ((int)((Form)BadReadMessage).ShowDialog() == 2)
				{
					flag = false;
				}
				((Component)(object)BadReadMessage).Dispose();
				if (!flag)
				{
					return;
				}
			}
			Read_CSV_Data(dataBlock, FromLightCurve, FrameInsertions_Allow);
		}

		internal static void PasteCSVdata(bool FromLightCurve, bool FrameInsertions_Allow)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			//IL_004a: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0100: Invalid comparison between Unknown and I4
			string text = Clipboard.GetText();
			if (text.Length < 100)
			{
				MessageBox.Show("Pasted data appears to be too short");
				return;
			}
			if (!(text.Substring(0, 90).Contains("Limovie") | text.Substring(0, 90).Contains("Tangra")))
			{
				MessageBox.Show("Pasted data has not come from Tangra or Limovie");
				return;
			}
			DataHasIntegrityIssues = !CheckDataIntegrity(text, FromLightCurve, out var ErrorMessage, out var ErrorCount);
			if (DataHasIntegrityIssues)
			{
				BadReadMessage = new DisplayData();
				((Control)BadReadMessage.txtBox).set_Text(ErrorMessage);
				((Control)BadReadMessage).set_Text("Bad pasted data");
				((Control)BadReadMessage).set_Width(700);
				if (ErrorCount < 25)
				{
					((Control)BadReadMessage).set_Height(16 * ErrorCount + 200);
				}
				else
				{
					((Control)BadReadMessage).set_Height(600);
				}
				((ToolStripItem)BadReadMessage.cmdOK).set_Text("Continue");
				((ToolStripItem)BadReadMessage.cmdCancel).Select();
				bool flag = true;
				if ((int)((Form)BadReadMessage).ShowDialog() == 2)
				{
					flag = false;
				}
				((Component)(object)BadReadMessage).Dispose();
				if (flag)
				{
					Read_CSV_Data(text, FromLightCurve, FrameInsertions_Allow);
				}
			}
		}

		internal static bool CheckDataIntegrity(string DataBlock, bool FromLightCurve, out string ErrorMessage, out int ErrorCount)
		{
			int CurrentPosition = 0;
			string text = "";
			text = (DataBlock.Substring(0, 100).Contains("\r\n") ? "\r\n" : ((!DataBlock.Substring(0, 100).Contains("\r")) ? "\n" : "\r"));
			ErrorMessage = "";
			string text2 = "\r\n\r\nSome of these errors might prevent the file from being read correctly.\r\nThis is particularly the case if the empty column is the time, or the star brightness measurement.\r\n\r\nDo you want to continue?";
			string text3 = "";
			string text4 = "";
			string text5 = "";
			bool flag = false;
			do
			{
				text3 = LineFromDataBlock(DataBlock, ref CurrentPosition, text).PadRight(10).Replace('"', ' ');
				if (text3.Substring(0, 10).Contains("Tangra"))
				{
					do
					{
						text4 = LineFromDataBlock(DataBlock, ref CurrentPosition, text);
						if (text4.Length >= 12 && (text4.Substring(0, 10).Contains("FrameNo") | text4.Substring(0, 10).Contains("BinNo")))
						{
							flag = true;
							text5 = text4;
							break;
						}
					}
					while (CurrentPosition < DataBlock.Length);
				}
				else if (text3.Substring(0, 10).Contains("Limovie"))
				{
					do
					{
						text4 = LineFromDataBlock(DataBlock, ref CurrentPosition, text);
						if (text4.Length >= 9 && text4.Substring(0, 8).Contains("No."))
						{
							flag = true;
							text5 = text4;
							break;
						}
					}
					while (CurrentPosition < DataBlock.Length);
				}
				else if (text3.Substring(0, 10).Contains("R-OTE"))
				{
					do
					{
						text4 = LineFromDataBlock(DataBlock, ref CurrentPosition, text);
						if (text4.Length >= 9)
						{
							if (text4.Substring(0, 8).Contains("RdgNum"))
							{
								flag = true;
								text5 = text4;
								break;
							}
							if (text4.Substring(0, 8).Contains("FrameNum"))
							{
								flag = true;
								text5 = text4;
								break;
							}
						}
					}
					while (CurrentPosition < DataBlock.Length);
				}
				else if (text3.Substring(0, 10).Contains("PyMovie") | text3.Substring(0, 10).Contains("PYOTE"))
				{
					do
					{
						text4 = LineFromDataBlock(DataBlock, ref CurrentPosition, text);
						if (text4.Length >= 9 && text4.Substring(0, 8).Contains("FrameNum"))
						{
							flag = true;
							text5 = text4;
							break;
						}
					}
					while (CurrentPosition < DataBlock.Length);
				}
				else if (text3.Substring(0, 10).Contains("#Scanalyze"))
				{
					do
					{
						text4 = LineFromDataBlock(DataBlock, ref CurrentPosition, text);
						if (text4.Length >= 1 && text4.Contains("#Pixel,UTC,Intensity"))
						{
							flag = true;
							text5 = text4;
							break;
						}
					}
					while (CurrentPosition < DataBlock.Length);
				}
				else if (text3.Contains("date;SNR;flux;"))
				{
					text4 = LineFromDataBlock(DataBlock, ref CurrentPosition, text);
					flag = true;
					text5 = text4;
					break;
				}
			}
			while (!flag && CurrentPosition >= 0);
			string[] array = LineFromDataBlock(DataBlock, ref CurrentPosition, text).Split(new char[1] { ',' });
			int num = array.Length;
			ErrorCount = 0;
			do
			{
				text4 = LineFromDataBlock(DataBlock, ref CurrentPosition, text);
				array = text4.Split(new char[1] { ',' });
				if (array.Length != num)
				{
					if (CurrentPosition > 0)
					{
						ErrorMessage = ErrorMessage + "Incorrect field length at: " + text4 + "\r\n";
						ErrorCount++;
					}
				}
				else if (!text3.Substring(0, 10).Contains("Limovie"))
				{
					for (int i = 0; i < num; i++)
					{
						if (array[i].Length == 0)
						{
							ErrorMessage = ErrorMessage + "Empty field(s) at: " + text4 + "\r\n";
							ErrorCount++;
							break;
						}
					}
				}
				if (ErrorCount >= 50)
				{
					ErrorMessage = "More than 50 format errors found. Error check aborted\r\n\r\n" + text5 + "\r\n" + ErrorMessage + text2;
					return false;
				}
			}
			while (CurrentPosition >= 0);
			if (ErrorCount > 0)
			{
				ErrorMessage = ErrorCount + " format errors found.\r\n\r\n" + text5 + "\r\n" + ErrorMessage + text2;
				return false;
			}
			return true;
		}

		internal static void Read_CSV_Data(string DataBlock, bool FromLightCurve, bool FrameInsertions_Allow)
		{
			//IL_0496: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04cc: Invalid comparison between Unknown and I4
			//IL_04db: Unknown result type (might be due to invalid IL or missing references)
			//IL_0660: Unknown result type (might be due to invalid IL or missing references)
			//IL_102d: Unknown result type (might be due to invalid IL or missing references)
			//IL_175b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1dbc: Unknown result type (might be due to invalid IL or missing references)
			//IL_1f76: Unknown result type (might be due to invalid IL or missing references)
			//IL_1f7c: Invalid comparison between Unknown and I4
			//IL_2933: Unknown result type (might be due to invalid IL or missing references)
			//IL_2d17: Unknown result type (might be due to invalid IL or missing references)
			//IL_2dbe: Unknown result type (might be due to invalid IL or missing references)
			//IL_302d: Unknown result type (might be due to invalid IL or missing references)
			bool flag = false;
			int num = 0;
			string text = "";
			text = (DataBlock.Substring(0, 100).Contains("\r\n") ? "\r\n" : ((!DataBlock.Substring(0, 100).Contains("\r")) ? "\n" : "\r"));
			int num2 = 2;
			int num3 = 16;
			int num4 = 4;
			int num5 = 6;
			int num6 = 8;
			int result = 0;
			int result2 = 0;
			int num7 = 0;
			int num8 = 0;
			int num9 = 0;
			double result3 = 0.0;
			double result4 = 0.0;
			double result5 = 0.0;
			double num10 = 0.0;
			double num11 = 0.0;
			bool flag2 = false;
			bool flag3 = true;
			bool flag4 = false;
			Background_Point = true;
			Background_Average = false;
			AllowBothPoint_Average = false;
			FramesAdded = 0;
			StarToPlot = new float[50000];
			StarValidToPlot = new bool[50000];
			Comp1ToPlot = new float[50000];
			Comp2ToPlot = new float[50000];
			Comp3ToPlot = new float[50000];
			FrameIDofPlot = new float[50000];
			FrameTimeSecs = new double[50000];
			StarToPlot_FromAve = new float[50000];
			Comp1ToPlot_FromAve = new float[50000];
			Comp2ToPlot_FromAve = new float[50000];
			Comp3ToPlot_FromAve = new float[50000];
			StarBackground_FromAve = new float[50000];
			Normalisation = new float[50000];
			RunningAverageValues = new float[50000];
			IntegrationFromLightCurve = FromLightCurve;
			Comp1Used = (Comp2Used = (Comp3Used = (TimePresentInSourceData = false)));
			Comp1Count = (Comp2Count = (Comp3Count = (StarCount = (StarCountToPlot = 0))));
			StarLevel = (Background = (Comp1Level = (Comp2Level = 0f)));
			X0 = (X1 = (X2 = (X5 = (X6 = (X7 = 0)))));
			NumberOfFramesBinned = 1;
			FirstFrameUsedInAnalysis = 0;
			Background = (Comp1Level = (Comp2Level = (Comp3Level = (StarLevel = 0f))));
			num9 = 0;
			TimePresentInSourceData = false;
			OnlySomeTimesPresentInSourceData = false;
			TimeSource = 0;
			IsPAL = true;
			CameraSpecs.VideoSystem = "Unspecified";
			LightCurveReportHasBeenSaved = false;
			if (!FromLightCurve)
			{
				PlotForm.optSystemNotKnown.set_Checked(true);
				((Control)PlotForm.lblSetNTSCorPAL).set_Visible(true);
				PlotForm.optSystemOther.set_Checked(true);
				NumericUpDown updnConfidenceD = PlotForm.updnConfidenceD;
				NumericUpDown updnConfidenceR = PlotForm.updnConfidenceR;
				decimal value = 90m;
				updnConfidenceR.set_Value(value);
				updnConfidenceD.set_Value(value);
				CameraSpecs.MeasurementsBinned = 1;
				CameraCorrectionsHaveBeenApplied = false;
				for (int i = 0; i < 5; i++)
				{
					EventAnalysed[i] = false;
				}
				CurrentlySelectedEvent = -1;
				ResetOutputs(JustOne: false, -1);
			}
			NormalisationStarRef = 0;
			AOTA_ExternalAccess.AOTA_Client = null;
			AOTA_ExternalAccess.TangraFrameDisplayFromTangra = (AOTA_ExternalAccess.TangraFrameDisplayFromAOTA = false);
			num = 0;
			do
			{
				string text2 = LineFromDataBlock(DataBlock, ref num, text).PadRight(10).Replace('"', ' ');
				float result6;
				if (text2.Substring(0, 10).Contains("Tangra"))
				{
					CameraSpecs.MeasuringTool = "Tangra";
					MeasurementEngine = " ,  measured with Tangra";
					HalfFrame_Interval = 0.02;
					TimeSource = 1;
					num9 = (StarCount = 0);
					IsADVSFile = (IsAAVFile = false);
					do
					{
						string text3 = LineFromDataBlock(DataBlock, ref num, text).PadRight(10).Replace('"', ' ');
						if (text3.Contains("Reversed Gamma"))
						{
							string[] array = text3.Split(new char[1] { ',' });
							string[] array2 = LineFromDataBlock(DataBlock, ref num, text).Split(new char[1] { ',' });
							for (int j = 0; j < array.Length; j++)
							{
								if (text3.Contains("(AAV Int"))
								{
									if (array2[j] == "")
									{
										IsADVSFile = (IsAAVFile = false);
									}
									else if (array2[j] == "1")
									{
										IsADVSFile = true;
									}
									else
									{
										IsAAVFile = true;
									}
								}
								if (text3.Contains("Instrumental Delay Corrections"))
								{
									CameraCorrectionsHaveBeenApplied = (array2[7] == "Applied") | (array2[7] == "Not Required");
									if (IsAAVFile & !CameraCorrectionsHaveBeenApplied)
									{
										MessageBox.Show("Your AAV file has not had camera corrections applied by Tangra. AOTA assumes that Tangra has appliedcamera corrections. This is a serious problem, and the times generated by continuing to analyse this light curve will be incorrect.\r\n\r\nTo obtain the correct times for this event, you need to re-measure your AAV file in Tangra, and at the relevant point in its processing apply the instrumental corrections. DO NOT SELECT the option 'Do not apply corrections. I'll do it myself'but select from the drop-down list the camera type you were using.", "AAV file alert", (MessageBoxButtons)0, (MessageBoxIcon)48);
									}
									if ((IsAAVFile & (array2[9] == "1")) && (int)MessageBox.Show("Your AAV file has been recorded on the basis that your camera was not operating in integration mode. That is, that each frame from the camera is different.\r\n\r\nWas your camera operating in integration mode? That is, was it set to integrate 2 or more frames?", "Confirm camera was integrating", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
									{
										MessageBox.Show("You have indicated that your video camera was operating in integration mode. However your AAV file has not been synced with your video camera's integration. This is a serious problem, and the times generated by continuing to analyse this light curve will be incorrect.\r\n\r\nTo fix this problem, you will need to save your light curve in Tangra as a CSV file — from 'File... Export light curve... Save as CSV file'\r\n\r\nYou will then need to send that CSV file to your regional coordinator to recover from this problem.\r\n\r\nTo avoid this problem in future YOU MUST SYNC your AAV recording to the camera's integration.", "AAV lack of sync alert", (MessageBoxButtons)0, (MessageBoxIcon)48);
									}
								}
							}
						}
						if (text3.Substring(0, 10).Contains("FrameNo"))
						{
							flag = false;
							num2 = 2;
							num4 = 4;
							num5 = 6;
							num6 = 8;
							break;
						}
						if (text3.Substring(0, 10).Contains("BinNo"))
						{
							flag = true;
							num2 = 2;
							num4 = 3;
							num5 = 4;
							num6 = 5;
							break;
						}
					}
					while (num >= 0);
					bool flag5 = false;
					bool flag6 = false;
					do
					{
						flag6 = flag5;
						flag5 = false;
						string text3 = LineFromDataBlock(DataBlock, ref num, text);
						if ((text3 == null) | (text3 == "") | !text3.Contains(","))
						{
							continue;
						}
						string[] array3 = text3.Replace('+', ' ').Split(new char[1] { ',' });
						if (StarCount == 0)
						{
							Comp1Used = array3.GetUpperBound(0) >= num4;
							if (Comp1Used && !float.TryParse(array3[num4], out result6))
							{
								Comp1Used = false;
							}
							Comp2Used = array3.GetUpperBound(0) >= num5;
							if (Comp2Used && !float.TryParse(array3[num5], out result6))
							{
								Comp2Used = false;
							}
							Comp3Used = array3.GetUpperBound(0) >= num6;
							if (Comp3Used && !float.TryParse(array3[num6], out result6))
							{
								Comp3Used = false;
							}
							int num12 = 1;
							if (Comp1Used)
							{
								num12 = 2;
							}
							if (Comp2Used)
							{
								num12 = 3;
							}
							if (Comp3Used)
							{
								num12 = 4;
							}
							if (num12 > 1)
							{
								((Form)new SelectOccultedStar(num12)).ShowDialog();
								int num13 = num2;
								if (OccStar == 1)
								{
									num2 = num4;
									num4 = num13;
								}
								else if (OccStar == 2)
								{
									num2 = num5;
									num5 = num13;
								}
								else if (OccStar == 3)
								{
									num2 = num6;
									num6 = num13;
								}
							}
						}
						if (array3[1].Contains("?"))
						{
							num10 = 0.0;
						}
						else if (array3[1] == "")
						{
							num10 = ((StarCount <= 1) ? 0.0 : (FrameTimeSecs[StarCount - 1] + num11));
							flag5 = true;
							Settings.Default.OCR_Automatic = false;
						}
						else
						{
							array3[1] = array3[1].Replace("[", "").Replace("]", "");
							if (array3[1].Length > 2)
							{
								num8 = array3[1].LastIndexOf(":");
								if (num8 > 0)
								{
									result3 = double.Parse(array3[1].Substring(num8 + 1));
									num7 = array3[1].LastIndexOf(":", num8 - 1);
									if (num7 > 0)
									{
										result = int.Parse(array3[1].Substring(0, num7));
										result2 = int.Parse(array3[1].Substring(num7 + 1, num8 - num7 - 1));
									}
									else
									{
										result = 0;
										result2 = int.Parse(array3[1].Substring(0, num8));
									}
								}
								else
								{
									result3 = double.Parse(array3[1]);
									result2 = (result = 0);
								}
								num10 = (double)(result * 3600) + (double)result2 * 60.0 + result3;
							}
						}
						if (StarCount == 20)
						{
							num11 = GetFrameDuration(20);
						}
						if (StarCount > 20 && !flag5 && !flag6 && FrameInsertions_Allow)
						{
							double num14 = num10 - FrameTimeSecs[StarCount - 1];
							if ((num14 > 1.3 * num11) & (Math.Abs(num14) < 4.0 * num11))
							{
								int num15 = Convert.ToInt32(num14 / num11);
								for (int k = 1; k < num15; k++)
								{
									FrameTimeSecs[StarCount] = FrameTimeSecs[StarCount - 1] + num11;
									FrameID[StarCount] = FrameID[StarCount - 1] + (float)(10 / num15) / 10f;
									StarValid[StarCount] = false;
									StarForegroundAperture[StarCount] = 0f;
									StarBackground[StarCount] = 0f;
									Star[StarCount] = 0f;
									StarForeground[StarCount] = Star[StarCount];
									num9++;
									StarCount++;
									FramesAdded++;
									if (Comp1Used)
									{
										Comp1[Comp1Count] = 0f;
										Comp1Count++;
									}
									if (Comp2Used)
									{
										Comp2[Comp2Count] = 0f;
										Comp2Count++;
									}
									if (Comp3Used)
									{
										Comp3[Comp3Count] = 0f;
										Comp3Count++;
									}
								}
							}
						}
						FrameTimeSecs[StarCount] = num10;
						StarValid[StarCount] = float.TryParse(array3[num2], out result6);
						if (flag)
						{
							result5 = 0.0;
						}
						else if (!double.TryParse(array3[num2 + 1], out result5))
						{
							result5 = 0.0;
						}
						if (result5 > 1000000000.0)
						{
							result5 -= 4294967296.0;
						}
						StarForegroundAperture[StarCount] = result6;
						StarBackground[StarCount] = (float)result5;
						Star[StarCount] = (float)((double)result6 - result5);
						StarForeground[StarCount] = Star[StarCount];
						StarLevel += Star[StarCount];
						Background += StarBackground[StarCount];
						FrameID[StarCount] = float.Parse(array3[0]);
						StarCount++;
						if (Comp1Used)
						{
							if (!float.TryParse(array3[num4], out result6))
							{
								result6 = 0f;
							}
							if (flag)
							{
								result5 = 0.0;
							}
							else
							{
								try
								{
									if (!double.TryParse(array3[num4 + 1], out result5))
									{
										result5 = 0.0;
									}
								}
								catch
								{
									result5 = 0.0;
								}
							}
							if (result5 > 1000000000.0)
							{
								result5 -= 4294967296.0;
							}
							Comp1[Comp1Count] = (float)((double)result6 - result5);
							Comp1Level += Comp1[Comp1Count];
							Comp1Count++;
						}
						if (Comp2Used)
						{
							if (!float.TryParse(array3[num5], out result6))
							{
								result6 = 0f;
							}
							if (flag)
							{
								result5 = 0.0;
							}
							else
							{
								try
								{
									if (!double.TryParse(array3[num5 + 1], out result5))
									{
										result5 = 0.0;
									}
								}
								catch
								{
									result5 = 0.0;
								}
							}
							if (result5 > 1000000000.0)
							{
								result5 -= 4294967296.0;
							}
							Comp2[Comp2Count] = (float)((double)result6 - result5);
							Comp2Level += Comp2[Comp2Count];
							Comp2Count++;
						}
						if (Comp3Used)
						{
							if (!float.TryParse(array3[num6], out result6))
							{
								result6 = 0f;
							}
							if (flag)
							{
								result5 = 0.0;
							}
							if (result5 > 1000000000.0)
							{
								result5 -= 4294967296.0;
							}
							else
							{
								try
								{
									if (!double.TryParse(array3[num6 + 1], out result5))
									{
										result5 = 0.0;
									}
								}
								catch
								{
									result5 = 0.0;
								}
							}
							Comp3[Comp3Count] = (float)((double)result6 - result5);
							Comp3Level += Comp3[Comp3Count];
							Comp3Count++;
						}
						if (StarCount >= 50000)
						{
							break;
						}
					}
					while (num >= 0);
					SetPAL_NTSC_forTangra();
					if (!FromLightCurve)
					{
						((Control)PlotForm.lblTangraCameraCorrns).set_Visible(IsADVSFile);
					}
					if (CameraCorrectionsHaveBeenApplied)
					{
						HalfFrame_Interval = (FrameTimeSecs[StarCount - 1] - FrameTimeSecs[0]) / 2.0 / (double)StarCount;
						for (int l = 0; l < StarCount; l++)
						{
							FrameTimeSecs[l] -= HalfFrame_Interval;
						}
						if (!FromLightCurve)
						{
							for (int m = 0; m < PlotForm.cmbCamera.get_Items().get_Count(); m++)
							{
								if (PlotForm.cmbCamera.get_Items().get_Item(m).ToString()!.Contains("ADVS"))
								{
									Settings @default = Settings.Default;
									int cameraIndex;
									((ListControl)PlotForm.cmbCamera).set_SelectedIndex(cameraIndex = m);
									@default.CameraIndex = cameraIndex;
									break;
								}
							}
						}
					}
					num9 = EstablishIfTimesArePresent();
					CameraSpecs.MeasuredAtFieldLevel = false;
					Background_Point = false;
					Background_Average = true;
					AllowBothPoint_Average = true;
				}
				else if (text2.Substring(0, 10).Contains("Limovie"))
				{
					CameraSpecs.MeasuringTool = "Limovie";
					MeasurementEngine = " ,  measured with Limovie";
					HalfFrame_Interval = 0.02;
					TimeSource = 2;
					num9 = (StarCount = 0);
					if (!FromLightCurve)
					{
						((Control)PlotForm.lblSetNTSCorPAL).set_Visible(false);
						CameraSpecs.VideoSystem = "PAL";
						PlotForm.optSystemPAL.set_Checked(true);
						PlotForm.optPAL.set_Checked(true);
					}
					string text3;
					do
					{
						text3 = LineFromDataBlock(DataBlock, ref num, text).PadRight(40).Replace('"', ' ');
						if (text3.Substring(15, 10).Contains("NTSC"))
						{
							IsPAL = false;
							if (!FromLightCurve)
							{
								PlotForm.optNTSC.set_Checked(true);
								PlotForm.optSystemNTSC.set_Checked(true);
								CameraSpecs.VideoSystem = "NTSC";
							}
						}
						num8 = text3.IndexOf("FrameRate=");
						if (num8 > 0 && double.TryParse(text3.Substring(num8 + 10, 5), out result4))
						{
							HalfFrame_Interval = 0.5 / result4;
						}
					}
					while (!text3.Substring(0, 8).Contains("No.") && num >= 0);
					do
					{
						text3 = LineFromDataBlock(DataBlock, ref num, text);
						if ((text3 == null) | (text3 == "") | text3.Contains("test"))
						{
							continue;
						}
						string[] array4 = text3.Replace('"', ' ').Replace('\t', ',').Split(new char[1] { ',' });
						if (StarCount == 0)
						{
							num2 = 10;
							num3 = 16;
							num4 = 11;
							num5 = 12;
							Comp1Used = array4[11].Trim().Length > 0;
							Comp2Used = array4[12].Trim().Length > 0;
							Comp3Used = false;
							LiMovie_FieldsUsed = TimePresentInSourceData & (array4[6].Trim().Length == 0);
							int num16 = 1;
							if (Comp1Used)
							{
								num16 = 2;
							}
							if (Comp2Used)
							{
								num16 = 3;
							}
							if (Comp3Used)
							{
								num16 = 4;
							}
							if (num16 > 1)
							{
								SelectOccultedStar selectOccultedStar = new SelectOccultedStar(num16);
								((Control)selectOccultedStar.opt1).set_Enabled(num16 > 0);
								((Control)selectOccultedStar.opt2).set_Enabled(num16 > 1);
								((Control)selectOccultedStar.opt3).set_Enabled(num16 > 2);
								((Control)selectOccultedStar.opt4).set_Enabled(num16 > 3);
								((Form)selectOccultedStar).ShowDialog();
								int num17 = num2;
								if (OccStar == 1)
								{
									num2 = num4;
									num4 = num17;
									num3 = 33;
								}
								else if (OccStar == 2)
								{
									num2 = num5;
									num5 = num17;
									num3 = 50;
								}
								else if (OccStar == 3)
								{
									num2 = num6;
									num6 = num17;
								}
							}
						}
						try
						{
							Star[StarCount] = float.Parse(array4[num2].Replace('+', ' '));
							StarValid[StarCount] = true;
							StarLevel += Star[StarCount];
							StarForeground[StarCount] = float.Parse(array4[num3].Replace('+', ' '));
							StarBackground[StarCount] = float.Parse(array4[num3 + 1].Replace('+', ' '));
							Background += StarBackground[StarCount];
							FrameID[StarCount] = float.Parse(array4[0]);
						}
						catch
						{
							Star[StarCount] = 0f;
							StarValid[StarCount] = false;
							StarLevel += Star[StarCount];
							StarForeground[StarCount] = 0f;
							StarBackground[StarCount] = 0f;
							Background += StarBackground[StarCount];
							FrameID[StarCount] = 0f;
						}
						if (array4.Length < 5)
						{
							flag2 = false;
						}
						bool flag7 = false;
						if (array4[5].Trim().Length > 0)
						{
							flag2 = int.TryParse(array4[3].Trim(), out result) & int.TryParse(array4[4].Trim(), out result2) & double.TryParse(array4[5].Trim(), out result3);
						}
						else
						{
							flag2 = int.TryParse(array4[3].Trim(), out result) & int.TryParse(array4[4].Trim(), out result2) & double.TryParse(array4[6].Trim(), out result3);
							flag7 = true;
						}
						if (flag2)
						{
							num10 = (double)(result * 3600) + (double)result2 * 60.0 + result3;
							if (flag7)
							{
								num10 += HalfFrame_Interval;
							}
						}
						else
						{
							num10 = 0.0;
						}
						if (StarCount == 20)
						{
							num11 = GetFrameDuration(20);
						}
						if (StarCount > 20 && FrameInsertions_Allow)
						{
							double num18 = num10 - FrameTimeSecs[StarCount - 1];
							if ((num18 > 1.3 * num11) & (Math.Abs(num18) < 4.0 * num11))
							{
								int num19 = Convert.ToInt32(num18 / num11);
								for (int n = 1; n < num19; n++)
								{
									FrameTimeSecs[StarCount] = FrameTimeSecs[StarCount - 1] + num18;
									StarValid[StarCount] = false;
									StarForegroundAperture[StarCount] = 0f;
									StarBackground[StarCount] = 0f;
									Star[StarCount] = 0f;
									StarForeground[StarCount] = Star[StarCount];
									num9++;
									StarCount++;
									FramesAdded++;
									if (Comp1Used)
									{
										Comp1[Comp1Count] = 0f;
										Comp1Count++;
									}
									if (Comp2Used)
									{
										Comp2[Comp2Count] = 0f;
										Comp2Count++;
									}
									if (Comp3Used)
									{
										Comp3[Comp3Count] = 0f;
										Comp3Count++;
									}
								}
							}
						}
						FrameTimeSecs[StarCount] = num10;
						StarCount++;
						if (Comp1Used)
						{
							Comp1[Comp1Count] = float.Parse(array4[num4].Replace('+', ' '));
							Comp1Level += Comp1[Comp1Count];
							Comp1Count++;
						}
						if (Comp2Used)
						{
							Comp2[Comp2Count] = float.Parse(array4[num5].Replace('+', ' '));
							Comp2Level += Comp2[Comp2Count];
							Comp2Count++;
						}
						if (StarCount >= 50000)
						{
							break;
						}
					}
					while (num >= 0);
					num9 = EstablishIfTimesArePresent();
				}
				else if (text2.Substring(0, 10).Contains("R-OTE"))
				{
					CameraSpecs.MeasuringTool = "R-OTE";
					MeasurementEngine = " ,  processed with R-OTE";
					HalfFrame_Interval = 0.02;
					TimeSource = 1;
					num9 = (StarCount = 0);
					if (!FromLightCurve)
					{
						((Control)PlotForm.lblSetNTSCorPAL).set_Visible(false);
						CameraSpecs.VideoSystem = "PAL";
						PlotForm.optSystemPAL.set_Checked(true);
						PlotForm.optPAL.set_Checked(true);
					}
					do
					{
						string text3 = LineFromDataBlock(DataBlock, ref num, text).PadRight(40).Replace('"', ' ');
						if (text3.Substring(0, 8).Contains("RdgNum") || text3.Substring(0, 8).Contains("FrameNum"))
						{
							break;
						}
						if (text3.Substring(15, 10).Contains("NTSC"))
						{
							IsPAL = false;
							if (!FromLightCurve)
							{
								PlotForm.optNTSC.set_Checked(true);
								PlotForm.optSystemNTSC.set_Checked(true);
								CameraSpecs.VideoSystem = "NTSC";
							}
						}
						num8 = text3.IndexOf("FrameRate=");
						if (num8 > 0 && double.TryParse(text3.Substring(num8 + 10), out result4))
						{
							HalfFrame_Interval = 0.5 / result4;
						}
						if (!FromLightCurve)
						{
							continue;
						}
						num8 = text3.IndexOf("timestamp = ");
						if (num8 > 0)
						{
							if (int.TryParse(text3.Substring(num8 + 12, 4), out var result7))
							{
								LightData.LightCurveForm.LCD.Year = result7;
							}
							if (int.TryParse(text3.Substring(num8 + 17, 2), out result7))
							{
								LightData.LightCurveForm.LCD.Month = result7;
							}
							if (int.TryParse(text3.Substring(num8 + 19, 2), out result7))
							{
								LightData.LightCurveForm.LCD.Day = result7;
							}
							LightData.LightCurveForm.SetAsAsteroidal();
						}
					}
					while (num >= 0);
					do
					{
						string text3 = LineFromDataBlock(DataBlock, ref num, text);
						if ((text3 == null) | (text3 == ""))
						{
							continue;
						}
						string[] array5 = text3.Replace('"', ' ').Replace('\t', ',').Replace("[", "")
							.Replace("]", "")
							.Split(new char[1] { ',' });
						if (StarCount == 0)
						{
							num2 = 2;
							num3 = -1;
							num4 = 3;
							num5 = -1;
							Comp1Used = array5.Length > 3;
							Comp2Used = (Comp3Used = false);
							TimePresentInSourceData = !array5[1].Contains("Na");
							LiMovie_FieldsUsed = false;
							int num20 = 1;
							if (Comp1Used)
							{
								num20 = 2;
							}
							if (num20 > 1)
							{
								((Form)new SelectOccultedStar(num20)).ShowDialog();
								int num21 = num2;
								if (OccStar == 1)
								{
									num2 = num4;
									num4 = num21;
								}
							}
						}
						StarValid[StarCount] = float.TryParse(array5[num2].Replace('+', ' '), out Star[StarCount]);
						StarLevel += Star[StarCount];
						StarForeground[StarCount] = 0f;
						StarBackground[StarCount] = 0f;
						Background += StarBackground[StarCount];
						FrameID[StarCount] = float.Parse(array5[0]);
						if (array5[1].ToUpper().Contains("NA") | (array5[1].Trim() == ""))
						{
							num10 = 0.0;
						}
						else
						{
							num8 = array5[1].LastIndexOf(":");
							if (num8 > 0)
							{
								result3 = double.Parse(array5[1].Substring(num8 + 1));
								num7 = array5[1].LastIndexOf(":", num8 - 1);
								if (num7 > 0)
								{
									result = int.Parse(array5[1].Substring(0, num7));
									result2 = int.Parse(array5[1].Substring(num7 + 1, num8 - num7 - 1));
								}
								else
								{
									result = 0;
									result2 = int.Parse(array5[1].Substring(0, num8));
								}
							}
							else
							{
								result3 = double.Parse(array5[1]);
								result2 = (result = 0);
							}
							num10 = (double)(result * 3600) + (double)result2 * 60.0 + result3;
						}
						if (StarCount == 20)
						{
							num11 = GetFrameDuration(20);
						}
						if (StarCount > 20 && FrameInsertions_Allow)
						{
							double num22 = num10 - FrameTimeSecs[StarCount - 1];
							if ((num22 > 1.3 * num11) & (Math.Abs(num22) < 4.0 * num11))
							{
								int num23 = Convert.ToInt32(num22 / num11);
								for (int num24 = 1; num24 < num23; num24++)
								{
									FrameTimeSecs[StarCount] = FrameTimeSecs[StarCount - 1] + num22;
									StarValid[StarCount] = false;
									StarForegroundAperture[StarCount] = 0f;
									StarBackground[StarCount] = 0f;
									Star[StarCount] = 0f;
									StarForeground[StarCount] = Star[StarCount];
									num9++;
									StarCount++;
									FramesAdded++;
									if (Comp1Used)
									{
										Comp1[Comp1Count] = 0f;
										Comp1Count++;
									}
									if (Comp2Used)
									{
										Comp2[Comp2Count] = 0f;
										Comp2Count++;
									}
									if (Comp3Used)
									{
										Comp3[Comp3Count] = 0f;
										Comp3Count++;
									}
								}
							}
						}
						FrameTimeSecs[StarCount] = num10;
						StarCount++;
						if (Comp1Used)
						{
							Comp1[Comp1Count] = float.Parse(array5[num4].Replace('+', ' '));
							Comp1Level += Comp1[Comp1Count];
							Comp1Count++;
						}
						if (StarCount >= 50000)
						{
							break;
						}
					}
					while (num >= 0);
				}
				else if (text2.Substring(0, 10).Contains("PyMovie") | text2.Substring(0, 10).ToUpper().Contains("PYOTE"))
				{
					CameraSpecs.MeasuringTool = "PyMovie";
					MeasurementEngine = " ,  measured with PyMovie";
					HalfFrame_Interval = 0.02;
					TimeSource = 3;
					if (!FromLightCurve)
					{
						((Control)PlotForm.lblSetNTSCorPAL).set_Visible(false);
						CameraSpecs.VideoSystem = "PAL";
						PlotForm.optSystemPAL.set_Checked(true);
						PlotForm.optPAL.set_Checked(true);
					}
					string[] array6;
					do
					{
						string text3 = LineFromDataBlock(DataBlock, ref num, text);
						array6 = text3.Split(new char[1] { ',' });
					}
					while ((array6.Length < 2) | array6[0].Contains("#"));
					int num25 = array6.Length;
					int[] array7 = new int[6] { 0, 0, -1, -1, -1, -1 };
					string[] array8 = new string[4] { "...", "...", "...", "..." };
					int num26 = 0;
					Comp1Used = (Comp2Used = (Comp3Used = false));
					for (int num27 = 0; num27 < num25; num27++)
					{
						if (array6[num27].ToString() == "FrameNum")
						{
							array7[0] = num27;
						}
						else if (array6[num27].ToString() == "timeInfo")
						{
							array7[1] = num27;
						}
						else if (array6[num27].ToString().IndexOf("signal-") == 0)
						{
							array7[2 + num26] = num27;
							array8[num26] = array6[num27].ToString();
							switch (num26)
							{
							case 1:
								Comp1Used = true;
								break;
							case 2:
								Comp2Used = true;
								break;
							case 3:
								Comp3Used = true;
								break;
							}
							num26++;
							if (num26 > 3)
							{
								break;
							}
						}
						else if (array6[num27].ToString() == "primaryData")
						{
							array7[2] = num27;
						}
						else if (array6[num27].ToString() == "secondaryData")
						{
							array7[3] = num27;
							Comp1Used = true;
						}
					}
					num9 = (StarCount = (Comp1Count = (Comp2Count = (Comp3Count = 0))));
					if (num26 > 1)
					{
						SelectOccultedStar selectOccultedStar2 = new SelectOccultedStar(num26);
						((Control)selectOccultedStar2.opt1).set_Text(array8[0]);
						((Control)selectOccultedStar2.opt1).set_Enabled(true);
						if (num26 > 1)
						{
							((Control)selectOccultedStar2.opt2).set_Text(array8[1]);
							((Control)selectOccultedStar2.opt2).set_Enabled(true);
						}
						if (num26 > 2)
						{
							((Control)selectOccultedStar2.opt3).set_Text(array8[2]);
							((Control)selectOccultedStar2.opt3).set_Enabled(true);
						}
						if (num26 > 3)
						{
							((Control)selectOccultedStar2.opt4).set_Text(array8[3]);
							((Control)selectOccultedStar2.opt4).set_Enabled(true);
						}
						((Form)selectOccultedStar2).ShowDialog();
						int num28 = num2;
						if (OccStar == 1)
						{
							array7[2] = array7[3];
							array7[3] = num28 + 2;
						}
						else if (OccStar == 2)
						{
							array7[2] = array7[4];
							array7[4] = num28 + 2;
						}
						else if (OccStar == 3)
						{
							array7[2] = array7[5];
							array7[5] = num28 + 2;
						}
					}
					do
					{
						string text3 = LineFromDataBlock(DataBlock, ref num, text);
						if ((text3 == null) | (text3 == ""))
						{
							continue;
						}
						string[] array9 = text3.Split(new char[1] { ',' });
						float.TryParse(array9[array7[0]].ToString(), out result6);
						FrameID[StarCount] = (int)result6;
						string[] array10 = array9[array7[1]].ToString().Replace("[", "").Replace("]", "")
							.Split(new char[1] { ':' });
						if (array10.Length < 2)
						{
							num10 = 0.0;
						}
						else
						{
							int.TryParse(array10[0], out result);
							int.TryParse(array10[1], out result2);
							double.TryParse(array10[2], out result3);
							num10 = (double)(result * 3600) + (double)result2 * 60.0 + result3;
						}
						if (StarCount == 20)
						{
							num11 = GetFrameDuration(20);
						}
						if (flag3 && !flag4 && StarCount > 20)
						{
							double num29 = num10 - FrameTimeSecs[StarCount - 1];
							if ((num29 > 1.3 * num11) & (Math.Abs(num29) < 4.0 * num11))
							{
								if (flag3)
								{
									flag3 = (int)MessageBox.Show("A missing frame has been detected.\r\nDo you want to automatically add empty frames?\r\n\r\nSet YES If this is the first time the file has been read\r\nIf a large number of frames are added\r\nre-read the file and set NO", "Add missing frames?", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6;
									flag4 = true;
								}
								int num30 = Convert.ToInt32(num29 / num11);
								for (int num31 = 1; num31 < num30; num31++)
								{
									FrameTimeSecs[StarCount] = FrameTimeSecs[StarCount - 1] + num29;
									StarValid[StarCount] = false;
									StarForegroundAperture[StarCount] = 0f;
									StarBackground[StarCount] = 0f;
									Star[StarCount] = 0f;
									StarForeground[StarCount] = Star[StarCount];
									num9++;
									StarCount++;
									if (Comp1Used)
									{
										Comp1[Comp1Count] = 0f;
										Comp1Count++;
									}
									if (Comp2Used)
									{
										Comp2[Comp2Count] = 0f;
										Comp2Count++;
									}
									if (Comp3Used)
									{
										Comp3[Comp3Count] = 0f;
										Comp3Count++;
									}
								}
							}
						}
						FrameTimeSecs[StarCount] = num10;
						StarValid[StarCount] = float.TryParse(array9[array7[2]].ToString(), out result6);
						Star[StarCount] = (StarForeground[StarCount] = result6);
						StarLevel += Star[StarCount];
						StarCount++;
						if (Comp1Used)
						{
							float.TryParse(array9[array7[3]].ToString(), out result6);
							Comp1[Comp1Count] = (StarForeground[Comp1Count] = result6);
							Comp1Level += Comp1[Comp1Count];
							Comp1Count++;
						}
						if (Comp2Used)
						{
							float.TryParse(array9[array7[4]].ToString(), out result6);
							Comp2[Comp2Count] = (StarForeground[Comp2Count] = result6);
							Comp2Level += Comp2[Comp2Count];
							Comp2Count++;
						}
						if (Comp3Used)
						{
							float.TryParse(array9[array7[5]].ToString(), out result6);
							Comp3[Comp3Count] = (StarForeground[Comp3Count] = result6);
							Comp3Level += Comp3[Comp3Count];
							Comp3Count++;
						}
					}
					while (num >= 0);
					if (PlotForm != null)
					{
						double num32 = (FrameTimeSecs[10] - FrameTimeSecs[0]) / 10.0;
						if ((num32 > 0.039 && num32 < 0.041) || (num32 > 0.019 && num32 < 0.021))
						{
							PlotForm.optPAL.set_Checked(PlotForm.optSystemPAL.get_Checked());
							CameraSpecs.VideoSystem = "PAL";
						}
						else if ((num32 > 0.032 && num32 < 0.035) || (num32 > 0.0155 && num32 < 0.0175))
						{
							RadioButton optNTSC = PlotForm.optNTSC;
							bool @checked;
							PlotForm.optSystemNTSC.set_Checked(@checked = true);
							optNTSC.set_Checked(@checked);
							CameraSpecs.VideoSystem = "NTSC";
						}
						else
						{
							RadioButton optSystemOther = PlotForm.optSystemOther;
							bool @checked;
							PlotForm.optSystemOther.set_Checked(@checked = true);
							optSystemOther.set_Checked(@checked);
							CameraSpecs.VideoSystem = "Other";
						}
					}
				}
				else if (text2.Substring(0, 10).Contains("#Scanalyze"))
				{
					int num33 = 0;
					int num34 = 0;
					bool flag8 = false;
					float num35 = 0f;
					int num36 = 0;
					CameraSpecs.MeasuringTool = "Maxim";
					MeasurementEngine = " ,  measured with Scanalyzer";
					HalfFrame_Interval = 0.02;
					TimeSource = 2;
					num9 = (StarCount = 0);
					if (!FromLightCurve)
					{
						((Control)PlotForm.lblSetNTSCorPAL).set_Visible(false);
						CameraSpecs.VideoSystem = "Scanalyzer";
						PlotForm.optSystemOther.set_Checked(true);
						PlotForm.optPAL.set_Checked(true);
					}
					Comp1Used = (Comp2Used = (Comp3Used = false));
					do
					{
						string text3 = LineFromDataBlock(DataBlock, ref num, text).PadRight(40).Replace('"', ' ');
						double result8;
						if (text3.Contains("exposure start"))
						{
							num8 = text3.LastIndexOf(" ");
							double.TryParse(text3.Substring(num8).Replace(',', '.'), out result8);
							num33 = (int)result8 + 1;
						}
						else if (text3.Contains("exposure end"))
						{
							num8 = text3.LastIndexOf(" ");
							double.TryParse(text3.Substring(num8).Replace(',', '.'), out result8);
							num34 = (int)result8;
						}
						else if (text3.Contains("#Pixel,UTC,Intensity"))
						{
							break;
						}
					}
					while (num >= 0);
					do
					{
						string text3 = LineFromDataBlock(DataBlock, ref num, text);
						if ((text3 == null) | (text3 == ""))
						{
							continue;
						}
						int startIndex = text3.LastIndexOf(':');
						int num37 = text3.IndexOf(']');
						int num38 = text3.IndexOf(',', startIndex);
						if (num38 < num37)
						{
							text3 = text3.Substring(0, num38) + "." + text3.Substring(num38 + 1);
						}
						string[] array11 = text3.Split(new char[1] { ',' });
						float.TryParse(array11[0].ToString(), out result6);
						flag8 = (result6 < (float)(num33 - 2) && result6 >= (float)(num33 - 7)) || (result6 > (float)(num34 + 2) && result6 < (float)(num34 + 7));
						if (result6 < (float)num33 || result6 > (float)num34)
						{
							if (flag8)
							{
								float.TryParse(array11[2].ToString(), out result6);
								num35 += result6;
								num36++;
							}
							continue;
						}
						FrameID[StarCount] = (int)result6;
						string[] array12 = array11[1].ToString().Replace("[", "").Replace("]", "")
							.Split(new char[1] { ':' });
						if (array12.Length < 2)
						{
							num10 = 0.0;
						}
						else
						{
							int.TryParse(array12[0], out result);
							int.TryParse(array12[1], out result2);
							double.TryParse(array12[2].Replace(',', '.'), out result3);
							num10 = (double)(result * 3600) + (double)result2 * 60.0 + result3;
						}
						FrameTimeSecs[StarCount] = num10;
						StarValid[StarCount] = float.TryParse(array11[2].ToString(), out result6);
						Star[StarCount] = (StarForeground[StarCount] = result6);
						StarLevel += Star[StarCount];
						StarCount++;
					}
					while (num >= 0);
					num35 /= (float)num36;
					for (int num39 = 0; num39 < StarCount; num39++)
					{
						Star[num39] -= num35;
					}
					StarLevel -= (float)StarCount * num35;
				}
				else
				{
					if (!text2.Contains("date;SNR;flux;"))
					{
						continue;
					}
					CameraSpecs.MeasuringTool = "Unistellar";
					MeasurementEngine = " ,  measured by Unistellar";
					HalfFrame_Interval = 0.02;
					TimeSource = 1;
					Comp1Used = (Comp2Used = (Comp3Used = false));
					num9 = (StarCount = 0);
					double num40 = 0.2;
					double num41 = 0.3;
					string[] array13 = DataBlock.Split(new char[1] { '\n' });
					string[] array14 = array13[1].Replace('+', ' ').Split(new char[1] { ';' });
					string[] array15 = array14[0].Substring(11).Split(new char[1] { ':' });
					int num42 = int.Parse(array15[0]);
					int num43 = int.Parse(array15[1]);
					double num44 = double.Parse(array15[2]);
					array14 = array13[array13.Count() - 5].Replace('+', ' ').Split(new char[1] { ';' });
					string[] array16 = array14[0].Substring(11).Split(new char[1] { ':' });
					int num45 = int.Parse(array16[0]);
					int num46 = int.Parse(array16[1]);
					double num47 = double.Parse(array16[2]);
					int num48 = (int)(((double)(3600 * (num42 + num45) + 60 * (num43 + num46)) + (num44 + num47)) / 2.0);
					int num49 = num48 / 3600;
					int num50 = (num48 - 3600 * num49) / 60;
					int num51 = num48 - 3600 * num49 - 60 * num50;
					Unistellar_SetTimes unistellar_SetTimes = new Unistellar_SetTimes();
					((Control)unistellar_SetTimes.lblStart).set_Text(num42 + ":" + num43 + ":" + num44);
					((Control)unistellar_SetTimes.lblEnd).set_Text(num45 + ":" + num46 + ":" + num47);
					((Control)unistellar_SetTimes.txtHevent).set_Text(num49.ToString());
					((Control)unistellar_SetTimes.txtMevent).set_Text(num50.ToString());
					((Control)unistellar_SetTimes.txtSevent).set_Text(num51.ToString());
					((Form)unistellar_SetTimes).ShowDialog();
					int.TryParse(((Control)unistellar_SetTimes.txtHevent).get_Text(), out var result9);
					int.TryParse(((Control)unistellar_SetTimes.txtMevent).get_Text(), out var result10);
					double.TryParse(((Control)unistellar_SetTimes.txtSevent).get_Text(), out var result11);
					int.TryParse(((Control)unistellar_SetTimes.txtSemiDuration).get_Text(), out var result12);
					double num52 = (double)(result9 * 3600) + (double)result10 * 60.0 + result11 - (double)result12;
					double num53 = (double)(result9 * 3600) + (double)result10 * 60.0 + result11 + (double)result12;
					double[] array17 = new double[11];
					for (int num54 = 1; num54 < 12; num54++)
					{
						array14 = array13[num54].Replace('+', ' ').Split(new char[1] { ';' });
						string[] array18 = array14[0].Substring(11).Split(new char[1] { ':' });
						result = int.Parse(array18[0]);
						result2 = int.Parse(array18[1]);
						result3 = double.Parse(array18[2]);
						array17[num54 - 1] = (double)(result * 3600) + (double)result2 * 60.0 + result3;
					}
					num40 = (double)Convert.ToInt32((array17[10] - array17[0]) * 1000.0) / 10000.0;
					double num55 = (double)Convert.ToInt32((array17[1] - array17[0]) * 1000.0) / 1000.0;
					double num56 = (double)Convert.ToInt32((array17[2] - array17[1]) * 1000.0) / 1000.0;
					if (num55 < num40)
					{
						num40 = num55;
					}
					if (num56 < num40)
					{
						num40 = num56;
					}
					num41 = 1.6 * num40;
					int num57 = 0;
					int num58 = 0;
					for (int num59 = 1; num59 < array13.Count() - 1; num59++)
					{
						if (array13[num59].Length < 10)
						{
							continue;
						}
						array14 = array13[num59].Replace('+', ' ').Split(new char[1] { ';' });
						string[] array19 = array14[0].Substring(11).Split(new char[1] { ':' });
						result = int.Parse(array19[0]);
						result2 = int.Parse(array19[1]);
						result3 = double.Parse(array19[2]);
						double num60 = (double)(result * 3600) + (double)result2 * 60.0 + result3;
						if (num60 < num52)
						{
							continue;
						}
						if (num60 > num53)
						{
							break;
						}
						num57++;
						if (num57 > 2)
						{
							while (num60 - FrameTimeSecs[StarCount - 1] > num41)
							{
								FrameTimeSecs[StarCount] = (double)Convert.ToInt32((FrameTimeSecs[StarCount - 1] + num40) * 10000.0) / 10000.0;
								FrameID[StarCount] = StarCount;
								Star[StarCount] = 0f;
								StarValid[StarCount] = false;
								num58++;
								StarCount++;
							}
						}
						FrameTimeSecs[StarCount] = num60;
						FrameID[StarCount] = StarCount;
						Star[StarCount] = float.Parse(array14[2]) * 100f;
						StarValid[StarCount] = true;
						_ = 2;
						StarLevel += Star[StarCount];
						StarCount++;
					}
					MessageBox.Show(num57 + " Frames were read from the file\r\n" + num58 + " Frames have been added\r\n\r\n" + string.Format("=> {0,3:f1}% frames were missing\r\n\r\n", (double)num58 / (double)(num58 + num57) * 100.0) + "Any frames added near to the event\r\nwill slow down the analysis", "Missing frames count", (MessageBoxButtons)0, (MessageBoxIcon)64);
				}
			}
			while (num >= 0);
			StarLevel /= StarCount;
			Comp1Level /= StarCount;
			Comp2Level /= StarCount;
			Comp3Level /= StarCount;
			Background /= StarCount;
			if (num9 > 0)
			{
				MessageBox.Show("When reading the file, " + num9 + " frames were missing\r\n\r\nand " + num9 + " 'error' frames have been added", "Missing frames", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
			if (FromLightCurve)
			{
				((Control)LightData.LightCurveForm.chkComp1).set_Visible(Comp1Used);
				((Control)LightData.LightCurveForm.chkComp2).set_Visible(Comp2Used);
				((Control)LightData.LightCurveForm.chkComp3).set_Visible(Comp3Used);
				((Control)LightData.LightCurveForm.chkShowBackground).set_Enabled(Background != 0f);
				LightData.LightCurveForm.updnVerticalScaleAdjustment.set_Value(1m);
				LightData.LightCurveForm.chkComp1.set_Checked(Settings.Default.AOTAshowComparisons);
				LightData.LightCurveForm.chkComp2.set_Checked(Settings.Default.AOTAshowComparisons);
				LightData.LightCurveForm.chkComp3.set_Checked(Settings.Default.AOTAshowComparisons);
				LightData.LightCurveForm.chkShowBackground.set_Checked(false);
				((ListControl)LightData.LightCurveForm.cmbPlotAverage).set_SelectedIndex(0);
				LightData.LightCurveForm.chkScaleComp.set_Checked(true);
			}
			else
			{
				((Control)PlotForm.chkComp1).set_Visible(Comp1Used);
				((Control)PlotForm.chkComp2).set_Visible(Comp2Used);
				((Control)PlotForm.chkComp3).set_Visible(Comp3Used);
				((Control)PlotForm.chkShowBackground).set_Visible(Background != 0f);
				PlotForm.updnVerticalScaleAdjustment.set_Value(1m);
				PlotForm.chkStarPoints.set_Checked(true);
				PlotForm.chkComp1.set_Checked(Settings.Default.AOTAshowComparisons);
				PlotForm.chkComp2.set_Checked(Settings.Default.AOTAshowComparisons);
				PlotForm.chkComp3.set_Checked(Settings.Default.AOTAshowComparisons);
				PlotForm.chkShowBackground.set_Checked(false);
				((ListControl)PlotForm.cmbPlotAverage).set_SelectedIndex(0);
				if (IsPAL)
				{
					PlotForm.optPAL.set_Checked(true);
				}
				else
				{
					PlotForm.optNTSC.set_Checked(true);
				}
				PlotForm.CameraDelaySecs = 0.0;
				PlotForm.updnVerticalScaleAdjustment.set_Value(1m);
				((Control)PlotForm.grpCameraCorrections).set_Enabled(!CameraCorrectionsHaveBeenApplied);
				if (StarCount < 50)
				{
					MessageBox.Show("AOTA requires at least 50 measurements to analyse the light curve\r\n\r\nYour measurements contain only " + StarCount + ", and cannot be processed by AOTA", "Not enough measurements", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
				AutoProcess = true;
				((Control)PlotForm).set_Text(PlotFormBaseText + "    File being processed is  " + Path.GetFileName(CurrentFileName) + MeasurementEngine);
				((ListControl)PlotForm.cmbFrames).set_SelectedIndex(0);
				PlotForm.chkScaleComparisons.set_Checked(true);
				CameraSpecs.CameraType = PlotForm.cmbCamera.get_Items().get_Item(((ListControl)PlotForm.cmbCamera).get_SelectedIndex()).ToString();
			}
			CameraSpecs.MeasuredAtFieldLevel = FrameID[10] - FrameID[0] < 8f;
			CameraSpecs.FramesIntegrated = 0;
			if (TwoAndOnlyTwoTimesPresentInSourceData)
			{
				SetTimeScale();
			}
			EstablishIfTimesArePresent();
			if (!TimePresentInSourceData | OnlySomeTimesPresentInSourceData)
			{
				SetTimeScale();
			}
			EstablishIfTimesArePresent();
			if (TimePresentInSourceData)
			{
				ShowIntegrityCheck(FromLightCurve);
			}
			if (FromLightCurve)
			{
				SetLightCurvePlotParameters();
				return;
			}
			if (!AutoProcess & (PlotForm.tabPlot.get_SelectedIndex() == 6))
			{
				PlotForm.tabPlot.set_SelectedIndex(5);
			}
			AutoProcess = false;
			((Control)PlotForm).Focus();
		}

		internal static double GetFrameDuration(int NumberOfFramesToCheck)
		{
			double[] array = new double[NumberOfFramesToCheck];
			double num = 100.0;
			double num2 = 0.0;
			for (int i = 0; i < NumberOfFramesToCheck; i++)
			{
				array[i] = FrameTimeSecs[i + 1] - FrameTimeSecs[i];
			}
			for (int j = 0; j < NumberOfFramesToCheck; j++)
			{
				if ((array[j] > 0.0) & (array[j] < num))
				{
					num = array[j];
				}
			}
			for (int k = 0; k < NumberOfFramesToCheck; k++)
			{
				if ((array[k] >= num) & (array[k] <= 1.4 * num))
				{
					num2 = array[k];
				}
			}
			return (num + num2) / 2.0;
		}

		internal static void SetLightCurvePlotParameters()
		{
			StartOfLightCurveSelection = 0;
			EndOfLightCurveSelection = StarCount - 1;
			((Control)LightData.LightCurveForm.txtStart).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[StartOfLightCurveSelection]));
			((Control)LightData.LightCurveForm.txtEnd).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[EndOfLightCurveSelection - 1]));
			LightData.LightCurveForm.LCD.SetHMS = Utilities.DEGtoDMS(FrameTimeSecs[0] / 3600.0, 2, 2, MinutesOnly: false, IncludeLeadingZeros: true);
			LightData.LightCurveForm.LCD.Duration = FrameTimeSecs[EndOfLightCurveSelection - 1] - FrameTimeSecs[StartOfLightCurveSelection];
			LightData.LightCurveForm.LCD.NumPoints = (EndOfLightCurveSelection - StartOfLightCurveSelection) / NumberOfFramesBinned + 1;
			((Control)LightData.LightCurveForm.txtNumDataPoints).set_Text(LightData.LightCurveForm.LCD.NumPoints.ToString());
			((Control)LightData.LightCurveForm.txtFramesIntegrated).set_Text(NumberOfFramesBinned.ToString());
			if (LightData.LightCurveForm.LCD.NumPoints > 1)
			{
				LightData.LightCurveForm.LCD.Interval = LightData.LightCurveForm.LCD.Duration / (double)(LightData.LightCurveForm.LCD.NumPoints - 1);
			}
			else
			{
				LightData.LightCurveForm.LCD.Interval = 0.0;
			}
			decimal num = (decimal)(0.9 * (double)((Control)LightData.LightCurveForm).get_Width() / (double)LightData.LightCurveForm.LCD.NumPoints);
			if (num > 15m)
			{
				num = 15m;
			}
			if (num < 0.2m)
			{
				num = 0.2m;
			}
			LightData.LightCurveForm.updnScale.set_Value(num);
			LightData.LightCurveForm.SetSizes(ResizeEvent: false);
		}

		private static int EstablishIfTimesArePresent()
		{
			int num = 0;
			for (int i = 0; i < StarCount; i++)
			{
				if (FrameTimeSecs[i] == 0.0)
				{
					num++;
				}
			}
			TwoAndOnlyTwoTimesPresentInSourceData = StarCount - num == 2;
			if (StarCount - num > 1)
			{
				TimePresentInSourceData = true;
				HalfFrame_Interval = (FrameTimeSecs[StarCount - 1] - FrameTimeSecs[0]) / 2.0 / (double)StarCount;
				CameraSpecs.TimeScaleFromMeasuringTool = true;
			}
			else
			{
				TimeSource = 0;
				CameraSpecs.TimeScaleFromMeasuringTool = false;
			}
			OnlySomeTimesPresentInSourceData = false;
			if (StarCount > 10 && (double)(StarCount - num) / (double)StarCount < 0.8)
			{
				OnlySomeTimesPresentInSourceData = true;
			}
			return num;
		}

		internal static string LineFromDataBlock(string DataBlock, ref int CurrentPosition, string LineBreak)
		{
			string text = "";
			int num = 0;
			int num2 = DataBlock.IndexOf(LineBreak, CurrentPosition);
			num = num2 - CurrentPosition + 1;
			if (num2 > 0)
			{
				text = DataBlock.Substring(CurrentPosition, num).Replace(LineBreak, "").Replace("\r", "")
					.Replace("\n", "");
				CurrentPosition = num2 + 1;
			}
			else
			{
				text = DataBlock.Substring(CurrentPosition, DataBlock.Length - CurrentPosition).Replace(LineBreak, "").Replace("\r", "")
					.Replace("\n", "");
				CurrentPosition = -1;
			}
			return text;
		}

		internal static void SetPAL_NTSC_forTangra()
		{
			if (StarCount <= 15)
			{
				return;
			}
			double num = FrameTimeSecs[11] - FrameTimeSecs[1];
			if (Math.Abs(num - 0.4) < 0.02)
			{
				IsPAL = true;
				CameraSpecs.VideoSystem = "PAL";
				if (PlotForm != null)
				{
					PlotForm.optSystemPAL.set_Checked(true);
					PlotForm.optPAL.set_Checked(true);
					((Control)PlotForm.lblSetNTSCorPAL).set_Visible(false);
				}
			}
			else if (Math.Abs(num - 0.33) < 0.02)
			{
				CameraSpecs.VideoSystem = "NTSC";
				if (PlotForm != null)
				{
					PlotForm.optNTSC.set_Checked(true);
					PlotForm.optSystemNTSC.set_Checked(true);
					((Control)PlotForm.lblSetNTSCorPAL).set_Visible(false);
				}
			}
		}

		internal static void InsertDroppedFrame(int InsertPosition, int NumToInsert)
		{
			for (int num = StarCount; num >= InsertPosition; num--)
			{
				Star[num + NumToInsert] = Star[num];
				StarValid[num + NumToInsert] = StarValid[num];
				StarForeground[num + NumToInsert] = StarForeground[num];
				StarForegroundAperture[num + NumToInsert] = StarForegroundAperture[num];
				StarBackground[num + NumToInsert] = StarBackground[num];
				Comp1[num + NumToInsert] = Comp1[num];
				Comp2[num + NumToInsert] = Comp2[num];
				Comp3[num + NumToInsert] = Comp3[num];
				FrameID[num + NumToInsert] = FrameID[num];
				FrameTimeSecs[num + NumToInsert] = FrameTimeSecs[num];
			}
			for (int i = 0; i < NumToInsert; i++)
			{
				Star[InsertPosition + i] = 0f;
				StarValid[InsertPosition + i] = false;
				StarForeground[InsertPosition + i] = 0f;
				StarForegroundAperture[InsertPosition + i] = 0f;
				StarBackground[InsertPosition + i] = 0f;
				Comp1[InsertPosition + i] = 0f;
				Comp2[InsertPosition + i] = 0f;
				Comp3[InsertPosition + i] = 0f;
				FrameID[InsertPosition + i] = FrameID[InsertPosition - 1] + (FrameID[InsertPosition - 1] - FrameID[InsertPosition - 2]) * (float)(i + 1) / (float)(NumToInsert + 1);
				FrameTimeSecs[InsertPosition + i] = FrameTimeSecs[InsertPosition - 1] + (FrameTimeSecs[InsertPosition - 1] - FrameTimeSecs[InsertPosition - 2]) * (double)(i + 1);
				StarCount++;
				Comp1Count++;
				Comp2Count++;
				Comp3Count++;
			}
		}

		internal static void DeleteFrame(int DeletePosition)
		{
			for (int i = DeletePosition + 1; i < StarCount; i++)
			{
				Star[i - 1] = Star[i];
				StarValid[i - 1] = StarValid[i];
				StarForeground[i - 1] = StarForeground[i];
				StarForegroundAperture[i - 1] = StarForegroundAperture[i];
				StarBackground[i - 1] = StarBackground[i];
				Comp1[i - 1] = Comp1[i];
				Comp2[i - 1] = Comp2[i];
				Comp3[i - 1] = Comp3[i];
				FrameID[i - 1] = FrameID[i];
				FrameTimeSecs[i - 1] = FrameTimeSecs[i];
			}
			StarCount--;
			Comp1Count--;
			Comp2Count--;
			Comp3Count--;
		}

		internal static void SetTimeScale()
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new AOTA_SetTimeScale()).ShowDialog();
		}

		internal static void Set_DataToPlot(bool FromLightCurve)
		{
			//IL_0033: Unknown result type (might be due to invalid IL or missing references)
			//IL_022d: Unknown result type (might be due to invalid IL or missing references)
			//IL_04aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ab: Unknown result type (might be due to invalid IL or missing references)
			IntegrationFromLightCurve = FromLightCurve;
			int[] array = new int[6];
			double num = 0.0;
			double num2 = 1.0;
			int numberOfFramesBinned = NumberOfFramesBinned;
			if (!IntegrationPreset)
			{
				((Form)new SetIntegrationDetails()).ShowDialog();
			}
			if (CancelIntegration)
			{
				return;
			}
			int numberOfFramesBinned2 = NumberOfFramesBinned;
			if (!FromLightCurve)
			{
				if (numberOfFramesBinned2 != numberOfFramesBinned)
				{
					double num3 = (double)numberOfFramesBinned / (double)numberOfFramesBinned2;
					X0 = (int)((double)X0 * num3);
					X1 = (int)((double)X1 * num3);
					X2 = (int)((double)X2 * num3);
					X3 = (int)((double)X3 * num3);
					X4 = (int)((double)X4 * num3);
					X5 = (int)((double)X5 * num3);
					X6 = (int)((double)X6 * num3);
					X7 = (int)((double)X7 * num3);
					XFull = (int)((double)XFull * num3);
					XOcc = (int)((double)XOcc * num3);
				}
				CurrentlySelectedEvent = -1;
				CameraSpecs.MeasurementsBinned = NumberOfFramesBinned;
				if (PlotForm != null)
				{
					TextBox txtBeforeD = PlotForm.txtBeforeD;
					TextBox txtBeforeR = PlotForm.txtBeforeR;
					TextBox txtAfterD = PlotForm.txtAfterD;
					string text;
					((Control)PlotForm.txtAfterR).set_Text(text = "");
					string text2;
					((Control)txtAfterD).set_Text(text2 = text);
					string text3;
					((Control)txtBeforeR).set_Text(text3 = text2);
					((Control)txtBeforeD).set_Text(text3);
					PlotForm.updnScale.set_Value(1m);
					TextBox txtSN_at_D = PlotForm.txtSN_at_D;
					((Control)PlotForm.txtSN_at_R).set_Text(text3 = "");
					((Control)txtSN_at_D).set_Text(text3);
				}
				XFull = (XOcc = 0);
				YFull = (YOcc = 0.0);
				Chi2.InitialiseVariables();
				array[1] = X1;
				array[2] = X2;
				array[3] = X5;
				array[4] = X6;
				for (int i = 1; i < 5; i++)
				{
					array[i] = array[i] * NumberOfFramesBinned + FirstFrameUsedInAnalysis;
				}
			}
			TransferData_to_PlotArrays();
			if (AOTA_ExternalAccess.RunFromTangra && StarCountToPlot > 6000)
			{
				MessageBox.Show("AOTA has been called directly from Tangra, and after binning (if any) there are more than 6000 data points to be plotted.\r\n\r\nFor reasons 'unknown', this may cause a program crash if the AOTA form size is increased to a large size.\r\n\r\nIf this occurs, you can either\r\n1. re-run the analysis from Tangra, making sure the AOTA form is not too large, or\r\n2. Export the Tangra light curve to a .csv file. then run AOTA from Occult, and read that .csv file.", "Number of data points", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			if (!FromLightCurve)
			{
				if (PlotForm != null)
				{
					FourierCheck();
					PlotForm.opt25.set_Checked(true);
					RadioButton opt = PlotForm.opt9;
					RadioButton opt2 = PlotForm.opt17;
					RadioButton opt3 = PlotForm.opt33;
					RadioButton opt4 = PlotForm.opt41;
					RadioButton opt5 = PlotForm.opt49;
					bool flag;
					PlotForm.opt57.set_Checked(flag = false);
					bool flag2;
					opt5.set_Checked(flag2 = flag);
					bool flag3;
					opt4.set_Checked(flag3 = flag2);
					bool flag4;
					opt3.set_Checked(flag4 = flag3);
					bool @checked;
					opt2.set_Checked(@checked = flag4);
					opt.set_Checked(@checked);
					ShortEventsCheck(25, ForLightCurve: false);
					for (int j = 1; j < 5; j++)
					{
						array[j] = (array[j] - FirstFrameUsedInAnalysis) / NumberOfFramesBinned;
						if (array[j] < 1)
						{
							array[j] = 1;
						}
					}
					X1 = array[1];
					((Control)PlotForm.txtBeforeD).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[X1]));
					X0 = X1 - 150;
					if (X0 < 1)
					{
						X0 = 1;
					}
					X2 = array[2];
					((Control)PlotForm.txtAfterD).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[X2]));
					X5 = array[3];
					((Control)PlotForm.txtBeforeR).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[X5]));
					X6 = array[4];
					((Control)PlotForm.txtAfterR).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[X6]));
					X7 = X6 + 150;
					if (X7 >= StarCountToPlot)
					{
						X7 = StarCountToPlot - 1;
					}
					((ListControl)PlotForm.cmbIntegration).set_SelectedIndex((int)Math.Ceiling(Math.Log10(NumberOfFramesBinned) / 0.301031));
					if (StarCountToPlot <= 10)
					{
						MessageBox.Show("There are fewer than 11 data points to analyse. \r\nThis is too few to analyse", "Too few data points", (MessageBoxButtons)0, (MessageBoxIcon)48);
						return;
					}
					num = 0.0;
					for (int k = 0; k < StarCountToPlot; k++)
					{
						num += CrossStar[k];
					}
					num2 = num / (double)(StarCountToPlot - 1);
					for (int l = 0; l < StarCountToPlot; l++)
					{
						CrossStar[l] -= num2;
					}
					if (StarCountToPlot < 50)
					{
						MessageBox.Show("There are fewer than 50 data points to analyse. \r\nThis may be difficult to analyse", "Too few data points", (MessageBoxButtons)0, (MessageBoxIcon)48);
						int num4 = StarCountToPlot / 10 + 1;
						if (PlotForm.updnExtraPoints.get_Value() > (decimal)num4)
						{
							PlotForm.updnExtraPoints.set_Value((decimal)num4);
						}
					}
					CrossCorrDone = false;
					PlotForm.tabPlot.set_SelectedIndex(3);
					((Control)PlotForm).Focus();
					PlotForm.ResetLevels();
					Array.Clear(PlusLimit, 0, 2);
					Array.Clear(MinusLimit, 0, 2);
					Chi2.Chi2_D.Clear();
					Chi2.Chi2_R.Clear();
					PlotAOTA();
					if (AutoProcess)
					{
						PlotForm.SetLevelsAutomatically();
					}
					if (AutoProcess)
					{
						PlotForm.FindPossibleEvents();
					}
					if (PlotForm.tabPlot.get_SelectedIndex() == 6)
					{
						PlotForm.tabPlot.set_SelectedIndex(5);
					}
				}
			}
			else
			{
				StartOfLightCurveSelection = 0;
				EndOfLightCurveSelection = StarCount - 1;
				((Control)LightData.LightCurveForm.txtStart).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[StartOfLightCurveSelection]));
				((Control)LightData.LightCurveForm.txtEnd).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[EndOfLightCurveSelection]));
				((Control)LightData.LightCurveForm.txtNumDataPoints).set_Text(string.Format("{0,1:f0}", (EndOfLightCurveSelection - StartOfLightCurveSelection) / NumberOfFramesBinned + 1));
				((Control)LightData.LightCurveForm.txtFramesIntegrated).set_Text(NumberOfFramesBinned.ToString());
				if (AutoProcess)
				{
					PlotForm.SetLevelsAutomatically();
				}
				PlotLightCurve();
				ShortEventsCheck(25, ForLightCurve: true);
			}
			if (FramesAdded > 20)
			{
				MessageBox.Show(FramesAdded + " frames were added when reading the file\r\n\r\nThis may indicate the presence of significant irregularities \r\nin the time base. You should consider re-opening the \r\nfile with the menu item\r\n\r\n         File... Correct for Missing exposures\r\n\r\nunchecked", "Added frames", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
			}
			if (!FromLightCurve)
			{
				if (PlotForm != null)
				{
					PlotForm.correctForMissingExposuresToolStripMenuItem.set_Checked(true);
				}
			}
			else
			{
				LightData.LightCurveForm.correctForMissingExposuresToolStripMenuItem.set_Checked(true);
			}
		}

		private static void TransferData_to_PlotArrays()
		{
			int num = 0;
			float num2 = 0f;
			float num3 = 0f;
			for (int i = 0; i < StarCount; i++)
			{
				num3 += StarBackground[i];
			}
			num3 /= (float)StarCount;
			int num4 = 0;
			float num5 = 1.3f * num3 + 0.01f;
			num3 = 0f;
			for (int j = 0; j < StarCount; j++)
			{
				if (StarBackground[j] < num5)
				{
					num3 += StarBackground[j];
					num4++;
				}
			}
			num3 /= (float)num4;
			for (int k = FirstFrameUsedInAnalysis; k <= StarCount - NumberOfFramesBinned; k += NumberOfFramesBinned)
			{
				SumStar = (SumComp1 = (SumComp2 = (SumComp3 = (SumBackground = (SumAperture = 0f)))));
				StarValidToPlot[num] = false;
				num2 = 0f;
				for (int l = 0; l < NumberOfFramesBinned; l++)
				{
					if (StarValid[k + l])
					{
						SumStar += Star[k + l];
						SumBackground += StarBackground[k + l];
						SumAperture += StarForegroundAperture[k + l];
						StarValidToPlot[num] = true;
						if (Comp1Used)
						{
							SumComp1 += Comp1[k + l];
						}
						if (Comp2Used)
						{
							SumComp2 += Comp2[k + l];
						}
						if (Comp3Used)
						{
							SumComp3 += Comp3[k + l];
						}
						num2 += 1f;
					}
				}
				if (num2 > 0f)
				{
					if (CameraSpecs.MeasuringTool == "Tangra")
					{
						if (Background_Point)
						{
							CrossStar[num] = (StarToPlot[num] = (SumAperture - SumBackground) / num2);
						}
						else if (Background_Average)
						{
							CrossStar[num] = (StarToPlot[num] = SumAperture / num2 - num3);
						}
					}
					else
					{
						CrossStar[num] = (StarToPlot[num] = SumStar / num2);
					}
					if (Comp1Used)
					{
						Comp1ToPlot[num] = SumComp1 / num2;
					}
					if (Comp2Used)
					{
						Comp2ToPlot[num] = SumComp2 / num2;
					}
					if (Comp3Used)
					{
						Comp3ToPlot[num] = SumComp3 / num2;
					}
				}
				else
				{
					CrossStar[num] = (StarToPlot[num] = (Comp1ToPlot[num] = (Comp2ToPlot[num] = (Comp3ToPlot[num] = 0f))));
				}
				FrameIDofPlot[num] = FrameID[k];
				num++;
			}
			StarCountToPlot = num;
			if (NormalisationStarRef == 1)
			{
				CreateNormalisationArray(ref Comp1ToPlot, StarCountToPlot);
			}
			else if (NormalisationStarRef == 2)
			{
				CreateNormalisationArray(ref Comp2ToPlot, StarCountToPlot);
			}
			else if (NormalisationStarRef == 3)
			{
				CreateNormalisationArray(ref Comp3ToPlot, StarCountToPlot);
			}
			if (NormalisationStarRef > 0)
			{
				for (int m = 0; m < StarCountToPlot; m++)
				{
					StarToPlot[m] /= Normalisation[m];
					Comp1ToPlot[m] /= Normalisation[m];
					Comp2ToPlot[m] /= Normalisation[m];
					Comp3ToPlot[m] /= Normalisation[m];
				}
			}
		}

		internal static void CreateNormalisationArray(ref float[] Comp, int NumPoints)
		{
			int num = RunningAverage / 2;
			for (int i = 0; i < NumPoints; i++)
			{
				Normalisation[i] = 0f;
				for (int j = -num; j <= num; j++)
				{
					if (i + j < 0)
					{
						Normalisation[i] += Comp[0];
					}
					else if (i + j >= NumPoints - 1)
					{
						Normalisation[i] += Comp[NumPoints - 1];
					}
					else
					{
						Normalisation[i] += Comp[i + j];
					}
				}
			}
			float num2 = 0f;
			for (int k = 0; k < NumPoints; k++)
			{
				num2 += Normalisation[k];
			}
			num2 /= (float)NumPoints;
			for (int l = 0; l < NumPoints; l++)
			{
				Normalisation[l] /= num2;
				if (Normalisation[l] > 10f)
				{
					Normalisation[l] = 10f;
				}
				if ((double)Normalisation[l] < 0.1)
				{
					Normalisation[l] = 0.1f;
				}
			}
		}

		internal static void CreateRunningAverageArray(ref float[] Comp, ref float[] FromAve, int RunningAverageNumber)
		{
			CreateRunningAverageArray(ref Comp, RunningAverageNumber);
			int starCountToPlot = StarCountToPlot;
			for (int i = 0; i < starCountToPlot; i++)
			{
				FromAve[i] = Comp[i] - RunningAverageValues[i];
			}
		}

		internal static void CreateRunningAverageArray(ref float[] Comp, int RunningAverageNumber)
		{
			int num = RunningAverageNumber / 2;
			float num2 = 0f;
			int num3 = 0;
			int starCountToPlot = StarCountToPlot;
			RunningAverageValues = new float[starCountToPlot];
			if (RunningAverageNumber == 0)
			{
				for (int i = 0; i < starCountToPlot; i++)
				{
					RunningAverageValues[i] = Comp[i];
				}
				return;
			}
			for (int j = 0; j < starCountToPlot; j++)
			{
				num2 += Comp[j];
				num3++;
			}
			num2 = ((num3 <= 0) ? 1f : (num2 / (float)num3));
			for (int k = 0; k < starCountToPlot; k++)
			{
				_ = 2975;
				RunningAverageValues[k] = 0f;
				int num4 = 0;
				for (int l = -num; l <= num; l++)
				{
					if (k + l >= 0 && k + l < starCountToPlot - 1 && StarValidToPlot[k + l])
					{
						RunningAverageValues[k] += Comp[k + l];
						num4++;
					}
				}
				if (num4 > 0)
				{
					RunningAverageValues[k] /= num4;
				}
				else
				{
					RunningAverageValues[k] = num2;
				}
			}
		}

		internal static void CheckIntegrationDuration()
		{
			int[] array = new int[9] { 1, 2, 4, 8, 16, 32, 64, 128, 256 };
			double num = 0.0;
			using StreamWriter streamWriter = new StreamWriter("c:\\Temp\\CI.txt");
			for (int i = 1; i < array.Length; i++)
			{
				int num2 = array[i];
				int num3 = StarCount / num2;
				streamWriter.WriteLine("Integn = " + num2);
				streamWriter.WriteLine("Start Point, Value:  ");
				for (int j = 0; j < num2; j++)
				{
					double num4 = 0.0;
					for (int k = 0; k < num3; k++)
					{
						double num5;
						num = (num5 = 0.0);
						int num6 = k * num2 + j;
						for (int l = 0; l < num2; l++)
						{
							num5 += (double)Star[num6 + l];
						}
						num5 /= (double)num2;
						for (int m = 0; m < num2; m++)
						{
							num += ((double)Star[num6 + m] - num5) * ((double)Star[num6 + m] - num5);
						}
						num = Math.Sqrt(num) / (double)num2;
					}
					num4 += num;
					streamWriter.WriteLine(string.Format("{0,1}   {1,2:f4}, ", j, num4));
				}
			}
		}

		internal static void PlotAOTA()
		{
			//IL_29ee: Unknown result type (might be due to invalid IL or missing references)
			if (IsPlotting || ((StarCount < 10) | (StarCountToPlot < 10)))
			{
				return;
			}
			IsPlotting = true;
			float num = (float)PlotForm.updnScale.get_Value();
			float num2 = 1f;
			float num3 = 1f;
			float num4 = 1f;
			float num5 = 10000f;
			float num6 = -10000f;
			int num7 = 0;
			int num8 = 0;
			int num9 = 0;
			double num10 = 0.0;
			double num11 = 0.0;
			double num12 = 1.0;
			double num13 = 1.0;
			int num14 = 1;
			string text = "";
			bool flag = true;
			Font font = new Font("Arial", 8f, FontStyle.Bold);
			Brush black = Brushes.Black;
			Pen pen = new Pen(Color.Black, 1f);
			Pen pen2 = new Pen(Color.FromArgb(100, Color.Plum));
			Pen pen3 = new Pen(Color.BlueViolet, 0.7f);
			pen3.DashPattern = new float[4] { 2f, 15f, 1f, 3f };
			Pen pen4 = pen3;
			pen3 = new Pen(Color.DarkCyan, 0.7f);
			pen3.DashPattern = new float[4] { 2f, 15f, 1f, 3f };
			Pen pen5 = pen3;
			Pen pen6 = new Pen(Color.Orange, 1f);
			Pen pen7 = new Pen(Color.DarkGreen, 1.5f);
			pen7.DashPattern = new float[2] { 6f, 6f };
			Brush darkBlue = Brushes.DarkBlue;
			Brush fuchsia = Brushes.Fuchsia;
			new Pen(Color.DarkOrchid, 5f);
			Pen pen8 = new Pen(Color.FromArgb(250, Color.SaddleBrown));
			Pen pen9 = new Pen(Color.FromArgb(200, Color.MediumSeaGreen), 1.5f);
			Brush mediumSeaGreen = Brushes.MediumSeaGreen;
			Pen pen10 = new Pen(Color.FromArgb(200, Color.DarkOrange), 1.5f);
			Brush darkOrange = Brushes.DarkOrange;
			Pen pen11 = new Pen(Color.FromArgb(200, Color.MediumVioletRed), 1.5f);
			Brush mediumVioletRed = Brushes.MediumVioletRed;
			Pen pen12 = new Pen(Color.FromArgb(130, Color.DarkBlue), 1f);
			Pen pen13 = new Pen(Color.FromArgb(130, Color.Fuchsia), 1f);
			pen13.DashPattern = new float[2] { 2f, 6f };
			Pen pen14 = new Pen(Color.Crimson, 3f);
			pen3 = new Pen(Color.MediumSeaGreen, 2f);
			pen3.DashPattern = new float[2] { 2f, 4f };
			Pen pen15 = pen3;
			Pen pen16 = new Pen(Color.Green, 1f);
			Pen pen17 = new Pen(Color.Black, 2f);
			if (PlotForm.tabPlot.get_SelectedIndex() > 2)
			{
				pen17 = new Pen(Color.Gray, 0.5f);
			}
			pen17.DashPattern = new float[2] { 2f, 2f };
			Pen pen18 = new Pen(Color.Coral, 1f);
			pen18.DashPattern = new float[2] { 3f, 3f };
			Pen pen19 = new Pen(Color.Black, 1f);
			pen19.DashPattern = new float[2] { 10f, 3f };
			new Pen(Color.Cyan, 2f);
			Brush darkSalmon = Brushes.DarkSalmon;
			Pen pen20 = new Pen(Color.DarkCyan, 2f);
			pen20.DashPattern = new float[2] { 2f, 4f };
			Pen pen21 = new Pen(Color.DarkMagenta, 2f);
			pen21.DashPattern = new float[2] { 3f, 4f };
			Brush lemonChiffon = Brushes.LemonChiffon;
			_ = Brushes.LightCyan;
			_ = Brushes.Azure;
			Brush wheat = Brushes.Wheat;
			SolidBrush brush = new SolidBrush(Color.FromArgb(128, Color.Orange));
			SolidBrush brush2 = new SolidBrush(Color.FromArgb(128, Color.Orchid));
			GraphicsPath graphicsPath = new GraphicsPath();
			graphicsPath.AddLine(new Point(-2, 0), new Point(3, 0));
			CustomLineCap customEndCap = new CustomLineCap(null, graphicsPath);
			GraphicsPath graphicsPath2 = new GraphicsPath();
			graphicsPath2.AddLine(new Point(-3, 0), new Point(2, 0));
			CustomLineCap customStartCap = new CustomLineCap(null, graphicsPath2);
			Pen pen22 = new Pen(Color.Navy, 1f);
			pen22.DashPattern = new float[2] { 1f, 5f };
			pen22.CustomStartCap = customStartCap;
			pen22.CustomEndCap = customEndCap;
			Pen pen23 = new Pen(Color.DarkGreen, 1f);
			Pen pen24 = new Pen(Color.Red, 2f);
			Pen pen25 = new Pen(Color.DeepPink, 2f);
			pen25.DashStyle = DashStyle.Dot;
			PlotForm.SetSizes(ResizeEvent: false);
			int width = ((Control)PlotForm.picPlot).get_Width();
			int height = ((Control)PlotForm.picPlot).get_Height();
			if (width < 10 || height < 10)
			{
				IsPlotting = false;
				return;
			}
			_ = StartOfLightCurveSelection / NumberOfFramesBinned;
			_ = EndOfLightCurveSelection / NumberOfFramesBinned;
			for (int i = 0; i < StarCountToPlot && i < StarCountToPlot; i++)
			{
				if (StarToPlot[i] < num5)
				{
					num5 = StarToPlot[i];
				}
				if (StarToPlot[i] > num6)
				{
					num6 = StarToPlot[i];
				}
			}
			if (num5 >= 0f)
			{
				num5 = 0f;
			}
			num5 -= 0.05f * num6;
			num6 *= 1.05f;
			if (num6 != num5)
			{
				AOTA_VerticalScale = (float)height / (num6 - num5) * VerticalScaleAdjustment;
				AOTA_ZeroHeight = (0f - num5) / (num6 - num5) * (float)height;
			}
			else
			{
				AOTA_VerticalScale = 1f;
				AOTA_ZeroHeight = (float)height * 0.1f;
			}
			num4 = 1f / ((float)height - AOTA_ZeroHeight);
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Color.White);
			if (PlotForm.tabPlot.get_SelectedIndex() > 4)
			{
				if ((X1 > 0) & (X2 >= X1))
				{
					graphics.FillRectangle(lemonChiffon, (float)X1 * num, 0f, (float)(X2 - X1) * num, height);
				}
				if ((X5 > 0) & (X6 >= X2))
				{
					graphics.FillRectangle(lemonChiffon, (float)X5 * num, 0f, (float)(X6 - X5) * num, height);
				}
				if ((X3 > 0) & (X3 >= X2))
				{
					for (float num15 = (float)height / 40f; (double)num15 < 1.2 * (double)height; num15 += (float)height / 20f)
					{
						graphics.DrawLine(pen4, (float)X2 * num, num15, (float)X3 * num, num15);
					}
				}
				if ((X6 > 0) & (X7 >= X6))
				{
					for (float num16 = 0f; (double)num16 < 1.2 * (double)height; num16 += (float)height / 20f)
					{
						graphics.DrawLine(pen5, (float)X6 * num, num16, (float)X7 * num, num16);
					}
				}
				if (X1 > X0)
				{
					for (float num17 = (float)height / 40f; (double)num17 < 1.2 * (double)height; num17 += (float)height / 20f)
					{
						graphics.DrawLine(pen4, (float)X1 * num, num17, (float)X0 * num, num17);
					}
				}
				if ((X5 > 0) & (X5 >= X4))
				{
					for (float num18 = 0f; (double)num18 < 1.2 * (double)height; num18 += (float)height / 20f)
					{
						graphics.DrawLine(pen5, (float)X5 * num, num18, (float)X4 * num, num18);
					}
				}
			}
			float num19;
			if (PlotForm.tabPlot.get_SelectedIndex() > 0)
			{
				float x;
				float y;
				if (CrossCorrDone)
				{
					if ((PlotForm.tabPlot.get_SelectedIndex() != 2) & ((PlotForm.tabPlot.get_SelectedIndex() < 4) | PlotForm.chkShowCrossCorr.get_Checked()))
					{
						num19 = (float)((double)height - ((double)AOTA_ZeroHeight + FOMforNoOccn / (double)num4));
						try
						{
							graphics.FillRectangle(wheat, 0f, num19, (float)StarCountToPlot * num, (float)height - num19);
						}
						catch
						{
						}
					}
					if ((PlotForm.tabPlot.get_SelectedIndex() != 2) & ((PlotForm.tabPlot.get_SelectedIndex() < 5) | PlotForm.chkShowCrossCorr.get_Checked()))
					{
						x = 0f;
						y = 0f;
						for (int j = 0; j < StarCountToPlot; j++)
						{
							float num20 = (float)j * num;
							if (num20 > 32700f)
							{
								num20 = 32700f;
							}
							num19 = (float)((double)height - ((double)AOTA_ZeroHeight + CrossCorrR[j] / (double)num4));
							if (j > 0)
							{
								try
								{
									graphics.DrawLine(pen16, x, y, num20, num19);
								}
								catch
								{
								}
							}
							x = num20;
							y = num19;
						}
					}
					if ((MaxValues.Count > 0) & (PlotForm.tabPlot.get_SelectedIndex() > 2))
					{
						for (int k = 0; k < 5; k++)
						{
							Pen pen26 = pen18;
							if (PlotForm.chkDisplayEvent[k].get_Checked())
							{
								pen26 = pen17;
							}
							pen26.DashPattern = new float[2] { 2f, 2f };
							if (MaxValues[k].Width <= 1)
							{
								continue;
							}
							x = 0f;
							y = (num19 = (float)height - (AOTA_ZeroHeight + (float)YFull * AOTA_VerticalScale));
							float num20 = ((float)MaxValues[k].Pos - (float)MaxValues[k].Width / 2f) * num;
							if (num20 > 32700f)
							{
								num20 = 32700f;
							}
							graphics.DrawLine(pen26, x, y, num20, num19);
							x = num20;
							y = num19;
							num19 = (float)height - (AOTA_ZeroHeight + (float)YOcc * AOTA_VerticalScale);
							graphics.DrawLine(pen26, x, y, num20, num19);
							x = num20;
							y = num19;
							num20 = ((float)MaxValues[k].Pos + (float)MaxValues[k].Width / 2f) * num;
							if (num20 > 32700f)
							{
								num20 = 32700f;
							}
							graphics.DrawLine(pen26, x, y, num20, num19);
							x = num20;
							y = num19;
							num19 = (float)height - (AOTA_ZeroHeight + (float)YFull * AOTA_VerticalScale);
							graphics.DrawLine(pen26, x, y, num20, num19);
							x = num20;
							y = num19;
							num20 = (float)StarCountToPlot * num;
							if (num20 > 32700f)
							{
								num20 = 32700f;
							}
							graphics.DrawLine(pen26, x, y, num20, num19);
							int num21 = (int)((float)(MaxValues[k].Width - 4) / 5f);
							if (num21 < 5)
							{
								num8 = (int)((float)MaxValues[k].Pos - (float)MaxValues[k].Width / 2f + 2f);
								num9 = (int)((float)MaxValues[k].Pos + (float)MaxValues[k].Width / 2f - 2f);
								if (num9 - num8 > 2)
								{
									x = (float)num8 * num;
									num20 = (float)num9 * num;
									if (num20 > 32700f)
									{
										num20 = 32700f;
									}
									num10 = 0.0;
									for (int l = num8; l <= num9; l++)
									{
										num10 += (double)StarToPlot[l];
									}
									num10 /= (double)(num9 - num8 + 1);
									graphics.DrawLine(pen19, x, num19, num20, num19);
									num11 = 0.0;
									for (int m = num8; m <= num9; m++)
									{
										num11 += ((double)StarToPlot[m] - num10) * ((double)StarToPlot[m] - num10);
									}
									num11 = Math.Sqrt(num11) / (double)(num9 - num8) * (double)AOTA_VerticalScale;
									num19 = (float)((double)height - ((double)AOTA_ZeroHeight + num10 * (double)AOTA_VerticalScale));
									graphics.FillRectangle(darkSalmon, x, (float)((double)num19 - num11), num20 - x, (float)(2.0 * num11));
									graphics.DrawLine(pen19, x, num19, num20, num19);
								}
								continue;
							}
							for (int n = 0; n < 5; n++)
							{
								num8 = (int)((float)MaxValues[k].Pos - (float)(MaxValues[k].Width - 4) / 2f + (float)(n * num21));
								num9 = num8 + num21;
								x = (float)num8 * num;
								num20 = (float)num9 * num;
								if (num20 > 32700f)
								{
									num20 = 32700f;
								}
								num10 = 0.0;
								for (int num22 = num8; num22 <= num9; num22++)
								{
									num10 += (double)StarToPlot[num22];
								}
								num10 /= (double)(num9 - num8 + 1);
								num11 = 0.0;
								for (int num23 = num8; num23 <= num9; num23++)
								{
									num11 += ((double)StarToPlot[num23] - num10) * ((double)StarToPlot[num23] - num10);
								}
								num11 = Math.Sqrt(num11) / (double)(num9 - num8) * (double)AOTA_VerticalScale;
								num19 = (float)((double)height - ((double)AOTA_ZeroHeight + num10 * (double)AOTA_VerticalScale));
								graphics.FillRectangle(darkSalmon, x, (float)((double)num19 - num11), num20 - x, (float)(2.0 * num11));
								graphics.DrawLine(pen19, x, num19, num20, num19);
							}
						}
					}
				}
				if ((PlotForm.tabPlot.get_SelectedIndex() != 2) & (PlotForm.tabPlot.get_SelectedIndex() < 4))
				{
					num7 = XFull - 20;
					if (num7 < 0)
					{
						num7 = 0;
					}
					x = (float)num7 * num;
					num7 = XFull + 20;
					if (num7 >= StarCountToPlot)
					{
						num7 = StarCountToPlot;
					}
					float num20 = (float)num7 * num;
					if (num20 > 32700f)
					{
						num20 = 32700f;
					}
					num19 = (float)((double)height - ((double)AOTA_ZeroHeight + YFull * (double)AOTA_VerticalScale));
					graphics.DrawLine(pen14, x, num19, num20, num19);
					num7 = XOcc - 2;
					if (num7 < 0)
					{
						num7 = 0;
					}
					x = (float)num7 * num;
					num7 = XOcc + 2;
					if (num7 >= StarCountToPlot)
					{
						num7 = StarCountToPlot;
					}
					num20 = (float)num7 * num;
					if (num20 > 32700f)
					{
						num20 = 32700f;
					}
					num19 = (float)((double)height - ((double)AOTA_ZeroHeight + YOcc * (double)AOTA_VerticalScale));
					pen14.DashStyle = DashStyle.Dot;
					graphics.DrawLine(pen14, x - 5f * num, num19, num20 + 5f * num, num19);
					pen14.DashStyle = DashStyle.Solid;
					graphics.DrawLine(pen14, x, num19, num20, num19);
					if ((YExpected > 0.0) & (PlotForm.updnMagDrop.get_Value() > 0.05m))
					{
						x = 0f;
						num20 = (float)StarCountToPlot * num;
						if (num20 > 32700f)
						{
							num20 = 32700f;
						}
						num19 = (float)((double)height - ((double)AOTA_ZeroHeight + YExpected * (double)AOTA_VerticalScale));
						graphics.DrawLine(pen15, x, num19, num20, num19);
					}
				}
				num19 = (float)height - (AOTA_ZeroHeight + AOTA_Height_100 * AOTA_VerticalScale);
				if (num19 > 10f)
				{
					graphics.DrawLine(pen7, 0f, num19, width, num19);
				}
				if (PlotForm.tabPlot.get_SelectedIndex() > 0)
				{
					num14 = ((PlotForm.updnScale.get_Value() > 10m) ? 5 : ((PlotForm.updnScale.get_Value() >= 5m) ? 10 : ((PlotForm.updnScale.get_Value() >= 2m) ? 20 : ((PlotForm.updnScale.get_Value() >= 1m) ? 40 : ((!(PlotForm.updnScale.get_Value() > 0.5m)) ? 200 : 100)))));
					bool flag2 = false;
					if (FrameIDofPlot[2] - FrameIDofPlot[0] < 2f)
					{
						flag2 = true;
					}
					for (int num24 = 0; num24 < StarCountToPlot; num24++)
					{
						if ((((NumberOfFramesBinned == 1) & (FrameIDofPlot[num24] % (float)num14 == 0f)) | ((NumberOfFramesBinned != 1) & (num24 % num14 == 0))) && (num24 <= 0 || FrameIDofPlot[num24] != FrameIDofPlot[num24 - 1]))
						{
							text = string.Format("{0,1:f0}", FrameIDofPlot[num24]);
							if (flag2)
							{
								text += ".0";
							}
							float num20 = ((float)num24 - 0.5f) * num;
							graphics.DrawLine(pen2, num20, 0f, num20, height);
							graphics.DrawLine(pen, num20, 0f, num20, 10f);
							graphics.DrawLine(pen, num20, height - 14, num20, height - 20);
							graphics.DrawLine(pen, num20, height - 15, num20 + num, height - 15);
							graphics.DrawLine(pen, num20 + num, height - 14, num20 + num, height - 16);
							graphics.DrawString(text, font, black, num20 + num / 2f - graphics.MeasureString(text, font).Width / 2f + 1f, height - 14);
						}
					}
				}
				((Control)PlotForm.cmdVerifyTimes).set_Enabled(AOTA_ExternalAccess.AOTA_Client != null);
				if (AOTA_ExternalAccess.AOTA_Client != null)
				{
					int num25;
					for (num25 = 0; num25 < StarCountToPlot && !(FrameIDofPlot[num25] >= AOTA_ExternalAccess.FrameID_DisplayedInTangra); num25++)
					{
					}
					if (num25 < StarCountToPlot)
					{
						float num20 = ((float)num25 - 0.5f) * num;
						if (AOTA_ExternalAccess.TangraFrameDisplayFromAOTA)
						{
							graphics.FillRectangle(brush2, num20, 0f, num, height);
						}
						if (AOTA_ExternalAccess.TangraFrameDisplayFromTangra)
						{
							graphics.FillRectangle(brush, num20, 0f, num, height);
						}
					}
				}
				x = (y = 0f);
				if (PlotForm.tabPlot.get_SelectedIndex() > 3)
				{
					for (int num26 = 0; num26 < MaximimumTransitions + 1; num26++)
					{
						Chi2_MinimaPositions.Index = num26;
						if (num26 == 0)
						{
							num12 = Chi2.Chi2_D_Minimums[0] * 5.0;
						}
						if (!((Chi2D_Selected == -1) | (num26 == Chi2D_Selected)))
						{
							continue;
						}
						for (int num27 = 0; num27 < Chi2.Chi2_D.Count; num27++)
						{
							float num20 = (float)Chi2.Chi2_D[num27].Pos * num;
							if (num20 > 32700f)
							{
								num20 = 32700f;
							}
							num19 = (float)((double)height - ((double)AOTA_ZeroHeight + Chi2.Chi2_D[num27].Chi2_value / num12 / (double)num4));
							if (num27 > 0 && !InValidPlotValues(num20, num19, x, y))
							{
								try
								{
									graphics.DrawLine(pen23, x, y, num20, num19);
								}
								catch
								{
								}
							}
							x = num20;
							y = num19;
						}
						if (Chi2D_Selected <= -1 || !((Chi2.Chi2_D_MinimumPosns[Chi2D_Selected] >= X1) & (Chi2.Chi2_D_MinimumPosns[Chi2D_Selected] <= X2)))
						{
							continue;
						}
						Chi2.CreateTestSignalArray(Chi2D_Selected, IsDisappear: true, Chi2.Chi2_D_MinimumPosns[Chi2D_Selected] - X1);
						for (int num28 = 0; num28 < Chi2.LengthOfSignal; num28++)
						{
							float num20 = (float)(X0 + num28) * num;
							if (num20 > 32700f)
							{
								num20 = 32700f;
							}
							num19 = (float)((double)height - ((double)AOTA_ZeroHeight + Chi2.Test[num28] * (double)AOTA_VerticalScale));
							if (num28 > 0 && !InValidPlotValues(num20, num19, x, y))
							{
								graphics.DrawLine(pen25, x, y, num20, num19);
							}
							x = num20;
							y = num19;
						}
						if (MinusLimit[0] != 0f)
						{
							for (int num29 = 0; num29 < Chi2.LengthOfSignal; num29++)
							{
								float num20 = ((float)(X0 + num29) + MinusLimit[0]) * num;
								if (num20 > 32700f)
								{
									num20 = 32700f;
								}
								num19 = (float)((double)height - ((double)AOTA_ZeroHeight + Chi2.Test[num29] * (double)AOTA_VerticalScale));
								if (num29 > 0 && !InValidPlotValues(num20, num19, x, y))
								{
									graphics.DrawLine(pen24, x, y, num20, num19);
								}
								x = num20;
								y = num19;
							}
						}
						if (PlusLimit[0] == 0f)
						{
							continue;
						}
						for (int num30 = 0; num30 < Chi2.LengthOfSignal; num30++)
						{
							float num20 = ((float)(X0 + num30) + PlusLimit[0]) * num;
							if (num20 > 32700f)
							{
								num20 = 32700f;
							}
							num19 = (float)((double)height - ((double)AOTA_ZeroHeight + Chi2.Test[num30] * (double)AOTA_VerticalScale));
							if (num30 > 0 && !InValidPlotValues(num20, num19, x, y))
							{
								graphics.DrawLine(pen24, x, y, num20, num19);
							}
							x = num20;
							y = num19;
						}
					}
					for (int num31 = 0; num31 < MaximimumTransitions + 1; num31++)
					{
						Chi2_MinimaPositions.Index = num31;
						if (num31 == 0)
						{
							num13 = Chi2.Chi2_R_Minimums[0] * 5.0;
						}
						if (!((Chi2R_Selected == -1) | (num31 == Chi2R_Selected)))
						{
							continue;
						}
						for (int num32 = 0; num32 < Chi2.Chi2_R.Count; num32++)
						{
							float num20 = (float)Chi2.Chi2_R[num32].Pos * num;
							if (num20 > 32700f)
							{
								num20 = 32700f;
							}
							num19 = (float)((double)height - ((double)AOTA_ZeroHeight + Chi2.Chi2_R[num32].Chi2_value / num13 / (double)num4));
							if (num32 > 0 && !InValidPlotValues(num20, num19, x, y))
							{
								try
								{
									graphics.DrawLine(pen23, x, y, num20, num19);
								}
								catch
								{
								}
							}
							x = num20;
							y = num19;
						}
						if (Chi2R_Selected <= -1 || !((Chi2.Chi2_R_MinimumPosns[Chi2R_Selected] >= X5) & (Chi2.Chi2_R_MinimumPosns[Chi2R_Selected] <= X6)))
						{
							continue;
						}
						Chi2.CreateTestSignalArray(Chi2R_Selected, IsDisappear: false, Chi2.Chi2_R_MinimumPosns[Chi2R_Selected] - X5);
						for (int num33 = 0; num33 < Chi2.LengthOfSignal; num33++)
						{
							float num20 = (float)(X4 + num33) * num;
							if (num20 > 32700f)
							{
								num20 = 32700f;
							}
							num19 = (float)((double)height - ((double)AOTA_ZeroHeight + Chi2.Test[num33] * (double)AOTA_VerticalScale));
							if (num33 > 0 && !InValidPlotValues(num20, num19, x, y))
							{
								graphics.DrawLine(pen25, x, y, num20, num19);
							}
							x = num20;
							y = num19;
						}
						if (MinusLimit[1] != 0f)
						{
							for (int num34 = 0; num34 < Chi2.LengthOfSignal; num34++)
							{
								float num20 = ((float)(X4 + num34) + MinusLimit[1]) * num;
								if (num20 > 32700f)
								{
									num20 = 32700f;
								}
								num19 = (float)((double)height - ((double)AOTA_ZeroHeight + Chi2.Test[num34] * (double)AOTA_VerticalScale));
								if (num34 > 0 && !InValidPlotValues(num20, num19, x, y))
								{
									graphics.DrawLine(pen24, x, y, num20, num19);
								}
								x = num20;
								y = num19;
							}
						}
						if (PlusLimit[1] == 0f)
						{
							continue;
						}
						for (int num35 = 0; num35 < Chi2.LengthOfSignal; num35++)
						{
							float num20 = ((float)(X4 + num35) + PlusLimit[1]) * num;
							if (num20 > 32700f)
							{
								num20 = 32700f;
							}
							num19 = (float)((double)height - ((double)AOTA_ZeroHeight + Chi2.Test[num35] * (double)AOTA_VerticalScale));
							if (num35 > 0 && !InValidPlotValues(num20, num19, x, y))
							{
								graphics.DrawLine(pen24, x, y, num20, num19);
							}
							x = num20;
							y = num19;
						}
					}
				}
			}
			num19 = (float)height - AOTA_ZeroHeight;
			graphics.DrawLine(pen6, 0f, num19, width, num19);
			MeanBefore = (SDBefore = (VarianceBefore = (MeanDuring = (SDduring = (VarianceDuring = (MeanAfter = (SDAfter = (VarianceAfter = 0.0))))))));
			if (PlotForm.tabPlot.get_SelectedIndex() < 3 && GetMean_SD(0, StarCountToPlot, out MeanAfter, out SDAfter, out VarianceAfter))
			{
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + (MeanAfter + SDAfter) * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen20, 0f, num19, (float)StarCountToPlot * num, num19);
				graphics.DrawString("+1σ", font, black, 4f, num19 - 14f);
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + (MeanAfter - SDAfter) * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen20, 0f, num19, (float)StarCountToPlot * num, num19);
				graphics.DrawString("-1σ", font, black, 4f, num19);
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + MeanAfter * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen, 0f, num19, (float)StarCountToPlot * num, num19);
			}
			if (((X0 > 0) & (X1 > X0)) && GetMean_SD(X0, X1, out MeanBefore, out SDBefore, out VarianceBefore) && ((PlotForm.tabPlot.get_SelectedIndex() > 2) & PlotForm.chkShowMeasurementMeans.get_Checked()))
			{
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + (MeanBefore + SDBefore) * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen20, (float)X0 * num, num19, (float)X1 * num, num19);
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + (MeanBefore - SDBefore) * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen20, (float)X0 * num, num19, (float)X1 * num, num19);
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + MeanBefore * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen, (float)X0 * num, num19, (float)X1 * num, num19);
			}
			if (((X2 > 0) & (X5 > X2)) && GetMean_SD(X2, X5, out MeanDuring, out SDduring, out VarianceDuring) && ((PlotForm.tabPlot.get_SelectedIndex() > 2) & PlotForm.chkShowMeasurementMeans.get_Checked()))
			{
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + (MeanDuring + SDduring) * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen21, (float)X2 * num, num19, (float)X5 * num, num19);
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + (MeanDuring - SDduring) * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen21, (float)X2 * num, num19, (float)X5 * num, num19);
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + MeanDuring * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen, (float)X2 * num, num19, (float)X5 * num, num19);
			}
			if (((X6 > 0) & (X7 > X6)) && GetMean_SD(X6, X7, out MeanAfter, out SDAfter, out VarianceAfter) && ((PlotForm.tabPlot.get_SelectedIndex() > 2) & PlotForm.chkShowMeasurementMeans.get_Checked()))
			{
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + (MeanAfter + SDAfter) * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen20, (float)X6 * num, num19, (float)X7 * num, num19);
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + (MeanAfter - SDAfter) * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen20, (float)X6 * num, num19, (float)X7 * num, num19);
				num19 = (float)((double)height - ((double)AOTA_ZeroHeight + MeanAfter * (double)AOTA_VerticalScale));
				graphics.DrawLine(pen, (float)X6 * num, num19, (float)X7 * num, num19);
			}
			if (CurrentlySelectedEvent >= 0)
			{
				if (PlotForm.optChi2StdDevn.get_Checked())
				{
					if (Math.Abs(SDBefore + SDduring) < 0.1)
					{
						Results[CurrentlySelectedEvent].D_SN = 0f;
					}
					else
					{
						Results[CurrentlySelectedEvent].D_SN = (float)((MeanBefore - MeanDuring) / ((SDBefore + SDduring) / 2.0));
					}
					if (Math.Abs(SDAfter + SDduring) < 0.1)
					{
						Results[CurrentlySelectedEvent].R_SN = 0f;
					}
					else
					{
						Results[CurrentlySelectedEvent].R_SN = (float)((MeanAfter - MeanDuring) / ((SDAfter + SDduring) / 2.0));
					}
				}
				else
				{
					if (Math.Abs(VarianceBefore + VarianceDuring) < 0.1)
					{
						Results[CurrentlySelectedEvent].D_SN = 0f;
					}
					else
					{
						Results[CurrentlySelectedEvent].D_SN = (float)((MeanBefore - MeanDuring) / Math.Sqrt((VarianceBefore + VarianceDuring) / 2.0));
					}
					if (Math.Abs(VarianceAfter + VarianceDuring) < 0.1)
					{
						Results[CurrentlySelectedEvent].R_SN = 0f;
					}
					else
					{
						Results[CurrentlySelectedEvent].R_SN = (float)((MeanAfter - MeanDuring) / Math.Sqrt((VarianceAfter + VarianceDuring) / 2.0));
					}
				}
				Results[CurrentlySelectedEvent].D_SN = (float)Math.Round(Results[CurrentlySelectedEvent].D_SN, 1);
				((Control)PlotForm.txtSN_at_D).set_Text(string.Format("{0,1:f1}", Results[CurrentlySelectedEvent].D_SN));
				Results[CurrentlySelectedEvent].R_SN = (float)Math.Round(Results[CurrentlySelectedEvent].R_SN, 1);
				((Control)PlotForm.txtSN_at_R).set_Text(string.Format("{0,1:f1}", Results[CurrentlySelectedEvent].R_SN));
				((Control)PlotForm.lblSNR).set_Text(string.Format("SnR = {0,3:f1}", (double)(Results[CurrentlySelectedEvent].D_SN + Results[CurrentlySelectedEvent].R_SN) / 2.0));
				if (GetMean_SD(StarCountToPlot / 2 - 50, StarCountToPlot / 2 + 50, out var Mean, out var SD, out var _))
				{
					double num36 = Mean * Math.Pow(10.0, (0.0 - (double)PlotForm.updnExpectedMagDrop.get_Value()) / 2.5);
					ExpectedSN_Miss = (float)((Mean - num36) / SD);
				}
			}
			if (PlotForm.chkShowErrorBars.get_Checked() && PlotForm.tabPlot.get_SelectedIndex() > 3)
			{
				float yTop;
				float yBottom;
				if ((X1 > 0) & (X2 > X1))
				{
					for (int num37 = X1 - ExtraPointsUsedInSolution; num37 <= X2 + ExtraPointsUsedInSolution; num37++)
					{
						if (!((num37 < 0) | (num37 >= StarCountToPlot)) && StarValidToPlot[num37])
						{
							float num20 = (float)num37 * num;
							Scale_SD(SDBefore, SDduring, MeanBefore, MeanDuring, StarToPlot[num37], out yTop, out yBottom);
							yTop = (float)height - (AOTA_ZeroHeight + yTop * AOTA_VerticalScale);
							yBottom = (float)height - (AOTA_ZeroHeight + yBottom * AOTA_VerticalScale);
							graphics.DrawLine(pen22, num20, yTop, num20, yBottom);
						}
					}
				}
				if ((X5 > 0) & (X6 > X2))
				{
					for (int num38 = X5 - ExtraPointsUsedInSolution; num38 <= X6 + ExtraPointsUsedInSolution; num38++)
					{
						if (!((num38 < 0) | (num38 >= StarCountToPlot)) && StarValidToPlot[num38])
						{
							float num20 = (float)num38 * num;
							Scale_SD(SDAfter, SDduring, MeanAfter, MeanDuring, StarToPlot[num38], out yTop, out yBottom);
							yTop = (float)height - (AOTA_ZeroHeight + yTop * AOTA_VerticalScale);
							yBottom = (float)height - (AOTA_ZeroHeight + yBottom * AOTA_VerticalScale);
							graphics.DrawLine(pen22, num20, yTop, num20, yBottom);
						}
					}
				}
			}
			graphics.DrawRectangle(pen, 0, 0, width - 1, height - 1);
			float num39 = 1f + (float)PlotForm.updnScale.get_Value() / 8f;
			if (!PlotForm.plotUsingLargePointsToolStripMenuItem.get_Checked())
			{
				num39 = 1f;
			}
			for (int num40 = 0; num40 < StarCountToPlot; num40++)
			{
				float num20 = (float)num40 * num;
				num19 = (float)height - (AOTA_ZeroHeight + StarToPlot[num40] * AOTA_VerticalScale);
				if (PlotForm.chkStarPoints.get_Checked())
				{
					if (StarValidToPlot[num40])
					{
						graphics.FillEllipse(darkBlue, num20 - num39, num19 - num39, 2.5f * num39, 2.5f * num39);
					}
					else
					{
						graphics.FillEllipse(fuchsia, num20 - num39, num19 - num39, 1.5f * num39, 1.5f * num39);
					}
				}
			}
			if (PlotForm.chkTarget.get_Checked())
			{
				float y;
				float x = (y = 0f);
				if (PlotRunningAverage == 0)
				{
					for (int num41 = 0; num41 < StarCountToPlot; num41++)
					{
						float num20 = (float)num41 * num;
						num19 = (float)height - (AOTA_ZeroHeight + StarToPlot[num41] * AOTA_VerticalScale * num2);
						if (num41 > 0)
						{
							if (StarValidToPlot[num41 - 1] & StarValidToPlot[num41])
							{
								graphics.DrawLine(pen12, x, y, num20, num19);
							}
							else
							{
								graphics.DrawLine(pen13, x, y, num20, num19);
							}
						}
						x = num20;
						y = num19;
					}
				}
				else
				{
					CreateRunningAverageArray(ref StarToPlot, PlotRunningAverage);
					for (int num42 = 0; num42 < StarCountToPlot; num42++)
					{
						float num20 = (float)num42 * num;
						num19 = (float)height - (AOTA_ZeroHeight + RunningAverageValues[num42] * AOTA_VerticalScale * num2);
						if (num42 > 0)
						{
							graphics.DrawLine(pen12, x, y, num20, num19);
						}
						x = num20;
						y = num19;
					}
				}
			}
			if (PlotForm.chkShowBackground.get_Checked() & (Background != 0f))
			{
				num2 = 1f;
				if (PlotForm.chkScaleComparisons.get_Checked())
				{
					num2 = StarLevel / Background / 2f;
				}
				float y;
				float x = (y = 0f);
				if (((ListControl)PlotForm.cmbPlotAverage).get_SelectedIndex() == 0)
				{
					for (int num43 = 0; num43 < StarCountToPlot; num43++)
					{
						float num20 = (float)num43 * num;
						num19 = (float)height - (AOTA_ZeroHeight + StarBackground[num43] * AOTA_VerticalScale * num2);
						if (num43 > 0)
						{
							graphics.DrawLine(pen8, x, y, num20, num19);
						}
						x = num20;
						y = num19;
					}
				}
				else
				{
					CreateRunningAverageArray(ref StarBackground, PlotRunningAverage);
					for (int num44 = 0; num44 < StarCountToPlot; num44++)
					{
						float num20 = (float)num44 * num;
						num19 = (float)height - (AOTA_ZeroHeight + RunningAverageValues[num44] * AOTA_VerticalScale * num2);
						if (num44 > 0)
						{
							graphics.DrawLine(pen8, x, y, num20, num19);
						}
						x = num20;
						y = num19;
					}
				}
			}
			if (PlotForm.chkComp1.get_Checked())
			{
				num2 = 1f;
				num3 = 1f;
				if (PlotForm.chkScaleComparisons.get_Checked() & ((double)Comp1Level > 0.8 * (double)StarLevel))
				{
					num2 = StarLevel / Comp1Level * 1f;
					num3 = 0.8f;
				}
				float y;
				float x = (y = 0f);
				if (((ListControl)PlotForm.cmbPlotAverage).get_SelectedIndex() == 0)
				{
					flag = true;
					for (int num45 = 0; num45 < StarCountToPlot; num45++)
					{
						float num20 = (float)num45 * num;
						num19 = (float)height - (AOTA_ZeroHeight - 0.05f * (float)height + Comp1ToPlot[num45] * AOTA_VerticalScale * num2 * num3);
						if (!flag & (Comp1ToPlot[num45] != 0f))
						{
							graphics.DrawLine(pen9, x, y, num20, num19);
						}
						if (PlotForm.chkStarPoints.get_Checked())
						{
							graphics.FillEllipse(mediumSeaGreen, num20 - num39, num19 - num39, 2.5f * num39, 2.5f * num39);
						}
						x = num20;
						y = num19;
						flag = Comp1ToPlot[num45] == 0f;
					}
				}
				else
				{
					CreateRunningAverageArray(ref Comp1ToPlot, PlotRunningAverage);
					for (int num46 = 0; num46 < StarCountToPlot; num46++)
					{
						float num20 = (float)num46 * num;
						num19 = (float)height - (AOTA_ZeroHeight - 0.05f * (float)height + RunningAverageValues[num46] * AOTA_VerticalScale * num2 * num3);
						if (num46 > 0)
						{
							graphics.DrawLine(pen9, x, y, num20, num19);
						}
						x = num20;
						y = num19;
					}
				}
			}
			if (PlotForm.chkComp2.get_Checked())
			{
				num2 = 1f;
				num3 = 1f;
				if (PlotForm.chkScaleComparisons.get_Checked() & ((double)Comp2Level > 0.8 * (double)StarLevel))
				{
					num2 = StarLevel / Comp2Level * 1f;
					num3 = 0.75f;
				}
				float y;
				float x = (y = 0f);
				if (((ListControl)PlotForm.cmbPlotAverage).get_SelectedIndex() == 0)
				{
					flag = true;
					for (int num47 = 0; num47 < StarCountToPlot; num47++)
					{
						float num20 = (float)num47 * num;
						num19 = (float)height - (AOTA_ZeroHeight - 0.1f * (float)height + Comp2ToPlot[num47] * AOTA_VerticalScale * num2 * num3);
						if (!flag & (Comp2ToPlot[num47] != 0f))
						{
							graphics.DrawLine(pen10, x, y, num20, num19);
						}
						if (PlotForm.chkStarPoints.get_Checked())
						{
							graphics.FillEllipse(darkOrange, num20 - num39, num19 - num39, 2.5f * num39, 2.5f * num39);
						}
						x = num20;
						y = num19;
						flag = Comp2ToPlot[num47] == 0f;
					}
				}
				else
				{
					CreateRunningAverageArray(ref Comp2ToPlot, PlotRunningAverage);
					for (int num48 = 0; num48 < StarCountToPlot; num48++)
					{
						float num20 = (float)num48 * num;
						num19 = (float)height - (AOTA_ZeroHeight - 0.1f * (float)height + RunningAverageValues[num48] * AOTA_VerticalScale * num2 * num3);
						if (num48 > 0)
						{
							graphics.DrawLine(pen10, x, y, num20, num19);
						}
						x = num20;
						y = num19;
					}
				}
			}
			if (PlotForm.chkComp3.get_Checked())
			{
				num2 = 1f;
				num3 = 1f;
				if (PlotForm.chkScaleComparisons.get_Checked() & ((double)Comp3Level > 0.8 * (double)StarLevel))
				{
					num2 = StarLevel / Comp3Level * 1f;
					num3 = 0.7f;
				}
				float y;
				float x = (y = 0f);
				if (((ListControl)PlotForm.cmbPlotAverage).get_SelectedIndex() == 0)
				{
					flag = true;
					for (int num49 = 0; num49 < StarCountToPlot; num49++)
					{
						float num20 = (float)num49 * num;
						num19 = (float)height - (AOTA_ZeroHeight - 0.15f * (float)height + Comp3ToPlot[num49] * AOTA_VerticalScale * num2 * num3);
						if (!flag & (Comp3ToPlot[num49] != 0f))
						{
							graphics.DrawLine(pen11, x, y, num20, num19);
						}
						if (PlotForm.chkStarPoints.get_Checked())
						{
							graphics.FillEllipse(mediumVioletRed, num20 - num39, num19 - num39, 2.5f * num39, 2.5f * num39);
						}
						x = num20;
						y = num19;
						flag = Comp3ToPlot[num49] == 0f;
					}
				}
				else
				{
					CreateRunningAverageArray(ref Comp3ToPlot, PlotRunningAverage);
					for (int num50 = 0; num50 < StarCountToPlot; num50++)
					{
						float num20 = (float)num50 * num;
						num19 = (float)height - (AOTA_ZeroHeight - 0.15f * (float)height + RunningAverageValues[num50] * AOTA_VerticalScale * num2 * num3);
						if (num50 > 0)
						{
							graphics.DrawLine(pen11, x, y, num20, num19);
						}
						x = num20;
						y = num19;
					}
				}
			}
			PlotForm.picPlot.set_Image((Image)image);
			graphics.Dispose();
			((Control)PlotForm).Focus();
			try
			{
				Application.DoEvents();
			}
			catch (Exception ex)
			{
				MessageBox.Show("DoEvents in Plot()\r\n" + ex.ToString());
			}
			NumericUpDown updnMagStar = PlotForm.updnMagStar;
			NumericUpDown updnMagObject = PlotForm.updnMagObject;
			bool flag3;
			((Control)PlotForm.updnMagDrop).set_Enabled(flag3 = true);
			bool enabled;
			((Control)updnMagObject).set_Enabled(enabled = flag3);
			((Control)updnMagStar).set_Enabled(enabled);
			IsPlotting = false;
		}

		private static bool InValidPlotValues(float x, float y, float oldx, float oldy)
		{
			return false | (float.IsNaN(x) | float.IsInfinity(x)) | (float.IsNaN(y) | float.IsInfinity(y)) | (float.IsNaN(oldx) | float.IsInfinity(oldx)) | (float.IsNaN(oldy) | float.IsInfinity(oldy));
		}

		internal static bool GetMean_SD(int start, int end, out double Mean, out double SD, out double Variance)
		{
			Mean = (SD = (Variance = 0.0));
			if (end <= start)
			{
				return false;
			}
			int num = 0;
			for (int i = start; i <= end; i++)
			{
				if (!((i < 0) | (i >= StarCountToPlot)) && StarValidToPlot[i])
				{
					Mean += StarToPlot[i];
					num++;
				}
			}
			Mean /= num;
			for (int j = start; j <= end; j++)
			{
				if (!((j < 0) | (j >= StarCountToPlot)) && StarValidToPlot[j])
				{
					SD += ((double)StarToPlot[j] - Mean) * ((double)StarToPlot[j] - Mean);
				}
			}
			Variance = Math.Abs(Mean - SD / (double)num);
			SD = Math.Sqrt(Variance);
			if (num < 2)
			{
				return false;
			}
			return true;
		}

		internal static bool GetUpperLowerMeans(double BoundaryValue, out double UpperMean, out double LowerMean)
		{
			int num = 0;
			int num2 = 0;
			UpperMean = (LowerMean = 0.0);
			for (int i = 0; i < StarCountToPlot; i++)
			{
				if (StarValidToPlot[i])
				{
					if ((double)StarToPlot[i] > BoundaryValue)
					{
						UpperMean += StarToPlot[i];
						num++;
					}
					else
					{
						LowerMean += StarToPlot[i];
						num2++;
					}
				}
			}
			if (num < 2 || num2 < 2)
			{
				return false;
			}
			UpperMean /= num;
			LowerMean /= num2;
			return true;
		}

		internal static void Scale_SD(double SDTop, double SDBottom, double Top, double Bottom, double Height, out float yTop, out float yBottom)
		{
			double num = (SDTop - SDBottom) / (Top - Bottom);
			double num2 = SDBottom + (Height - Bottom) * num;
			double num3 = SDBottom + (Height + num2 - Bottom) * num;
			double num4 = SDBottom + (Height - num2 - Bottom) * num;
			yTop = (float)(Height + num3);
			yBottom = (float)(Height - num4);
		}

		internal static void FindPossibleOccultations(double FullLight, double OccLight, int MaxDurnToCheck)
		{
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_0206: Invalid comparison between Unknown and I4
			//IL_0222: Unknown result type (might be due to invalid IL or missing references)
			//IL_0228: Invalid comparison between Unknown and I4
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			int num4 = 0;
			int num5 = 0;
			int num6 = MaxDurnToCheck;
			if (num6 > (int)((double)StarCountToPlot / 1.2))
			{
				num6 = (int)((double)StarCountToPlot / 1.2);
			}
			double num7 = 1.0;
			double num8 = 1.0;
			double num9 = 1.0;
			double num10 = 0.0;
			double num11 = 0.0;
			double num12 = 0.0;
			double num13 = 0.0;
			FOMforNoOccn = 0.0;
			CancelFind = false;
			((Control)PlotForm.cmdCancelFind).set_Visible(true);
			for (int i = 0; i < CrossCorrR.GetUpperBound(0); i++)
			{
				CrossCorrR[i] = (CrossCorrWidth[i] = -1);
			}
			for (int j = 0; j < 5; j++)
			{
				LastMaxima[j] = 0.0;
				LastPos[j] = 0;
				LastWidth[j] = 0;
			}
			LastMaximaCount = 0;
			CheckForPrimaryEventFound = true;
			for (int k = 1; k < StarCountToPlot; k++)
			{
				num += CrossStar[k] * CrossStar[k];
			}
			((Control)PlotForm.pBar).set_Visible(true);
			PlotForm.pBar.set_Minimum(0);
			PlotForm.pBar.set_Maximum(num6);
			((Control)PlotForm.lblCurrentWidth).set_Visible(true);
			for (num4 = 1; num4 < num6; num4++)
			{
				PlotForm.pBar.set_Value(num4);
				((Control)PlotForm.lblCurrentWidth).set_Text("Width = " + num4 + " frames");
				Application.DoEvents();
				if (CancelFind)
				{
					break;
				}
				if (num4 % 50 == 0)
				{
					if ((int)MessageBox.Show("Do you want to continue the search?", "Continue search?", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 7)
					{
						break;
					}
					if ((int)MessageBox.Show("Do you want to skip the next 50 steps in the width?", "Increment width?", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
					{
						num4 += 50;
					}
				}
				num7 = ((double)(StarCountToPlot - num4) * FullLight + (double)num4 * OccLight) / (double)StarCountToPlot;
				num8 = FullLight - num7;
				double num14 = num8 * num8;
				num9 = OccLight - num7;
				num10 = num9 * num9;
				num2 = num14 * (double)(StarCountToPlot - num4) + num10 * (double)num4;
				for (int l = 0; l < StarCountToPlot; l++)
				{
					num3 = 0.0;
					num5 = l - num4 / 2;
					int num15 = num5 + num4;
					for (int m = 0; m < StarCountToPlot; m++)
					{
						if (StarValidToPlot[m])
						{
							num3 = ((!(m < num5 || m > num15)) ? (num3 + num9 * CrossStar[m]) : (num3 + num8 * CrossStar[m]));
						}
					}
					double num16 = num3 / Math.Sqrt(num2) / Math.Sqrt(num);
					if (num4 == 1)
					{
						if (!(num16 > FOMforNoOccn))
						{
							continue;
						}
						if (num16 > num11)
						{
							FOMforNoOccn = num11;
							if (num16 > num12)
							{
								num11 = num12;
								if (num16 > num13)
								{
									num12 = num13;
									num13 = num16;
								}
								else
								{
									num12 = num16;
								}
							}
							else
							{
								num11 = num16;
							}
						}
						else
						{
							FOMforNoOccn = num16;
						}
					}
					else if (num16 > CrossCorrR[l])
					{
						CrossCorrR[l] = num16;
						CrossCorrWidth[l] = num4;
					}
				}
				CrossCorrDone = true;
				if (num4 > 1)
				{
					if (FindPutativeEvents())
					{
						break;
					}
					if (num4 % 10 == 0)
					{
						int num17 = (int)((double)MaxValues[0].Pos * PlotForm.OldScaleValue - (double)((float)((Control)PlotForm).get_Width() / 2f));
						if (num17 < 0)
						{
							num17 = 0;
						}
						if (num17 >= ((ScrollProperties)((ScrollableControl)PlotForm.panelPlot).get_HorizontalScroll()).get_Maximum())
						{
							num17 = ((ScrollProperties)((ScrollableControl)PlotForm.panelPlot).get_HorizontalScroll()).get_Maximum();
						}
						((ScrollProperties)((ScrollableControl)PlotForm.panelPlot).get_HorizontalScroll()).set_Value(num17);
					}
				}
				PlotAOTA();
			}
			((Control)PlotForm.pBar).set_Visible(false);
			((Control)PlotForm.lblCurrentWidth).set_Visible(false);
			((Control)PlotForm.cmdCancelFind).set_Visible(false);
			for (int n = 0; n < 5; n++)
			{
				PlotForm.chkSetAsMiss[n].set_Checked(!PlotForm.chkDisplayEvent[n].get_Checked());
			}
		}

		private static bool FindPutativeEvents()
		{
			//IL_04c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ce: Invalid comparison between Unknown and I4
			//IL_04f3: Unknown result type (might be due to invalid IL or missing references)
			int num = 0;
			MaxValues.Clear();
			Maxima.SortToLower = true;
			for (int i = 0; i < 5; i++)
			{
				Maxima item = new Maxima();
				MaxValues.Add(item);
			}
			for (int j = 0; j < StarCountToPlot; j++)
			{
				if (CrossCorrR[j] > MaxValues[0].FOM)
				{
					MaxValues[0].FOM = CrossCorrR[j];
					MaxValues[0].Pos = j;
					MaxValues[0].Width = CrossCorrWidth[j];
				}
			}
			if (CheckForPrimaryEventFound)
			{
				LastMaxima[LastMaximaCount] = MaxValues[0].FOM;
				LastPos[LastMaximaCount] = MaxValues[0].Pos;
				LastWidth[LastMaximaCount] = MaxValues[0].Width;
				LastMaximaCount++;
			}
			MaxValues.Sort();
			for (int k = 2; k < StarCountToPlot - 2; k++)
			{
				if (!(CrossCorrR[k] > 0.8 * FOMforNoOccn) || !((CrossCorrR[k] > CrossCorrR[k - 2]) & (CrossCorrR[k] > CrossCorrR[k + 2]) & (CrossCorrR[k] > MaxValues[0].FOM)))
				{
					continue;
				}
				bool flag = false;
				for (int l = 0; l < 5; l++)
				{
					if ((double)Math.Abs(k - MaxValues[l].Pos) < 0.9 * (double)(MaxValues[l].Width + CrossCorrWidth[k]))
					{
						flag = true;
					}
				}
				if (!flag & (CrossCorrR[k] > MaxValues[0].FOM))
				{
					MaxValues[0].FOM = CrossCorrR[k];
					MaxValues[0].Pos = k;
					MaxValues[0].Width = CrossCorrWidth[k];
					MaxValues.Sort();
				}
			}
			Maxima.SortToLower = false;
			MaxValues.Sort();
			num = 0;
			for (int m = 0; m < 5; m++)
			{
				if (MaxValues[m].Pos == -1)
				{
					TextBox obj = PlotForm.txtPositions[m];
					string text;
					((Control)PlotForm.txtWidths[m]).set_Text(text = "-1");
					((Control)obj).set_Text(text);
					PlotForm.chkDisplayEvent[m].set_Checked(false);
				}
				else
				{
					((Control)PlotForm.txtPositions[m]).set_Text(FrameID[NumberOfFramesBinned * MaxValues[m].Pos + FirstFrameUsedInAnalysis].ToString());
					((Control)PlotForm.txtWidths[m]).set_Text((NumberOfFramesBinned * MaxValues[m].Width).ToString());
					PlotForm.chkDisplayEvent[m].set_Checked(true);
					num++;
				}
			}
			if ((LastMaximaCount > 4) & CheckForPrimaryEventFound)
			{
				if ((LastMaxima[4] == LastMaxima[0]) & (LastMaxima[4] > FOMforNoOccn))
				{
					if (MaxValues[1].Width > 0)
					{
						AutoProcess = false;
					}
					if (AutoProcess)
					{
						return true;
					}
					string text2 = "very good";
					if (LastMaxima[4] < 0.8)
					{
						text2 = "good";
					}
					if (LastMaxima[4] < 0.6)
					{
						text2 = "average";
					}
					if (LastMaxima[4] < 0.4)
					{
						text2 = "poor";
					}
					if (LastMaxima[4] < 2.0 * FOMforNoOccn)
					{
						text2 = "doubtful";
					}
					if ((int)MessageBox.Show("The primary occultation may have been found.\r\n\r\nThe possible event is at frame ID" + string.Format(" {0,1:f0}", FrameID[NumberOfFramesBinned * LastPos[4] + FirstFrameUsedInAnalysis]) + " , with a width of " + string.Format("{0,1:f0} frames", NumberOfFramesBinned * LastWidth[4]) + "\r\n\r\nThe cross-correlation coefficient is " + string.Format(" {0,1:f3}", LastMaxima[4]) + ", which is " + text2 + "\r\n\r\nDo you want to stop the search?", "Event found", (MessageBoxButtons)4, (MessageBoxIcon)64, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
					{
						if (num > 1)
						{
							AutoProcess = false;
							MessageBox.Show(num + " possible events have been found.\r\n\r\nFurther processing requires manual selection of the events to analyse", "Plural possibilities", (MessageBoxButtons)0, (MessageBoxIcon)64);
						}
						return true;
					}
					CheckForPrimaryEventFound = false;
				}
				for (int n = 0; n < 4; n++)
				{
					LastMaxima[n] = LastMaxima[n + 1];
					LastPos[n] = LastPos[n + 1];
					LastWidth[n] = LastWidth[n + 1];
				}
				LastMaximaCount = 4;
			}
			return false;
		}

		internal static void EvaluateSelectedEvent(int SelectedEvent)
		{
			int num = 0;
			CurrentlySelectedEvent = SelectedEvent;
			EventID = (SelectedEvent + 1).ToString();
			EventIDnumber = SelectedEvent;
			CurrentSolution = SelectedEvent;
			((Control)PlotForm.lblEventIsMiss).set_Visible(MissEvent[CurrentSolution]);
			((Control)PlotForm.tabPage6).set_Text("5. Analyse event #" + EventID);
			((Control)PlotForm.tabPage7).set_Text("6. Camera corrections && final times Event #" + EventID);
			((Control)PlotForm.grpFinalResults).set_Text("Final results  **  Event #" + EventID);
			((Control)PlotForm.cmdSaveResults).set_Text("Save form at Tab 5\r\n for Event # " + EventID);
			((Control)PlotForm.cmdSetMiss).set_Text("Set Event # " + EventID + "\r\n as a non-event");
			int num2 = 0;
			for (int i = 0; i < 5; i++)
			{
				if (((Control)PlotForm.cmdEvaluate[i]).get_Enabled())
				{
					num2++;
				}
			}
			((Control)PlotForm.lblEventsToAnalyse).set_Text(num2.ToString());
			X1 = MaxValues[SelectedEvent].Pos - MaxValues[SelectedEvent].Width / 2 - 30;
			if (X1 < 2)
			{
				X1 = 2;
			}
			X2 = MaxValues[SelectedEvent].Pos - MaxValues[SelectedEvent].Width / 2 + 20;
			if (X2 >= StarCountToPlot)
			{
				X2 = StarCountToPlot - 1;
			}
			X5 = MaxValues[SelectedEvent].Pos + MaxValues[SelectedEvent].Width / 2 - 20;
			if (X5 < 1)
			{
				X5 = 1;
			}
			X6 = MaxValues[SelectedEvent].Pos + MaxValues[SelectedEvent].Width / 2 + 30;
			if (X6 > StarCountToPlot - 2)
			{
				X6 = StarCountToPlot - 2;
			}
			if (X2 > X5)
			{
				X2 = (X5 = (X2 + X5) / 2);
			}
			X0 = X1 - ExtraPointsUsedInSolution;
			if (X0 < 1)
			{
				X0 = 1;
			}
			X3 = X2 + ExtraPointsUsedInSolution;
			X4 = X5 - ExtraPointsUsedInSolution;
			X7 = X6 + ExtraPointsUsedInSolution;
			if (X7 >= StarCountToPlot)
			{
				X7 = StarCountToPlot - 1;
			}
			((Control)PlotForm.txtBeforeD).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[X1]));
			((Control)PlotForm.txtAfterD).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[X2]));
			((Control)PlotForm.txtBeforeR).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[X5]));
			((Control)PlotForm.txtAfterR).set_Text(string.Format("{0,1:f1}", FrameIDofPlot[X6]));
			PlotAOTA();
			FirstTimeAnalysed = true;
			PlotForm.AnalyseForDandR(0, 0);
			decimal num3 = (decimal)(0.9 * (double)((Control)PlotForm).get_Width() / (double)(X6 - X1));
			if (num3 > 15m)
			{
				num3 = 15m;
			}
			PlotForm.updnScale.set_Value(num3);
			num = (int)((1.5 * (double)X1 - 0.5 * (double)X2) * (double)(float)PlotForm.updnScale.get_Value());
			if (num > 32000)
			{
				num = 31000;
			}
			if (num > ((ScrollProperties)((ScrollableControl)PlotForm.panelPlot).get_HorizontalScroll()).get_Maximum())
			{
				num = ((ScrollProperties)((ScrollableControl)PlotForm.panelPlot).get_HorizontalScroll()).get_Maximum();
			}
			if (num < 0)
			{
				num = 0;
			}
			HScrollProperties horizontalScroll = ((ScrollableControl)PlotForm.panelPlot).get_HorizontalScroll();
			int value;
			((ScrollProperties)((ScrollableControl)PlotForm.panelPlot).get_HorizontalScroll()).set_Value(value = num);
			((ScrollProperties)horizontalScroll).set_Value(value);
			PlotAOTA();
			PlotForm.tabPlot.set_SelectedIndex(6);
			PlotForm.SetDelayTimes();
		}

		internal static void PlotChi2()
		{
			if (PlotForm == null)
			{
				return;
			}
			int num = 1;
			int num2 = 1;
			float num3 = 1f;
			float num4 = 0f;
			float num5 = 0f;
			string text = "";
			int num6 = 1;
			if (PlotForm.updnTransitionNumber.get_Value() > 9m)
			{
				num6 = 2;
			}
			if (PlotForm.updnTransitionNumber.get_Value() > 19m)
			{
				num6 = 3;
			}
			ChiPlot_HorizontalScale = (float)((Control)PlotForm.picChiD).get_Width() / (float)(MaximimumTransitions + 2);
			for (int i = 0; i < 2; i++)
			{
				if (i == 0)
				{
					num = ((Control)PlotForm.picChiD).get_Width();
					num2 = ((Control)PlotForm.picChiD).get_Height();
				}
				else
				{
					num = ((Control)PlotForm.picChiR).get_Width();
					num2 = ((Control)PlotForm.picChiR).get_Height();
				}
				num3 = (float)num2 / 2f;
				Bitmap image = new Bitmap(num, num2);
				Graphics graphics = Graphics.FromImage(image);
				graphics.Clear(Color.White);
				Pen pen = new Pen(Color.Black);
				Pen pen2 = new Pen(Color.Green, 1f);
				Brush darkGreen = Brushes.DarkGreen;
				Brush orange = Brushes.Orange;
				Font font = new Font("Arial", 8f, FontStyle.Bold);
				Brush black = Brushes.Black;
				if (i == 0)
				{
					if (Chi2D_Selected == -1)
					{
						graphics.FillRectangle(orange, 0f, 0f, ChiPlot_HorizontalScale, num2);
					}
				}
				else if (Chi2R_Selected == -1)
				{
					graphics.FillRectangle(orange, 0f, 0f, ChiPlot_HorizontalScale, num2);
				}
				graphics.DrawString("A", font, black, 0f, num2 - 38);
				graphics.DrawString("L", font, black, 0f, num2 - 26);
				graphics.DrawString("L", font, black, 0f, num2 - 14);
				double num7 = 1000.0;
				for (int j = 0; j < MaximimumTransitions + 1; j++)
				{
					if (i == 0)
					{
						if (Chi2.Chi2_D_Minimums[j] < num7)
						{
							num7 = Chi2.Chi2_D_Minimums[j];
						}
					}
					else if (Chi2.Chi2_R_Minimums[j] < num7)
					{
						num7 = Chi2.Chi2_R_Minimums[j];
					}
				}
				for (int k = 0; k < MaximimumTransitions + 1; k++)
				{
					num4 = ChiPlot_HorizontalScale * (float)(1 + k);
					num5 = ((i != 0) ? ((float)((double)num3 * (Chi2.Chi2_R_Minimums[k] / num7 - 0.9))) : ((float)((double)num3 * (Chi2.Chi2_D_Minimums[k] / num7 - 0.9))));
					if (!float.IsNaN(num5))
					{
						if (i == 0)
						{
							if ((Chi2D_Selected >= 0) & (k == Chi2D_Selected))
							{
								graphics.FillRectangle(darkGreen, num4, num5, ChiPlot_HorizontalScale, (float)num2 - num5);
							}
							else
							{
								graphics.DrawRectangle(pen2, num4, num5, ChiPlot_HorizontalScale, (float)num2 - num5);
							}
						}
						else if ((Chi2R_Selected >= 0) & (k == Chi2R_Selected))
						{
							graphics.FillRectangle(darkGreen, num4, num5, ChiPlot_HorizontalScale, (float)num2 - num5);
						}
						else
						{
							graphics.DrawRectangle(pen2, num4, num5, ChiPlot_HorizontalScale, (float)num2 - num5);
						}
					}
					text = (k + 1).ToString();
					if (k % num6 == 0)
					{
						graphics.DrawString(text, font, black, num4 + ChiPlot_HorizontalScale / 2f - graphics.MeasureString(text, font).Width / 2f + 1f, num2 - 14);
					}
					graphics.DrawRectangle(pen, 0, 0, num - 1, num2 - 1);
				}
				if (i == 0)
				{
					PlotForm.picChiD.set_Image((Image)image);
				}
				else
				{
					PlotForm.picChiR.set_Image((Image)image);
				}
				graphics.Dispose();
			}
		}

		internal static void PlotUncertainties()
		{
			if (PlotForm == null)
			{
				return;
			}
			int num = 1;
			int num2 = 1;
			float num3 = 1f;
			float num4 = 0f;
			float num5 = 0f;
			string text = "";
			float num6 = (float)((Control)PlotForm.picDUncert).get_Width() / 21f;
			float num7 = 0f;
			int num8 = 0;
			int num9 = 0;
			for (int i = 0; i < 2; i++)
			{
				if (i == 0)
				{
					num = ((Control)PlotForm.picDUncert).get_Width();
					num2 = ((Control)PlotForm.picDUncert).get_Height();
				}
				else
				{
					num = ((Control)PlotForm.picRUncert).get_Width();
					num2 = ((Control)PlotForm.picRUncert).get_Height();
				}
				num7 = 1.02f * (float)Chi2.MonteCarloTrials;
				int monteCarloTrials = Chi2.MonteCarloTrials;
				int num10 = 0;
				for (int j = 0; j <= 20; j++)
				{
					num10 = ((i != 0) ? (num10 + Chi2.Chi2_R_CMcounts[j]) : (num10 + Chi2.Chi2_D_CMcounts[j]));
					if ((double)num10 >= 0.05 * (double)monteCarloTrials)
					{
						num8 = j;
						MinusLimit[i] = (float)(j - 10) - 0.5f;
						break;
					}
				}
				num10 = 0;
				for (int num11 = 20; num11 >= 0; num11--)
				{
					num10 = ((i != 0) ? (num10 + Chi2.Chi2_R_CMcounts[num11]) : (num10 + Chi2.Chi2_D_CMcounts[num11]));
					if ((double)num10 >= 0.05 * (double)monteCarloTrials)
					{
						num9 = num11;
						PlusLimit[i] = (float)(num11 - 10) + 0.5f;
						break;
					}
				}
				num3 = (float)num2 / num7;
				Bitmap image = new Bitmap(num, num2);
				Graphics graphics = Graphics.FromImage(image);
				graphics.Clear(Color.White);
				Pen pen = new Pen(Color.Black);
				Pen pen2 = new Pen(Color.Green, 1f);
				_ = Brushes.DarkGreen;
				Brush crimson = Brushes.Crimson;
				Brush darkRed = Brushes.DarkRed;
				Font font = new Font("Microsoft Sans Serif", 7f, FontStyle.Bold);
				Brush black = Brushes.Black;
				monteCarloTrials = 0;
				for (int k = 0; k < 21; k++)
				{
					num4 = num6 * (float)k;
					num5 = ((i != 0) ? (num3 * (float)Chi2.Chi2_R_CMcounts[k]) : (num3 * (float)Chi2.Chi2_D_CMcounts[k]));
					if (k == 0 || k == 20)
					{
						graphics.FillRectangle(crimson, num4, (float)num2 - num5, num6, num5);
					}
					else if (!float.IsNaN(num5) && (k < num8 || k > num9))
					{
						graphics.FillRectangle(darkRed, num4, (float)num2 - num5, num6, num5);
					}
					graphics.DrawRectangle(pen2, num4, (float)num2 - num5, num6, num5);
					text = (k - 10).ToString();
					if ((k - 10) % 3 == 0)
					{
						graphics.DrawString(text, font, black, num4 + num6 / 2f - graphics.MeasureString(text, font).Width / 2f + 1f, 2f);
					}
					graphics.DrawRectangle(pen, 0, 0, num - 1, num2 - 1);
				}
				if (i == 0)
				{
					PlotForm.picDUncert.set_Image((Image)image);
				}
				else
				{
					PlotForm.picRUncert.set_Image((Image)image);
				}
				graphics.Dispose();
			}
		}

		internal static void GetUncertaintyLimits()
		{
			int num = 0;
			int num2 = 0;
			for (int i = 0; i <= 20; i++)
			{
				num += Chi2.Chi2_D_CMcounts[i];
				num2 += Chi2.Chi2_R_CMcounts[i];
			}
			int num3 = (int)((1.0 - ConfidenceLevelD) / 2.0 * (double)num);
			int num4 = (int)((1.0 - ConfidenceLevelR) / 2.0 * (double)num2);
			num = 0;
			num2 = 0;
			for (int j = 0; j <= 10; j++)
			{
				num += Chi2.Chi2_D_CMcounts[j];
				if (num >= num3)
				{
					MinusLimit[0] = j - 10;
					if (num >= 3 * num3)
					{
						MinusLimit[0] -= 0.5f;
					}
					break;
				}
			}
			for (int k = 0; k <= 10; k++)
			{
				num2 += Chi2.Chi2_R_CMcounts[k];
				if (num2 >= num4)
				{
					MinusLimit[1] = k - 10;
					if (num2 >= 3 * num4)
					{
						MinusLimit[1] -= 0.5f;
					}
					break;
				}
			}
			num = 0;
			num2 = 0;
			for (int num5 = 20; num5 >= 10; num5--)
			{
				num += Chi2.Chi2_D_CMcounts[num5];
				if (num >= num3)
				{
					PlusLimit[0] = num5 - 10;
					if (num >= 3 * num3)
					{
						PlusLimit[0] += 0.5f;
					}
					break;
				}
			}
			for (int num6 = 20; num6 >= 10; num6--)
			{
				num2 += Chi2.Chi2_R_CMcounts[num6];
				if (num2 >= num4)
				{
					PlusLimit[1] = num6 - 10;
					if (num2 >= 3 * num4)
					{
						PlusLimit[1] += 0.5f;
					}
					break;
				}
			}
		}

		internal static bool SetTangraSaveRootFileName()
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_002b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_FileName(TangraSourceFile);
			((FileDialog)val).set_Title("Set File name for saving outputs");
			val.set_OverwritePrompt(true);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				TangraSourceFile = ((FileDialog)val).get_FileName();
				return true;
			}
			return false;
		}

		internal static bool All_Events_Are_Non_Events()
		{
			bool flag = true;
			for (int i = 0; i < 5; i++)
			{
				flag &= PlotForm.chkSetAsMiss[i].get_Checked();
			}
			return flag;
		}

		internal static string CreateTextReport()
		{
			StringBuilder stringBuilder = new StringBuilder();
			string text = "";
			if (!CameraCorrectionsAppliedByAOTA())
			{
				text = " NOT";
			}
			if (AOTA_ExternalAccess.RunFromTangra)
			{
				stringBuilder.AppendLine("AOTA analysis of the file " + TangraSourceFile);
			}
			else
			{
				stringBuilder.AppendLine("AOTA analysis of the file " + CurrentFileName);
			}
			stringBuilder.AppendLine("- analysed on " + DateTime.Now.ToUniversalTime().ToLongDateString() + " at " + DateTime.Now.ToUniversalTime().ToShortTimeString() + " UTC");
			stringBuilder.AppendLine("- using AOTA v" + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version?.ToString() + "\r\n");
			if (All_Events_Are_Non_Events())
			{
				stringBuilder.AppendLine("No valid events found. If the star has been correctly\r\nidentified, report the event as a Miss.");
				stringBuilder.AppendFormat("  Expected SN for a non-miss event: {0,1:f1}\r\n\r\n", ExpectedSN_Miss);
			}
			else
			{
				for (int i = 0; i < 5; i++)
				{
					if (MissEvent[i])
					{
						continue;
					}
					if (i > 0)
					{
						stringBuilder.AppendLine("=====\r\n");
					}
					stringBuilder.AppendLine("Event #" + (i + 1));
					stringBuilder.AppendLine("");
					if (EventAnalysed[i])
					{
						stringBuilder.AppendLine("  Event location in video frame ID's");
						stringBuilder.AppendFormat("   D: {0,1:f1}  +{1,2:f1}/{2,1:f1}\r\n", Results[i].D_Frame, Results[i].D_FrameUncertPlus, Results[i].D_FrameUncertMinus);
						stringBuilder.AppendFormat("   R: {0,1:f1}  +{1,2:f1}/{2,1:f1}\r\n", Results[i].R_Frame, Results[i].R_FrameUncertPlus, Results[i].R_FrameUncertMinus);
						stringBuilder.AppendLine("");
						stringBuilder.AppendLine("  Event time in UTC");
						stringBuilder.AppendLine("   D: " + Results[i].D_UTC);
						stringBuilder.AppendLine("   R: " + Results[i].R_UTC);
						stringBuilder.AppendLine("");
						stringBuilder.AppendLine("  Duration of transition in frames");
						stringBuilder.AppendLine("   D: " + Results[i].D_DurationFrames);
						stringBuilder.AppendLine("   R: " + Results[i].R_DurationFrames);
						if (CurrentlySelectedEvent >= 0)
						{
							stringBuilder.AppendLine("");
							stringBuilder.AppendLine("  SN at event locations");
							stringBuilder.AppendFormat("   D: {0,1:f1}\r\n", Results[CurrentlySelectedEvent].D_SN);
							stringBuilder.AppendFormat("   R: {0,1:f1}\r\n", Results[CurrentlySelectedEvent].R_SN);
							stringBuilder.AppendFormat("   Ave: {0,1:f1}\r\n", (double)(Results[CurrentlySelectedEvent].D_SN + Results[CurrentlySelectedEvent].R_SN) / 2.0);
							stringBuilder.AppendLine("");
							stringBuilder.AppendLine("  Applied confidence level");
							stringBuilder.AppendFormat("   D: {0:P0}\r\n", AppliedConfidenceD[CurrentlySelectedEvent]);
							stringBuilder.AppendFormat("   R: {0:P0}\r\n", AppliedConfidenceR[CurrentlySelectedEvent]);
							if ((InvalidNearD[CurrentlySelectedEvent] > 0) | (InvalidNearR[CurrentlySelectedEvent] > 0))
							{
								stringBuilder.AppendLine("");
								stringBuilder.AppendLine("  Data points ignored within 10 points from the event");
								if (InvalidNearD[CurrentlySelectedEvent] > 0)
								{
									stringBuilder.AppendFormat("   D: {0:f0}\r\n", InvalidNearD[CurrentlySelectedEvent]);
								}
								if (InvalidNearR[CurrentlySelectedEvent] > 0)
								{
									stringBuilder.AppendFormat("   R: {0:f0}\r\n", InvalidNearR[CurrentlySelectedEvent]);
								}
							}
						}
					}
					else
					{
						stringBuilder.AppendLine("  Potential event not analysed");
						stringBuilder.AppendLine("    located at frame " + ((Control)PlotForm.txtPositions[i]).get_Text() + ", with width " + ((Control)PlotForm.txtWidths[i]).get_Text());
						try
						{
							if (MaxValues[i].Pos > 0)
							{
								stringBuilder.AppendLine("    Cross-correlation value = " + string.Format("{0,1:f3}", CrossCorrR[MaxValues[i].Pos]));
							}
						}
						catch
						{
						}
					}
					stringBuilder.AppendLine("");
				}
			}
			stringBuilder.AppendLine("===== Camera details =====");
			stringBuilder.AppendLine("Camera                 : " + CameraSpecs.CameraType);
			stringBuilder.AppendLine("Frames integrated      : " + CameraSpecs.FramesIntegrated);
			stringBuilder.AppendLine("Video system           : " + CameraSpecs.VideoSystem);
			stringBuilder.AppendLine("Camera delays have" + text + " been applied by AOTA to the times");
			stringBuilder.AppendLine("");
			stringBuilder.AppendLine("==== Measurement info ====");
			stringBuilder.AppendLine("Measurement tool       : " + CameraSpecs.MeasuringTool);
			stringBuilder.AppendLine("Measured at Field level: " + CameraSpecs.MeasuredAtFieldLevel);
			stringBuilder.AppendLine("# Measurements binned  : " + CameraSpecs.MeasurementsBinned);
			if (NormalisationStarRef == 0)
			{
				stringBuilder.AppendLine("Normalisation          : None");
			}
			else
			{
				stringBuilder.AppendLine(string.Format("Normalisation          : Comp-star #{0,1:f0}, {1,1:f0}-point running average", NormalisationStarRef, RunningAverage));
			}
			stringBuilder.AppendLine("Time scale from tool   : " + CameraSpecs.TimeScaleFromMeasuringTool);
			stringBuilder.AppendLine("");
			stringBuilder.AppendLine("===== end of report ======");
			return stringBuilder.ToString();
		}

		internal static bool CameraCorrectionsAppliedByAOTA()
		{
			CameraSpecs.CameraDelaysKnownToAOTA = true;
			if (CameraCorrectionsHaveBeenApplied)
			{
				CameraSpecs.CameraDelaysKnownToAOTA = false;
			}
			else
			{
				CameraSpecs.CameraDelaysKnownToAOTA = !CameraSpecs.CameraType.ToLower().Contains("unknown");
			}
			return CameraSpecs.CameraDelaysKnownToAOTA;
		}

		internal static DialogResult VerifyBeforeExit(bool Exit)
		{
			//IL_0151: Unknown result type (might be due to invalid IL or missing references)
			StringBuilder stringBuilder = new StringBuilder();
			string text = "";
			if (!CameraCorrectionsAppliedByAOTA())
			{
				text = " NOT";
			}
			if (Exit)
			{
				if (AOTA_ExternalAccess.RunFromTangra)
				{
					stringBuilder.AppendLine("You are returning to Tangra. The following need to be confirmed before leaving AOTA");
				}
				else
				{
					stringBuilder.AppendLine("You are leaving AOTA.\r\n");
				}
			}
			else
			{
				stringBuilder.AppendLine("The Report will need to be resaved if any of the following are changed before leaving AOTA");
			}
			if (AOTA_ExternalAccess.RunFromTangra || !Exit)
			{
				stringBuilder.AppendLine("");
				stringBuilder.AppendLine("Camera                 : " + CameraSpecs.CameraType);
				stringBuilder.AppendLine("Video system           : " + CameraSpecs.VideoSystem);
				stringBuilder.AppendLine("Frames integrated      : " + CameraSpecs.FramesIntegrated);
				stringBuilder.AppendLine("# Measurements binned  : " + CameraSpecs.MeasurementsBinned);
				stringBuilder.AppendLine("");
				stringBuilder.AppendLine("Camera delays have" + text + " been applied by AOTA to the times");
				stringBuilder.AppendLine("");
			}
			if (Exit)
			{
				if (AOTA_ExternalAccess.RunFromTangra)
				{
					stringBuilder.AppendLine("Are these settings correct?");
				}
				else
				{
					stringBuilder.AppendLine("If you want to save a report or images (and haven't done so), press 'Cancel' to abort the Exit.");
				}
			}
			else
			{
				stringBuilder.AppendLine("Do you want to continue to Save the report?");
			}
			return MessageBox.Show(stringBuilder.ToString(), "Confirm settings before exit", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
		}

		internal static void FourierCheck()
		{
			if (StarCountToPlot < 10)
			{
				return;
			}
			double[] array = new double[4];
			uint num = (uint)Math.Ceiling(Math.Log(StarCountToPlot) / Math.Log(2.0));
			uint num2 = (uint)Math.Pow(2.0, num);
			FourierReal = new double[num2];
			FourierReal1 = new double[num2];
			FourierReal2 = new double[num2];
			FourierReal3 = new double[num2];
			FourierImaginary = new double[num2];
			FourierImaginary1 = new double[num2];
			FourierImaginary2 = new double[num2];
			FourierImaginary3 = new double[num2];
			for (int i = 0; i < num2; i++)
			{
				FourierImaginary[i] = (FourierImaginary1[i] = (FourierImaginary2[i] = (FourierImaginary3[i] = 0.0)));
			}
			for (int j = 0; j < StarCountToPlot; j++)
			{
				FourierReal[j] = StarToPlot[j] - StarLevel;
				array[0] += FourierReal[j];
				if (Comp1Used)
				{
					FourierReal1[j] = Comp1ToPlot[j] - Comp1Level;
					array[1] += FourierReal[j];
				}
				if (Comp2Used)
				{
					FourierReal2[j] = Comp2ToPlot[j] - Comp2Level;
					array[2] += FourierReal[j];
				}
				if (Comp3Used)
				{
					FourierReal3[j] = Comp3ToPlot[j] - Comp3Level;
					array[3] += FourierReal[j];
				}
			}
			for (int k = 0; k < 4; k++)
			{
				array[k] /= StarCountToPlot;
			}
			for (int l = StarCountToPlot; l < num2; l++)
			{
				FourierReal[l] = array[0];
				if (Comp1Used)
				{
					FourierReal1[l] = array[1];
				}
				if (Comp2Used)
				{
					FourierReal2[l] = array[2];
				}
				if (Comp3Used)
				{
					FourierReal3[l] = array[3];
				}
			}
			FFT2 fFT = new FFT2();
			fFT.init(num);
			fFT.run(FourierReal, FourierImaginary, inverse: false);
			if (Comp1Used)
			{
				fFT.init(num);
				fFT.run(FourierReal1, FourierImaginary1, inverse: false);
			}
			if (Comp2Used)
			{
				fFT.init(num);
				fFT.run(FourierReal2, FourierImaginary2, inverse: false);
			}
			if (Comp3Used)
			{
				fFT.init(num);
				fFT.run(FourierReal3, FourierImaginary3, inverse: false);
			}
			PlotFourier();
		}

		internal static void PlotFourier()
		{
			try
			{
				if (FourierReal.Length < 10)
				{
					return;
				}
			}
			catch
			{
				return;
			}
			int num = 20;
			int num2 = 30;
			Pen pen = new Pen(Color.Black, 1f);
			new Pen(Color.Blue);
			Brush darkBlue = Brushes.DarkBlue;
			Pen pen2 = new Pen(Color.FromArgb(130, Color.MediumSeaGreen));
			Pen pen3 = new Pen(Color.FromArgb(130, Color.DarkOrange));
			Pen pen4 = new Pen(Color.FromArgb(130, Color.MediumVioletRed));
			_ = Brushes.Orange;
			Font font = new Font("Courier New", 8f);
			Brush black = Brushes.Black;
			int width = ((Control)PlotForm.picFourier).get_Width();
			int height = ((Control)PlotForm.picFourier).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Color.White);
			uint num3 = (uint)Math.Ceiling(Math.Log(StarCountToPlot) / Math.Log(2.0));
			uint num4 = (uint)Math.Pow(2.0, num3);
			int num5 = (int)((double)num4 / 8.0);
			double[] array = new double[4];
			double num6 = (double)(width - 2 * num) / (double)num5;
			double[] array2 = new double[4];
			int num7 = 0;
			for (int i = 1; i < num5; i++)
			{
				array2[0] += Math.Sqrt(FourierReal[i] * FourierReal[i] + FourierImaginary[i] * FourierImaginary[i]);
				num7++;
				if (PlotForm.chkComp1.get_Checked())
				{
					array2[1] += Math.Sqrt(FourierReal1[i] * FourierReal1[i] + FourierImaginary1[i] * FourierImaginary1[i]);
				}
				if (PlotForm.chkComp2.get_Checked())
				{
					array2[2] += Math.Sqrt(FourierReal2[i] * FourierReal2[i] + FourierImaginary2[i] * FourierImaginary2[i]);
				}
				if (PlotForm.chkComp3.get_Checked())
				{
					array2[3] += Math.Sqrt(FourierReal3[i] * FourierReal3[i] + FourierImaginary3[i] * FourierImaginary3[i]);
				}
				if ((double)num4 / (double)i < 50.0 && i > 10)
				{
					break;
				}
			}
			for (int j = 0; j < 4; j++)
			{
				array2[j] /= num7;
			}
			array[0] = ((double)height - 1.1 * (double)num2) / array2[0] / 10.0;
			if (PlotForm.chkComp1.get_Checked())
			{
				array[1] = ((double)height - 1.1 * (double)num2) / array2[1] / 10.0;
			}
			if (PlotForm.chkComp2.get_Checked())
			{
				array[2] = ((double)height - 1.1 * (double)num2) / array2[2] / 10.0;
			}
			if (PlotForm.chkComp3.get_Checked())
			{
				array[3] = ((double)height - 1.1 * (double)num2) / array2[3] / 10.0;
			}
			for (int k = 0; k < num5; k++)
			{
				if (PlotForm.chkTarget.get_Checked() | PlotForm.chkStarPoints.get_Checked())
				{
					double num8 = Math.Sqrt(FourierReal[k] * FourierReal[k] + FourierImaginary[k] * FourierImaginary[k]);
					graphics.FillRectangle(darkBlue, (float)((double)(num + 2) + num6 * (double)k), (float)((double)(height - num2) - array[0] * num8), (float)num6, (float)(array[0] * num8));
				}
				if (PlotForm.chkComp1.get_Checked())
				{
					double num9 = Math.Sqrt(FourierReal1[k] * FourierReal1[k] + FourierImaginary1[k] * FourierImaginary1[k]);
					graphics.DrawRectangle(pen2, (float)((double)(num + 2) + num6 * (double)k), (float)((double)(height - num2) - array[1] * num9), (float)num6, (float)(array[1] * num9));
				}
				if (PlotForm.chkComp2.get_Checked())
				{
					double num10 = Math.Sqrt(FourierReal2[k] * FourierReal2[k] + FourierImaginary2[k] * FourierImaginary2[k]);
					graphics.DrawRectangle(pen3, (float)((double)(num + 2) + num6 * (double)k), (float)((double)(height - num2) - array[2] * num10), (float)num6, (float)(array[2] * num10));
				}
				if (PlotForm.chkComp3.get_Checked())
				{
					double num11 = Math.Sqrt(FourierReal3[k] * FourierReal3[k] + FourierImaginary3[k] * FourierImaginary3[k]);
					graphics.DrawRectangle(pen4, (float)((double)(num + 2) + num6 * (double)k), (float)((double)(height - num2) - array[3] * num11), (float)num6, (float)(array[3] * num11));
				}
			}
			float[] array3 = new float[22]
			{
				8f, 9f, 10f, 11f, 12f, 13.5f, 15f, 17.5f, 20f, 25f,
				30f, 35f, 40f, 45f, 50f, 60f, 70f, 85f, 100f, 150f,
				200f, 300f
			};
			string text;
			for (int l = 0; l < array3.Length; l++)
			{
				float num12 = (float)((double)num4 / (double)array3[l]);
				if (num6 * (double)num12 < 1.5 * (double)width)
				{
					if (l % 2 == 0)
					{
						graphics.DrawLine(pen, (float)((double)(num + 2) + num6 * (double)num12), height - num2, (float)((double)(num + 2) + num6 * (double)num12), height - num2 + 5);
						text = string.Format("{0,1:f0}", array3[l]);
						graphics.DrawString(text, font, black, (float)((double)(num + 2) + num6 * (double)num12 - (double)(graphics.MeasureString(text, font).Width / 2f)), height - num2 + 5);
					}
					else
					{
						graphics.DrawLine(pen, (float)((double)(num + 2) + num6 * (double)num12), height - num2, (float)((double)(num + 2) + num6 * (double)num12), (float)(height - num2) + 3f);
					}
				}
			}
			text = "Period - in displayed measurement intervals";
			graphics.DrawString(text, font, black, (float)(width / 2) - graphics.MeasureString(text, font).Width / 2f, height - num2 + 15);
			text = "Frequency analysis of " + CurrentFileName + "    Bin size = " + NumberOfFramesBinned;
			graphics.DrawString(text, font, black, (float)(width / 2) - graphics.MeasureString(text, font).Width / 2f, 5f);
			graphics.DrawLine(pen, num, 0, num, height - num2);
			graphics.DrawLine(pen, num, height - num2, width, height - num2);
			graphics.DrawRectangle(pen, 0, 0, width - 1, height - 1);
			PlotForm.picFourier.set_Image((Image)image);
			graphics.Dispose();
		}

		internal static void ShortEventsCheck(int RunningAverage, bool ForLightCurve)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double num8 = 0.0;
			double num9 = 0.0;
			double num10 = 0.0;
			double num11 = 0.0;
			double num12 = 0.0;
			int num13 = 0;
			double num14 = 0.0;
			double num15 = 0.0;
			double num16 = 0.0;
			double num17 = 0.0;
			double num18 = 2.0;
			double num19 = 1.25;
			int num20 = 0;
			int num21 = 0;
			bool flag = true;
			bool flag2 = true;
			bool flag3 = true;
			if (ForLightCurve)
			{
				try
				{
					((Control)LightData.LightCurveForm).Show();
				}
				catch
				{
					LightData.LightCurveForm = new LightCurve();
					((Control)LightData.LightCurveForm).Show();
				}
			}
			Sdevs sdevs = new Sdevs();
			List<Sdevs> list = new List<Sdevs>();
			bool flag4 = (flag = Comp1Used);
			bool flag5 = (flag2 = Comp2Used);
			bool flag6 = (flag3 = Comp3Used);
			CreateRunningAverageArray(ref StarToPlot, ref StarToPlot_FromAve, RunningAverage);
			if (flag)
			{
				CreateRunningAverageArray(ref Comp1ToPlot, ref Comp1ToPlot_FromAve, RunningAverage);
			}
			if (flag2)
			{
				CreateRunningAverageArray(ref Comp2ToPlot, ref Comp2ToPlot_FromAve, RunningAverage);
			}
			if (flag3)
			{
				CreateRunningAverageArray(ref Comp3ToPlot, ref Comp3ToPlot_FromAve, RunningAverage);
			}
			int starCountToPlot = StarCountToPlot;
			for (int i = 0; i < starCountToPlot; i++)
			{
				if (StarValidToPlot[i])
				{
					num += (double)StarToPlot_FromAve[i];
					num9 += (double)StarToPlot[i];
					if (flag)
					{
						num2 += (double)Comp1ToPlot_FromAve[i];
						num10 += (double)Comp1ToPlot[i];
					}
					if (flag2)
					{
						num3 += (double)Comp2ToPlot_FromAve[i];
						num11 += (double)Comp2ToPlot[i];
					}
					if (flag3)
					{
						num4 += (double)Comp3ToPlot_FromAve[i];
						num12 += (double)Comp3ToPlot[i];
					}
					num13++;
				}
			}
			if (num13 < 8)
			{
				return;
			}
			num /= (double)num13;
			num2 /= (double)num13;
			num3 /= (double)num13;
			num4 /= (double)num13;
			num9 /= (double)num13;
			num10 /= (double)num13;
			num11 /= (double)num13;
			num12 /= (double)num13;
			if (!ForLightCurve)
			{
				Label lblComp = PlotForm.lblComp1;
				ListBox lstComp = PlotForm.lstComp1;
				Label lblHead = PlotForm.lblHead1;
				bool flag7;
				((Control)PlotForm.optComp1).set_Visible(flag7 = flag);
				bool flag8;
				((Control)lblHead).set_Visible(flag8 = flag7);
				bool visible;
				((Control)lstComp).set_Visible(visible = flag8);
				((Control)lblComp).set_Visible(visible);
				Label lblComp2 = PlotForm.lblComp2;
				ListBox lstComp2 = PlotForm.lstComp2;
				Label lblHead2 = PlotForm.lblHead2;
				((Control)PlotForm.optComp2).set_Visible(flag7 = flag2);
				((Control)lblHead2).set_Visible(flag8 = flag7);
				((Control)lstComp2).set_Visible(visible = flag8);
				((Control)lblComp2).set_Visible(visible);
				Label lblComp3 = PlotForm.lblComp3;
				ListBox lstComp3 = PlotForm.lstComp3;
				Label lblHead3 = PlotForm.lblHead3;
				((Control)PlotForm.optComp3).set_Visible(flag7 = flag3);
				((Control)lblHead3).set_Visible(flag8 = flag7);
				((Control)lstComp3).set_Visible(visible = flag8);
				((Control)lblComp3).set_Visible(visible);
				((Control)PlotForm.lblNoCompStars).set_Visible(!flag);
				Label lblComp4 = PlotForm.lblComp1;
				((Control)PlotForm.lstComp1).set_Visible(visible = flag);
				((Control)lblComp4).set_Visible(visible);
				Label lblComp5 = PlotForm.lblComp1;
				Label lblComp6 = PlotForm.lblComp2;
				Color darkGreen;
				((Control)PlotForm.lblComp3).set_ForeColor(darkGreen = Color.DarkGreen);
				Color foreColor;
				((Control)lblComp6).set_ForeColor(foreColor = darkGreen);
				((Control)lblComp5).set_ForeColor(foreColor);
				ListBox lstComp4 = PlotForm.lstComp1;
				ListBox lstComp5 = PlotForm.lstComp2;
				((Control)PlotForm.lstComp3).set_BackColor(darkGreen = Color.FromArgb(230, 255, 230));
				((Control)lstComp5).set_BackColor(foreColor = darkGreen);
				((Control)lstComp4).set_BackColor(foreColor);
				((Control)PlotForm.lstTarget).set_BackColor(Color.FromArgb(255, 255, 220));
				if (flag)
				{
					flag4 = (num10 / num9 < 0.2) | (num10 / num9 > 5.0);
					if (PlotForm.optComp1.get_Checked())
					{
						flag4 = !flag4;
					}
					if (flag4)
					{
						((Control)PlotForm.lblComp1).set_Text(string.Format("Comp 1 = {0,3:f2}", num10 / num9));
						((Control)PlotForm.lblComp1).set_ForeColor(Color.DarkRed);
						((Control)PlotForm.lstComp1).set_BackColor(Color.MistyRose);
						((Control)PlotForm.optComp1).set_Text("Not Used");
					}
					else
					{
						((Control)PlotForm.lblComp1).set_Text(string.Format("Comp 1 = {0,3:f2}", num10 / num9));
						((Control)PlotForm.optComp1).set_Text("Used");
						num20++;
					}
				}
				if (flag2)
				{
					flag5 = (num11 / num9 < 0.2) | (num11 / num9 > 5.0);
					if (PlotForm.optComp2.get_Checked())
					{
						flag5 = !flag5;
					}
					if (flag5)
					{
						((Control)PlotForm.lblComp2).set_Text(string.Format("Comp 2 = {0,3:f2}", num11 / num9));
						((Control)PlotForm.lblComp2).set_ForeColor(Color.DarkRed);
						((Control)PlotForm.lstComp2).set_BackColor(Color.MistyRose);
						((Control)PlotForm.optComp2).set_Text("Not Used");
					}
					else
					{
						((Control)PlotForm.lblComp2).set_Text(string.Format("Comp 2 = {0,3:f2}", num11 / num9));
						((Control)PlotForm.optComp2).set_Text("Used");
						num20++;
					}
				}
				if (flag3)
				{
					flag6 = (num12 / num9 < 0.2) | (num12 / num9 > 5.0);
					if (PlotForm.optComp3.get_Checked())
					{
						flag6 = !flag6;
					}
					if (flag6)
					{
						((Control)PlotForm.lblComp3).set_Text(string.Format("Comp 3 = {0,3:f2}", num12 / num9));
						((Control)PlotForm.lblComp3).set_ForeColor(Color.DarkRed);
						((Control)PlotForm.lstComp3).set_BackColor(Color.MistyRose);
						((Control)PlotForm.optComp3).set_Text("Not Used");
					}
					else
					{
						((Control)PlotForm.lblComp3).set_Text(string.Format("Comp 3 = {0,3:f2}", num12 / num9));
						((Control)PlotForm.optComp3).set_Text("Used");
						num20++;
					}
				}
			}
			else
			{
				Label lblComp7 = LightData.LightCurveForm.lblComp1;
				ListBox lstComp6 = LightData.LightCurveForm.lstComp1;
				Label lblHead4 = LightData.LightCurveForm.lblHead1;
				bool flag7;
				((Control)LightData.LightCurveForm.optComp1).set_Visible(flag7 = flag);
				bool flag8;
				((Control)lblHead4).set_Visible(flag8 = flag7);
				bool visible;
				((Control)lstComp6).set_Visible(visible = flag8);
				((Control)lblComp7).set_Visible(visible);
				Label lblComp8 = LightData.LightCurveForm.lblComp2;
				ListBox lstComp7 = LightData.LightCurveForm.lstComp2;
				Label lblHead5 = LightData.LightCurveForm.lblHead2;
				((Control)LightData.LightCurveForm.optComp2).set_Visible(flag7 = flag2);
				((Control)lblHead5).set_Visible(flag8 = flag7);
				((Control)lstComp7).set_Visible(visible = flag8);
				((Control)lblComp8).set_Visible(visible);
				Label lblComp9 = LightData.LightCurveForm.lblComp3;
				ListBox lstComp8 = LightData.LightCurveForm.lstComp3;
				Label lblHead6 = LightData.LightCurveForm.lblHead3;
				((Control)LightData.LightCurveForm.optComp3).set_Visible(flag7 = flag3);
				((Control)lblHead6).set_Visible(flag8 = flag7);
				((Control)lstComp8).set_Visible(visible = flag8);
				((Control)lblComp9).set_Visible(visible);
				((Control)LightData.LightCurveForm.lblNoCompStars).set_Visible(!flag);
				Label lblComp10 = LightData.LightCurveForm.lblComp1;
				((Control)LightData.LightCurveForm.lstComp1).set_Visible(visible = flag);
				((Control)lblComp10).set_Visible(visible);
				Label lblComp11 = LightData.LightCurveForm.lblComp1;
				Label lblComp12 = LightData.LightCurveForm.lblComp2;
				Color darkGreen;
				((Control)LightData.LightCurveForm.lblComp3).set_ForeColor(darkGreen = Color.DarkGreen);
				Color foreColor;
				((Control)lblComp12).set_ForeColor(foreColor = darkGreen);
				((Control)lblComp11).set_ForeColor(foreColor);
				ListBox lstComp9 = LightData.LightCurveForm.lstComp1;
				ListBox lstComp10 = LightData.LightCurveForm.lstComp2;
				((Control)LightData.LightCurveForm.lstComp3).set_BackColor(darkGreen = Color.FromArgb(230, 255, 230));
				((Control)lstComp10).set_BackColor(foreColor = darkGreen);
				((Control)lstComp9).set_BackColor(foreColor);
				((Control)LightData.LightCurveForm.lstTarget).set_BackColor(Color.FromArgb(255, 255, 220));
				if (flag)
				{
					flag4 = (num10 / num9 < 0.2) | (num10 / num9 > 5.0);
					if (LightData.LightCurveForm.optComp1.get_Checked())
					{
						flag4 = !flag4;
					}
					if (flag4)
					{
						((Control)LightData.LightCurveForm.lblComp1).set_Text(string.Format("Comp 1 = {0,3:f2}", num10 / num9));
						((Control)LightData.LightCurveForm.lblComp1).set_ForeColor(Color.DarkRed);
						((Control)LightData.LightCurveForm.lstComp1).set_BackColor(Color.MistyRose);
						((Control)LightData.LightCurveForm.optComp1).set_Text("Not Used");
					}
					else
					{
						((Control)LightData.LightCurveForm.lblComp1).set_Text(string.Format("Comp 1 = {0,3:f2}", num10 / num9));
						((Control)LightData.LightCurveForm.optComp1).set_Text("Used");
						num20++;
					}
				}
				if (flag2)
				{
					flag5 = (num11 / num9 < 0.2) | (num11 / num9 > 5.0);
					if (LightData.LightCurveForm.optComp2.get_Checked())
					{
						flag5 = !flag5;
					}
					if (flag5)
					{
						((Control)LightData.LightCurveForm.lblComp2).set_Text(string.Format("Comp 2 = {0,3:f2}", num11 / num9));
						((Control)LightData.LightCurveForm.lblComp2).set_ForeColor(Color.DarkRed);
						((Control)LightData.LightCurveForm.lstComp2).set_BackColor(Color.MistyRose);
						((Control)LightData.LightCurveForm.optComp2).set_Text("Not Used");
					}
					else
					{
						((Control)LightData.LightCurveForm.lblComp2).set_Text(string.Format("Comp 2 = {0,3:f2}", num11 / num9));
						((Control)LightData.LightCurveForm.optComp2).set_Text("Used");
						num20++;
					}
				}
				if (flag3)
				{
					flag6 = (num12 / num9 < 0.2) | (num12 / num9 > 5.0);
					if (LightData.LightCurveForm.optComp3.get_Checked())
					{
						flag6 = !flag6;
					}
					if (flag6)
					{
						((Control)LightData.LightCurveForm.lblComp3).set_Text(string.Format("Comp 3 = {0,3:f2}", num12 / num9));
						((Control)LightData.LightCurveForm.lblComp3).set_ForeColor(Color.DarkRed);
						((Control)LightData.LightCurveForm.lstComp3).set_BackColor(Color.MistyRose);
						((Control)LightData.LightCurveForm.optComp3).set_Text("Not Used");
					}
					else
					{
						((Control)LightData.LightCurveForm.lblComp3).set_Text(string.Format("Comp 3 = {0,3:f2}", num12 / num9));
						((Control)LightData.LightCurveForm.optComp3).set_Text("Used");
						num20++;
					}
				}
			}
			for (int j = 0; j < starCountToPlot; j++)
			{
				num5 += Math.Pow((double)StarToPlot_FromAve[j] - num, 2.0);
				if (flag)
				{
					num6 += Math.Pow((double)Comp1ToPlot_FromAve[j] - num2, 2.0);
				}
				if (flag2)
				{
					num7 += Math.Pow((double)Comp2ToPlot_FromAve[j] - num3, 2.0);
				}
				if (flag3)
				{
					num8 += Math.Pow((double)Comp3ToPlot_FromAve[j] - num4, 2.0);
				}
			}
			num14 = Math.Sqrt(num5 / (double)(num13 - 1));
			if (flag)
			{
				num15 = Math.Sqrt(num6 / (double)(num13 - 1));
			}
			if (flag2)
			{
				num16 = Math.Sqrt(num7 / (double)(num13 - 1));
			}
			if (flag3)
			{
				num17 = Math.Sqrt(num8 / (double)(num13 - 1));
			}
			double num22 = 0.0;
			double num23 = 0.0;
			double num24 = 0.0;
			double num25 = 0.0;
			double num26 = 0.0;
			double num27 = 0.0;
			double num28 = 0.0;
			for (int k = 0; k < starCountToPlot; k++)
			{
				if (StarValidToPlot[k])
				{
					num22 = 0.0;
					num23 = 0.0;
					num24 = 0.0;
					num25 = 0.0;
					sdevs = new Sdevs();
					num22 = ((double)StarToPlot_FromAve[k] - num) / num14;
					if (flag)
					{
						num23 = ((double)Comp1ToPlot_FromAve[k] - num2) / num15;
					}
					if (num23 < num26)
					{
						num26 = num23;
					}
					if (flag2)
					{
						num24 = ((double)Comp2ToPlot_FromAve[k] - num3) / num16;
					}
					if (num24 < num27)
					{
						num27 = num24;
					}
					if (flag3)
					{
						num25 = ((double)Comp3ToPlot_FromAve[k] - num4) / num17;
					}
					if (num25 < num28)
					{
						num28 = num25;
					}
					sdevs.Frame = (int)FrameIDofPlot[k];
					sdevs.SDevT = num22;
					sdevs.SDev1 = num23;
					sdevs.SDev2 = num24;
					sdevs.SDev3 = num25;
					list.Add(sdevs);
				}
			}
			Sdevs.Comp1Used = Comp1Used;
			Sdevs.Comp2Used = Comp2Used;
			Sdevs.Comp3Used = Comp3Used;
			Sdevs.SortField = 0;
			string text = "";
			string text2 = "";
			list.Sort();
			if (!ForLightCurve)
			{
				PlotForm.lstTarget.get_Items().Clear();
				PlotForm.lstTarget.get_Items().Add((object)"Frame  Target   Comp1   Comp2   Comp3");
				PlotForm.lstComp1.get_Items().Clear();
				PlotForm.lstComp1.get_Items().Add((object)"Frame   Comp1");
				PlotForm.lstComp2.get_Items().Clear();
				PlotForm.lstComp2.get_Items().Add((object)"Frame   Comp2");
				PlotForm.lstComp3.get_Items().Clear();
				PlotForm.lstComp3.get_Items().Add((object)"Frame   Comp3");
				((Control)PlotForm.lblFrame).set_Text(string.Format("#{0,1:f0}", list[0].Frame));
			}
			else
			{
				LightData.LightCurveForm.lstTarget.get_Items().Clear();
				LightData.LightCurveForm.lstTarget.get_Items().Add((object)"Frame  Target   Comp1   Comp2   Comp3");
				LightData.LightCurveForm.lstComp1.get_Items().Clear();
				LightData.LightCurveForm.lstComp1.get_Items().Add((object)"Frame   Comp1");
				LightData.LightCurveForm.lstComp2.get_Items().Clear();
				LightData.LightCurveForm.lstComp2.get_Items().Add((object)"Frame   Comp2");
				LightData.LightCurveForm.lstComp3.get_Items().Clear();
				LightData.LightCurveForm.lstComp3.get_Items().Add((object)"Frame   Comp3");
				((Control)LightData.LightCurveForm.lblFrame).set_Text(string.Format("#{0,1:f0}", list[0].Frame));
			}
			double num29;
			double num30 = (num29 = 0.0 - list[0].SDevT);
			double num31 = num29 / (0.0 - list[1].SDevT);
			num21 = 0;
			if (Math.Abs(list[0].Frame - list[1].Frame) == NumberOfFramesBinned)
			{
				num21 = 1;
				num31 = num29 / (0.0 - list[2].SDevT);
				text = "*";
			}
			if ((Math.Abs(list[0].Frame - list[1].Frame) == NumberOfFramesBinned) & (Math.Abs(list[0].Frame - list[2].Frame) == NumberOfFramesBinned))
			{
				num21 = 2;
				num31 = num29 / (0.0 - list[3].SDevT);
				text = "*";
			}
			else if (Math.Abs(list[1].Frame - list[2].Frame) == NumberOfFramesBinned && ((Math.Abs(list[0].Frame - list[1].Frame) == NumberOfFramesBinned) | (Math.Abs(list[0].Frame - list[2].Frame) == NumberOfFramesBinned)))
			{
				num21 = 2;
				num31 = num29 / (0.0 - list[3].SDevT);
				text = "*";
			}
			for (int l = 0; l < 20; l++)
			{
				if (!ForLightCurve)
				{
					PlotForm.lstTarget.get_Items().Add((object)list[l].CompareAll);
				}
				else
				{
					LightData.LightCurveForm.lstTarget.get_Items().Add((object)list[l].CompareAll);
				}
			}
			if (flag)
			{
				Sdevs.SortField = 1;
				list.Sort();
				for (int m = 0; m < 20; m++)
				{
					if (!ForLightCurve)
					{
						PlotForm.lstComp1.get_Items().Add((object)list[m].CompareComp1);
					}
					else
					{
						LightData.LightCurveForm.lstComp1.get_Items().Add((object)list[m].CompareComp1);
					}
				}
				if (!flag4 && 0.0 - list[0].SDev1 > num30)
				{
					num30 = 0.0 - list[0].SDev1;
				}
			}
			if (flag2)
			{
				Sdevs.SortField = 2;
				list.Sort();
				for (int n = 0; n < 20; n++)
				{
					if (!ForLightCurve)
					{
						PlotForm.lstComp2.get_Items().Add((object)list[n].CompareComp2);
					}
					else
					{
						LightData.LightCurveForm.lstComp2.get_Items().Add((object)list[n].CompareComp2);
					}
				}
				if (!flag5 && 0.0 - list[0].SDev2 > num30)
				{
					num30 = 0.0 - list[0].SDev2;
				}
			}
			if (flag3)
			{
				Sdevs.SortField = 3;
				list.Sort();
				for (int num32 = 0; num32 < 20; num32++)
				{
					if (!ForLightCurve)
					{
						PlotForm.lstComp3.get_Items().Add((object)list[num32].CompareComp3);
					}
					else
					{
						LightData.LightCurveForm.lstComp3.get_Items().Add((object)list[num32].CompareComp3);
					}
				}
				if (!flag6 && 0.0 - list[0].SDev3 > num30)
				{
					num30 = 0.0 - list[0].SDev3;
				}
			}
			switch (num20)
			{
			case 0:
				num18 = 2.0;
				num19 = 1.3;
				break;
			case 1:
				num18 = 1.7;
				num19 = 1.2;
				break;
			case 2:
				num18 = 1.6;
				num19 = 1.15;
				break;
			case 3:
				num18 = 1.5;
				num19 = 1.1;
				break;
			}
			double num33 = 1.0;
			if (StarCountToPlot < 50)
			{
				num33 = 10.0;
			}
			else if (StarCountToPlot < 100)
			{
				num33 = 1.5;
			}
			else if (StarCountToPlot < 150)
			{
				num33 = 1.3;
			}
			else if (StarCountToPlot < 200)
			{
				num33 = 1.2;
			}
			else if (StarCountToPlot < 400)
			{
				num33 = 1.1;
			}
			switch (num21)
			{
			case 2:
				num33 = 0.8 * (num33 - 1.0) + 1.0;
				break;
			case 3:
				num33 = 0.6 * (num33 - 1.0) + 1.0;
				break;
			}
			num18 *= num33;
			num19 *= num33;
			if (!ForLightCurve)
			{
				((Control)PlotForm.lblProbable).set_Text(string.Format(">{0,1:f2}", num18));
				((Control)PlotForm.lblPossible).set_Text(string.Format(">{0,1:f2}", num19));
			}
			else
			{
				((Control)LightData.LightCurveForm.lblProbable).set_Text(string.Format(">{0,1:f2}", num18));
				((Control)LightData.LightCurveForm.lblPossible).set_Text(string.Format(">{0,1:f2}", num19));
			}
			if (num31 >= num18)
			{
				if (!ForLightCurve)
				{
					((Control)PlotForm.lblRatio12).set_ForeColor(Color.Green);
				}
				else
				{
					((Control)LightData.LightCurveForm.lblRatio12).set_ForeColor(Color.Green);
				}
				text2 = "✔";
			}
			else if (num31 >= num19)
			{
				if (!ForLightCurve)
				{
					((Control)PlotForm.lblRatio12).set_ForeColor(Color.Navy);
				}
				else
				{
					((Control)LightData.LightCurveForm.lblRatio12).set_ForeColor(Color.Navy);
				}
				text2 = "?";
			}
			if (num29 != num30 && num29 < num19 * num30)
			{
				if (!ForLightCurve)
				{
					((Control)PlotForm.lblRatio12).set_ForeColor(Color.Orange);
				}
				else
				{
					((Control)LightData.LightCurveForm.lblRatio12).set_ForeColor(Color.Orange);
				}
				text2 = "??";
			}
			if (!ForLightCurve)
			{
				if (num31 < num19)
				{
					((Control)PlotForm.lblRatio12).set_ForeColor(Color.Red);
					text2 = "X";
				}
			}
			else if (num31 < num19)
			{
				((Control)LightData.LightCurveForm.lblRatio12).set_ForeColor(Color.Red);
				text2 = "X";
			}
			if (!ForLightCurve)
			{
				((Control)PlotForm.lblRatio12).set_Text(string.Format("{0,1:f2} ", num31) + text + text2);
			}
			else
			{
				((Control)LightData.LightCurveForm.lblRatio12).set_Text(string.Format("{0,1:f2} ", num31) + text + text2);
			}
			int num34 = (int)(10.0 * num30) + 3;
			int num35 = 0;
			int[] array = new int[num34];
			Sdevs.SortField = 0;
			list.Sort();
			for (int num36 = 0; num36 < list.Count; num36++)
			{
				num35 = (int)(-10.0 * list[num36].SDevT);
				if (num35 < 0)
				{
					break;
				}
				array[num35]++;
			}
			if (!ForLightCurve)
			{
				PlotShortEvent(PlotForm.picTarget, array, num21);
			}
			else
			{
				PlotShortEvent(LightData.LightCurveForm.picTarget, array, num21);
			}
			if (!ForLightCurve)
			{
				((Control)PlotForm.picComp1).set_Visible(flag);
			}
			else
			{
				((Control)LightData.LightCurveForm.picComp1).set_Visible(flag);
			}
			if (flag)
			{
				array = new int[num34];
				Sdevs.SortField = 1;
				list.Sort();
				for (int num37 = 0; num37 < list.Count; num37++)
				{
					num35 = (int)(-10.0 * list[num37].SDev1);
					if (num35 < 0)
					{
						break;
					}
					if (num35 < num34 - 1)
					{
						array[num35]++;
					}
				}
				if (!ForLightCurve)
				{
					PlotShortEvent(PlotForm.picComp1, array, 0);
				}
				else
				{
					PlotShortEvent(LightData.LightCurveForm.picComp1, array, 0);
				}
			}
			if (!ForLightCurve)
			{
				((Control)PlotForm.picComp2).set_Visible(flag2);
			}
			else
			{
				((Control)LightData.LightCurveForm.picComp2).set_Visible(flag2);
			}
			if (flag2)
			{
				Sdevs.SortField = 2;
				list.Sort();
				array = new int[num34];
				for (int num38 = 0; num38 < list.Count; num38++)
				{
					num35 = (int)(-10.0 * list[num38].SDev2);
					if (num35 < 0)
					{
						break;
					}
					if (num35 < num34 - 1)
					{
						array[num35]++;
					}
				}
				if (!ForLightCurve)
				{
					PlotShortEvent(PlotForm.picComp2, array, 0);
				}
				else
				{
					PlotShortEvent(LightData.LightCurveForm.picComp2, array, 0);
				}
			}
			if (!ForLightCurve)
			{
				((Control)PlotForm.picComp3).set_Visible(flag3);
			}
			else
			{
				((Control)LightData.LightCurveForm.picComp3).set_Visible(flag3);
			}
			if (!flag3)
			{
				return;
			}
			Sdevs.SortField = 3;
			list.Sort();
			array = new int[num34];
			for (int num39 = 0; num39 < list.Count; num39++)
			{
				num35 = (int)(-10.0 * list[num39].SDev3);
				if (num35 < 0)
				{
					break;
				}
				if (num35 < num34 - 1)
				{
					array[num35]++;
				}
			}
			if (!ForLightCurve)
			{
				PlotShortEvent(PlotForm.picComp3, array, 0);
			}
			else
			{
				PlotShortEvent(LightData.LightCurveForm.picComp3, array, 0);
			}
		}

		internal static void PlotShortEvent(PictureBox pic, int[] Columns, int DrawAdjacentHighFrame)
		{
			int num = ((Control)pic).get_Width() - 3;
			int num2 = ((Control)pic).get_Height() - 3;
			new Pen(Color.Black, 1f);
			Brush cyan = Brushes.Cyan;
			Brush red = Brushes.Red;
			Brush gold = Brushes.Gold;
			Bitmap image = new Bitmap(num, num2);
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Color.FromArgb(0, 20, 100));
			int num3 = Columns.Length;
			float num4 = (float)(num - 2) / (float)num3;
			float num5 = (float)(num2 - 1) / 10f;
			bool flag = false;
			for (int num6 = num3 - 1; num6 >= 0; num6--)
			{
				int num7 = Columns[num6];
				if (num7 > 0)
				{
					if (num7 > 10)
					{
						num7 = 10;
					}
					if (!flag)
					{
						graphics.FillRectangle(red, (float)num6 * num4, (float)(num2 - 1) - num5 * (float)num7, num4 * 2f, num5 * (float)num7);
						flag = true;
					}
					else if (flag && DrawAdjacentHighFrame > 0)
					{
						graphics.FillRectangle(gold, (float)num6 * num4, (float)(num2 - 1) - num5 * (float)num7, num4, num5 * (float)num7);
						DrawAdjacentHighFrame--;
					}
					else
					{
						graphics.FillRectangle(cyan, (float)num6 * num4, (float)(num2 - 1) - num5 * (float)num7, num4, num5 * (float)num7);
					}
				}
			}
			pic.set_Image((Image)image);
			graphics.Dispose();
		}

		internal static void PlotLightCurve()
		{
			if (IsPlotting)
			{
				return;
			}
			LightData.LightCurveForm.SetTextBoxes();
			IsPlotting = true;
			((Control)LightData.LightCurveForm.updnScale).set_Enabled(false);
			float num = (float)LightData.LightCurveForm.updnScale.get_Value();
			float num2 = 1f;
			float num3 = 1f;
			int num4 = 1;
			string text = "";
			bool flag = true;
			float num5 = 10000f;
			float num6 = -10000f;
			if (LightData.LightCurveForm.LCD == null)
			{
				LightData.LightCurveForm.LCD = new LightCurveData();
			}
			Font font = new Font("Arial", 8f, FontStyle.Bold);
			Brush black = Brushes.Black;
			Pen pen = new Pen(Color.Black, 1f);
			Pen pen2 = new Pen(Color.Black, 2f);
			Pen pen3 = new Pen(Color.FromArgb(100, Color.Plum));
			Pen pen4 = new Pen(Color.Red, 1.5f);
			new Pen(Color.Orange, 1f);
			Brush darkBlue = Brushes.DarkBlue;
			Brush fuchsia = Brushes.Fuchsia;
			Brush aquamarine = Brushes.Aquamarine;
			new Pen(Color.Blue);
			Pen pen5 = new Pen(Color.FromArgb(200, Color.SaddleBrown), 2f);
			Pen pen6 = new Pen(Color.FromArgb(130, Color.MediumSeaGreen), 1.5f);
			Pen pen7 = new Pen(Color.FromArgb(130, Color.DarkOrange), 1.5f);
			Pen pen8 = new Pen(Color.FromArgb(130, Color.MediumVioletRed));
			Pen pen9 = new Pen(Color.FromArgb(130, Color.DarkBlue));
			Pen pen10 = new Pen(Color.FromArgb(130, Color.Coral), 1f);
			pen10.DashPattern = new float[2] { 1f, 10f };
			LightData.LightCurveForm.SetSizes(ResizeEvent: false);
			int num7 = StartOfLightCurveSelection / NumberOfFramesBinned;
			int num8 = EndOfLightCurveSelection / NumberOfFramesBinned;
			for (int i = num7; i < num8 && i < StarCountToPlot; i++)
			{
				if (StarToPlot[i] < num5)
				{
					num5 = StarToPlot[i];
				}
				if (StarToPlot[i] > num6)
				{
					num6 = StarToPlot[i];
				}
			}
			if (num5 >= 0f)
			{
				num5 = 0f;
			}
			num5 -= 0.05f * num6;
			num6 *= 1.05f;
			int width = ((Control)LightData.LightCurveForm.picPlot).get_Width();
			int height = ((Control)LightData.LightCurveForm.picPlot).get_Height();
			if (width < 10 || height < 10)
			{
				IsPlotting = false;
				return;
			}
			if (num6 != num5)
			{
				num3 = (float)height / (num6 - num5) * VerticalScaleAdjustmentLC;
				LightCurve_ZeroHeight = (0f - num5) / (num6 - num5) * (float)height;
			}
			else
			{
				num3 = 1f;
				LightCurve_ZeroHeight = (float)height * 0.1f;
			}
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Color.White);
			graphics.FillRectangle(aquamarine, ((float)StartOfLightCurveSelection - 0.5f) * num / (float)NumberOfFramesBinned, 0f, (float)(EndOfLightCurveSelection - StartOfLightCurveSelection + 1) * num / (float)NumberOfFramesBinned, height);
			if ((double)LightCurve_Height_100 > 0.02)
			{
				graphics.DrawLine(pen4, 0f, LightCurve_Height_100 * (float)height, width, LightCurve_Height_100 * (float)height);
			}
			num4 = 50;
			num4 = ((LightData.LightCurveForm.updnScale.get_Value() > 10m) ? 5 : ((LightData.LightCurveForm.updnScale.get_Value() >= 5m) ? 10 : ((LightData.LightCurveForm.updnScale.get_Value() >= 2m) ? 20 : ((LightData.LightCurveForm.updnScale.get_Value() >= 1m) ? 40 : ((!(LightData.LightCurveForm.updnScale.get_Value() > 0.5m)) ? 200 : 100)))));
			_ = FrameIDofPlot[2] - FrameIDofPlot[0];
			_ = 2f;
			for (int j = 0; j < StarCountToPlot; j++)
			{
				if ((((NumberOfFramesBinned == 1) & (FrameIDofPlot[j] % (float)num4 == 0f)) | ((NumberOfFramesBinned != 1) & (j % num4 == 0))) && (j <= 0 || FrameIDofPlot[j] != FrameIDofPlot[j - 1]))
				{
					text = string.Format("{0,1:f0}", FrameIDofPlot[j]);
					float num9 = ((float)j - 0.5f) * num;
					graphics.DrawLine(pen3, num9 + 0.5f * num, 0f, num9 + 0.5f * num, height);
					graphics.DrawLine(pen, num9, 0f, num9, 10f);
					graphics.DrawLine(pen, num9, height - 14, num9, height - 20);
					graphics.DrawLine(pen2, num9, height - 15, num9 + num, height - 15);
					graphics.DrawLine(pen, num9 + num, height - 14, num9 + num, (float)height - 18.5f);
					graphics.DrawString(text, font, black, num9 + num / 2f - graphics.MeasureString(text, font).Width / 2f + 1f, height - 14);
				}
			}
			graphics.DrawRectangle(pen, 0, 0, width - 1, height - 1);
			float num10 = (float)height - LightCurve_ZeroHeight;
			graphics.DrawLine(pen, 0f, num10, (float)StarCountToPlot * num, num10);
			float num11 = 1.7f + (float)LightData.LightCurveForm.updnScale.get_Value() / 8f;
			LightData.LightCurveForm.LCD.LightValues = new List<int>();
			LightData.LightCurveForm.LCD.LightValuesValid = new List<bool>();
			for (int k = 0; k < StarCountToPlot; k++)
			{
				float num9 = (float)k * num;
				num10 = (float)height - (LightCurve_ZeroHeight + StarToPlot[k] * num3);
				if (k >= num7 && k <= num8)
				{
					LightData.LightCurveForm.LCD.LightValues.Add((int)(StarToPlot[k] / num6 * 10000f));
					LightData.LightCurveForm.LCD.LightValuesValid.Add(StarValid[k]);
				}
				if (StarValidToPlot[k])
				{
					graphics.FillEllipse(darkBlue, num9 - num11, num10 - num11, 2f * num11, 2f * num11);
				}
				else
				{
					graphics.FillEllipse(fuchsia, num9 - num11, num10 - num11, 2f * num11, 2f * num11);
				}
			}
			LightData.LightCurveForm.LCD.NumPoints = LightData.LightCurveForm.LCD.LightValues.Count;
			if (LightData.LightCurveForm.chkTarget.get_Checked())
			{
				float y;
				float x = (y = 0f);
				if (PlotRunningAverageLC == 0)
				{
					for (int l = 0; l < StarCountToPlot; l++)
					{
						float num9 = (float)l * num;
						num10 = (float)height - (LightCurve_ZeroHeight + StarToPlot[l] * num3 * num2);
						if (l > 0)
						{
							if (StarValidToPlot[l - 1] & StarValidToPlot[l])
							{
								graphics.DrawLine(pen9, x, y, num9, num10);
							}
							else
							{
								graphics.DrawLine(pen10, x, y, num9, num10);
							}
						}
						x = num9;
						y = num10;
					}
				}
				else
				{
					CreateRunningAverageArray(ref StarToPlot, PlotRunningAverageLC);
					for (int m = 0; m < StarCountToPlot; m++)
					{
						float num9 = (float)m * num;
						num10 = (float)height - (LightCurve_ZeroHeight + RunningAverageValues[m] * num3 * num2);
						if (m > 0 && ((RunningAverageValues[m] != 0f) & (RunningAverageValues[m - 1] != 0f)))
						{
							graphics.DrawLine(pen9, x, y, num9, num10);
						}
						x = num9;
						y = num10;
					}
				}
			}
			if (LightData.LightCurveForm.chkShowBackground.get_Checked() & (Background != 0f))
			{
				num2 = StarLevel / Background / 2f;
				float y;
				float x = (y = 0f);
				if (((ListControl)LightData.LightCurveForm.cmbPlotAverage).get_SelectedIndex() == 0)
				{
					for (int n = 0; n < StarCountToPlot; n++)
					{
						float num9 = (float)n * num;
						num10 = (float)height - (LightCurve_ZeroHeight + StarBackground[n] * num3 * num2);
						if (n > 0)
						{
							graphics.DrawLine(pen5, x, y, num9, num10);
						}
						x = num9;
						y = num10;
					}
				}
				else
				{
					CreateRunningAverageArray(ref StarBackground, PlotRunningAverageLC);
					for (int num12 = 0; num12 < StarCountToPlot; num12++)
					{
						float num9 = (float)num12 * num;
						num10 = (float)height - (LightCurve_ZeroHeight + RunningAverageValues[num12] * num3 * num2);
						if (num12 > 0)
						{
							graphics.DrawLine(pen5, x, y, num9, num10);
						}
						x = num9;
						y = num10;
					}
				}
			}
			if (LightData.LightCurveForm.chkComp1.get_Checked())
			{
				num2 = 1f;
				if (LightData.LightCurveForm.chkScaleComp.get_Checked())
				{
					num2 = (float)((double)(StarLevel / Comp1Level) * 0.9);
				}
				float y;
				float x = (y = 0f);
				if (((ListControl)LightData.LightCurveForm.cmbPlotAverage).get_SelectedIndex() == 0)
				{
					flag = true;
					for (int num13 = 0; num13 < StarCountToPlot; num13++)
					{
						float num9 = (float)num13 * num;
						num10 = (float)height - (LightCurve_ZeroHeight + Comp1ToPlot[num13] * num3 * num2);
						if (!flag & (Comp1ToPlot[num13] != 0f))
						{
							graphics.DrawLine(pen6, x, y, num9, num10);
						}
						x = num9;
						y = num10;
						flag = Comp1ToPlot[num13] == 0f;
					}
				}
				else
				{
					CreateRunningAverageArray(ref Comp1ToPlot, PlotRunningAverageLC);
					for (int num14 = 0; num14 < StarCountToPlot; num14++)
					{
						float num9 = (float)num14 * num;
						num10 = (float)height - (LightCurve_ZeroHeight + RunningAverageValues[num14] * num3 * num2);
						if (num14 > 0)
						{
							graphics.DrawLine(pen6, x, y, num9, num10);
						}
						x = num9;
						y = num10;
					}
				}
			}
			if (LightData.LightCurveForm.chkComp2.get_Checked())
			{
				num2 = 1f;
				if (LightData.LightCurveForm.chkScaleComp.get_Checked())
				{
					num2 = (float)((double)(StarLevel / Comp2Level) * 0.8);
				}
				float y;
				float x = (y = 0f);
				if (((ListControl)LightData.LightCurveForm.cmbPlotAverage).get_SelectedIndex() == 0)
				{
					flag = true;
					for (int num15 = 0; num15 < StarCountToPlot; num15++)
					{
						float num9 = (float)num15 * num;
						num10 = (float)height - (LightCurve_ZeroHeight + Comp2ToPlot[num15] * num3 * num2);
						if (!flag & (Comp2ToPlot[num15] != 0f))
						{
							graphics.DrawLine(pen7, x, y, num9, num10);
						}
						x = num9;
						y = num10;
						flag = Comp2ToPlot[num15] == 0f;
					}
				}
				else
				{
					CreateRunningAverageArray(ref Comp2ToPlot, PlotRunningAverageLC);
					for (int num16 = 0; num16 < StarCountToPlot; num16++)
					{
						float num9 = (float)num16 * num;
						num10 = (float)height - (LightCurve_ZeroHeight + RunningAverageValues[num16] * num3 * num2);
						if (num16 > 0)
						{
							graphics.DrawLine(pen7, x, y, num9, num10);
						}
						x = num9;
						y = num10;
					}
				}
			}
			if (LightData.LightCurveForm.chkComp3.get_Checked())
			{
				num2 = 1f;
				if (LightData.LightCurveForm.chkScaleComp.get_Checked())
				{
					num2 = (float)((double)(StarLevel / Comp3Level) * 0.7);
				}
				float y;
				float x = (y = 0f);
				if (((ListControl)LightData.LightCurveForm.cmbPlotAverage).get_SelectedIndex() == 0)
				{
					flag = true;
					for (int num17 = 0; num17 < StarCountToPlot; num17++)
					{
						float num9 = (float)num17 * num;
						num10 = (float)height - (LightCurve_ZeroHeight + Comp3ToPlot[num17] * num3 * num2);
						if (!flag & (Comp3ToPlot[num17] != 0f))
						{
							graphics.DrawLine(pen8, x, y, num9, num10);
						}
						x = num9;
						y = num10;
						flag = Comp3ToPlot[num17] == 0f;
					}
				}
				else
				{
					CreateRunningAverageArray(ref Comp3ToPlot, PlotRunningAverageLC);
					for (int num18 = 0; num18 < StarCountToPlot; num18++)
					{
						float num9 = (float)num18 * num;
						num10 = (float)height - (LightCurve_ZeroHeight + RunningAverageValues[num18] * num3 * num2);
						if (num18 > 0)
						{
							graphics.DrawLine(pen8, x, y, num9, num10);
						}
						x = num9;
						y = num10;
					}
				}
			}
			LightData.LightCurveForm.picPlot.set_Image((Image)image);
			graphics.Dispose();
			((Control)LightData.LightCurveForm).Focus();
			try
			{
				Application.DoEvents();
			}
			catch
			{
			}
			((Control)LightData.LightCurveForm.updnScale).set_Enabled(true);
			IsPlotting = false;
		}
	}
}
