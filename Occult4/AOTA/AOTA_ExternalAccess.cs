#define TRACE
using System;
using System.Diagnostics;
using System.Globalization;
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using Occult.SDK;

namespace AOTA
{
	public class AOTA_ExternalAccess : IAOTAExternalAccess
	{
		internal static bool RunFromTangra;

		internal static bool ResultsAvailable;

		internal static float FrameID_DisplayedInTangra;

		internal static bool TangraFrameDisplayFromAOTA;

		internal static bool TangraFrameDisplayFromTangra;

		internal static IAOTAClientCallbacks AOTA_Client;

		public bool ResultsAreAvailable => ResultsAvailable;

		public bool IsMiss => AOTAData.All_Events_Are_Non_Events();

		public EventResults ResultsForEvent1 => AOTAData.Results[0];

		public EventResults ResultsForEvent2 => AOTAData.Results[1];

		public EventResults ResultsForEvent3 => AOTAData.Results[2];

		public EventResults ResultsForEvent4 => AOTAData.Results[3];

		public EventResults ResultsForEvent5 => AOTAData.Results[4];

		public Camera Camera
		{
			get
			{
				return AOTAData.CameraSpecs;
			}
			set
			{
				AOTAData.CameraSpecs = value;
			}
		}

		public string ResultsReport => AOTAData.CreateTextReport();

		public string AOTA_Version => "AOTA v" + Assembly.GetExecutingAssembly().GetName(copiedName: false).Version;

		public void InitialiseAOTA(string SourceFile, IAOTAClientCallbacks AOTAClient)
		{
			if (Thread.CurrentThread.CurrentCulture.NumberFormat.NumberDecimalSeparator != ".")
			{
				Thread.CurrentThread.CurrentCulture = new CultureInfo("en-US");
			}
			AOTAData.TangraSourceFile = SourceFile;
			AOTA_Client = AOTAClient;
			AOTAData.Comp1Used = (AOTAData.Comp2Used = (AOTAData.Comp3Used = (AOTAData.TimePresentInSourceData = (AOTAData.OnlySomeTimesPresentInSourceData = false))));
			AOTAData.Comp1Count = (AOTAData.Comp2Count = (AOTAData.Comp3Count = (AOTAData.StarCount = (AOTAData.StarCountToPlot = 0))));
			AOTAData.StarLevel = (AOTAData.Background = (AOTAData.Comp1Level = (AOTAData.Comp2Level = 0f)));
			AOTAData.X0 = (AOTAData.X1 = (AOTAData.X2 = (AOTAData.X5 = (AOTAData.X6 = (AOTAData.X7 = 0)))));
			AOTAData.NumberOfFramesBinned = 1;
			AOTAData.FirstFrameUsedInAnalysis = 0;
			AOTAData.Background = (AOTAData.Comp1Level = (AOTAData.Comp2Level = (AOTAData.Comp3Level = (AOTAData.StarLevel = 0f))));
			AOTAData.IsPAL = true;
			AOTAData.CameraCorrectionsHaveBeenApplied = false;
			AOTAData.CameraSpecs = default(Camera);
			ResultsAvailable = false;
			for (int i = 0; i < 5; i++)
			{
				AOTAData.EventAnalysed[i] = false;
			}
		}

		public void InitialiseAOTA(IAOTAClientCallbacks AOTAClient)
		{
			InitialiseAOTA("", AOTAClient);
		}

		public void Set_TargetData(float[] data)
		{
			int num = data.GetLength(0);
			if (num > 50000)
			{
				num = 50000;
			}
			AOTAData.StarCount = 0;
			for (int i = 0; i < num; i++)
			{
				if (float.IsNaN(data[i]))
				{
					AOTAData.StarValid[i] = false;
					AOTAData.Star[i] = 0f;
				}
				else
				{
					AOTAData.StarValid[i] = true;
					AOTAData.Star[i] = data[i];
					AOTAData.StarLevel += data[i];
				}
				AOTAData.StarCount++;
			}
		}

		public void Set_TargetData(float[] foreground, float[] background)
		{
			int num = foreground.GetLength(0);
			if (num > 50000)
			{
				num = 50000;
			}
			AOTAData.StarCount = 0;
			for (int i = 0; i < num; i++)
			{
				if (float.IsNaN(foreground[i]))
				{
					AOTAData.StarValid[i] = false;
					AOTAData.Star[i] = (AOTAData.StarBackground[i] = 0f);
				}
				else
				{
					AOTAData.StarValid[i] = true;
					AOTAData.Star[i] = foreground[i] - background[num];
					AOTAData.StarLevel += AOTAData.Star[i];
					AOTAData.StarBackground[i] = background[i];
					AOTAData.Background += background[i];
					AOTAData.StarForegroundAperture[i] = foreground[i];
				}
				AOTAData.StarCount++;
			}
		}

		public void Set_TargetData_BackgroundAlreadySubtracted(float[] foreground, float[] background)
		{
			int num = foreground.GetLength(0);
			if (num > 50000)
			{
				num = 50000;
			}
			AOTAData.StarCount = 0;
			for (int i = 0; i < num; i++)
			{
				if (float.IsNaN(foreground[i]))
				{
					AOTAData.StarValid[i] = false;
					AOTAData.Star[i] = (AOTAData.StarBackground[i] = 0f);
				}
				else
				{
					AOTAData.StarValid[i] = true;
					AOTAData.Star[i] = foreground[i];
					AOTAData.StarLevel += AOTAData.Star[i];
					AOTAData.StarBackground[i] = background[i];
					AOTAData.Background += background[i];
					AOTAData.StarForegroundAperture[i] = foreground[i] + background[i];
				}
				AOTAData.StarCount++;
			}
			AOTAData.Background_Point = false;
			AOTAData.Background_Average = true;
			AOTAData.AllowBothPoint_Average = true;
		}

		public void Set_FrameID(float[] data)
		{
			int num = data.GetLength(0);
			if (num > 50000)
			{
				num = 50000;
			}
			for (int i = 0; i < num; i++)
			{
				AOTAData.FrameID[i] = data[i];
			}
		}

		public void Set_Comp1Data(float[] data)
		{
			int num = data.GetLength(0);
			if (num > 50000)
			{
				num = 50000;
			}
			AOTAData.Comp1Count = 0;
			for (int i = 0; i < num; i++)
			{
				if (float.IsNaN(data[i]))
				{
					AOTAData.Comp1[i] = 0f;
				}
				else
				{
					AOTAData.Comp1[i] = data[i];
					AOTAData.Comp1Level += data[i];
				}
				AOTAData.Comp1Count++;
			}
			AOTAData.Comp1Used = AOTAData.Comp1Count > 0;
		}

		public void Set_Comp1Data(float[] foreground, float[] background)
		{
			int num = foreground.GetLength(0);
			if (num > 50000)
			{
				num = 50000;
			}
			AOTAData.Comp1Count = 0;
			for (int i = 0; i < num; i++)
			{
				if (float.IsNaN(foreground[i]))
				{
					AOTAData.Comp1[i] = 0f;
				}
				else
				{
					AOTAData.Comp1[i] = foreground[i] - background[num];
					AOTAData.Comp1Level += AOTAData.Comp1[i];
				}
				AOTAData.Comp1Count++;
			}
			AOTAData.Comp1Used = AOTAData.Comp1Count > 0;
		}

		public void Set_Comp2Data(float[] data)
		{
			int num = data.GetLength(0);
			if (num > 50000)
			{
				num = 50000;
			}
			AOTAData.Comp2Count = 0;
			for (int i = 0; i < num; i++)
			{
				if (float.IsNaN(data[i]))
				{
					AOTAData.Comp2[i] = 0f;
				}
				else
				{
					AOTAData.Comp2[i] = data[i];
					AOTAData.Comp2Level += data[i];
				}
				AOTAData.Comp2Count++;
			}
			AOTAData.Comp2Used = AOTAData.Comp2Count > 0;
		}

		public void Set_Comp2Data(float[] foreground, float[] background)
		{
			int num = foreground.GetLength(0);
			if (num > 50000)
			{
				num = 50000;
			}
			AOTAData.Comp2Count = 0;
			for (int i = 0; i < num; i++)
			{
				if (float.IsNaN(foreground[i]))
				{
					AOTAData.Comp2[i] = 0f;
				}
				else
				{
					AOTAData.Comp2[i] = foreground[i] - background[num];
					AOTAData.Comp2Level += AOTAData.Comp2[i];
				}
				AOTAData.Comp2Count++;
			}
			AOTAData.Comp2Used = AOTAData.Comp2Count > 0;
		}

		public void Set_Comp3Data(float[] data)
		{
			int num = data.GetLength(0);
			if (num > 50000)
			{
				num = 50000;
			}
			AOTAData.Comp3Count = 0;
			for (int i = 0; i < num; i++)
			{
				if (float.IsNaN(data[i]))
				{
					AOTAData.Comp3[i] = 0f;
				}
				else
				{
					AOTAData.Comp3[i] = data[i];
					AOTAData.Comp3Level += data[i];
				}
				AOTAData.Comp3Count++;
			}
			AOTAData.Comp3Used = AOTAData.Comp3Count > 0;
		}

		public void Set_Comp3Data(float[] foreground, float[] background)
		{
			int num = foreground.GetLength(0);
			if (num > 50000)
			{
				num = 50000;
			}
			AOTAData.Comp3Count = 0;
			for (int i = 0; i < num; i++)
			{
				if (float.IsNaN(foreground[i]))
				{
					AOTAData.Comp3[i] = 0f;
				}
				else
				{
					AOTAData.Comp3[i] = foreground[i] - background[num];
					AOTAData.Comp3Level += AOTAData.Comp3[i];
				}
				AOTAData.Comp3Count++;
			}
			AOTAData.Comp3Used = AOTAData.Comp3Count > 0;
		}

		public void Set_TimeBase(double[] data)
		{
			Set_TimeBase(data, CameraCorrectionsHaveBeenApplied: false);
		}

		public void Set_TimeBase(double[] data, bool CameraCorrectionsHaveBeenApplied)
		{
			AOTAData.CameraCorrectionsHaveBeenApplied = CameraCorrectionsHaveBeenApplied;
			double num = 0.0;
			int num2 = 0;
			int num3 = data.GetLength(0);
			if (num3 > 50000)
			{
				num3 = 50000;
			}
			for (int i = 0; i < num3; i++)
			{
				AOTAData.FrameTimeSecs[i] = data[i];
			}
			if (num3 > 10)
			{
				if (data[0] != 0.0)
				{
					num = (data[num3 - 1] - data[0]) / (double)(2 * (num3 - 1));
				}
				else if (data[1] != 0.0)
				{
					num = (data[num3 - 1] - data[1]) / (double)(2 * (num3 - 2));
				}
			}
			AOTAData.SetCameraDelays_AAV_Files = false;
			if ((data[0] != 0.0) & (data[1] == 0.0) & (data[num3 - 1] != 0.0))
			{
				AOTAData.SetCameraDelays_AAV_Files = true;
			}
			if (AOTAData.CameraCorrectionsHaveBeenApplied)
			{
				for (int j = 0; j < num3; j++)
				{
					if (AOTAData.FrameTimeSecs[j] != 0.0)
					{
						AOTAData.FrameTimeSecs[j] -= num;
					}
				}
			}
			if (num3 > 1)
			{
				AOTAData.TimePresentInSourceData = true;
				AOTAData.OnlySomeTimesPresentInSourceData = false;
				for (int k = 0; k < num3; k++)
				{
					if (AOTAData.FrameTimeSecs[k] == 0.0)
					{
						num2++;
					}
				}
				if ((double)(num3 - num2) / (double)num3 < 0.8)
				{
					AOTAData.OnlySomeTimesPresentInSourceData = true;
				}
			}
			if (AOTAData.TimePresentInSourceData)
			{
				AOTAData.TimeSource = 1;
			}
		}

		public void Set_VideoCamera(string VideoCamera)
		{
			AOTAData.Camera_from_Tangra = VideoCamera;
		}

		public bool RunAOTA(IWin32Window parentWindow)
		{
			RunAOTA(parentWindow, 0, 0, RunModally: true);
			return ResultsAvailable;
		}

		public bool RunAOTA(IWin32Window parentWindow, int FirstFrame, int FramesInIntegration)
		{
			RunAOTA(parentWindow, FirstFrame, FramesInIntegration, RunModally: true);
			return ResultsAvailable;
		}

		public bool RunAOTA(IWin32Window parentWindow, int FirstFrame, int FramesInIntegration, bool RunModally)
		{
			//IL_002a: Unknown result type (might be due to invalid IL or missing references)
			//IL_00db: Unknown result type (might be due to invalid IL or missing references)
			try
			{
				if (AOTAData.StarCount < 50)
				{
					MessageBox.Show("AOTA requires at least 50 measurements to analyse the light curve\r\n\r\nYour measurements contain only " + AOTAData.StarCount + ", and cannot be processed by AOTA", "Not enough measurements", (MessageBoxButtons)0, (MessageBoxIcon)48);
					ResultsAvailable = false;
					try
					{
						ThreadPool.QueueUserWorkItem(delegate
						{
							Thread.Sleep(1000);
							try
							{
								AOTA_Client.OnAOTAFormClosing();
							}
							catch
							{
							}
						});
					}
					catch (Exception value)
					{
						Trace.WriteLine(value);
					}
					return false;
				}
				AOTAData.IntegrationFromLightCurve = false;
				RunFromTangra = true;
				if (FirstFrame < 0)
				{
					FirstFrame = 0;
				}
				if (FramesInIntegration < 1)
				{
					FramesInIntegration = 0;
				}
				AOTAData.FramesInIntegration_fromTangra = FramesInIntegration;
				AOTAData.FirstFrameUsedInAnalysis = FirstFrame;
				if (FramesInIntegration < 1)
				{
					AOTAData.NumberOfFramesBinned = 1;
				}
				else
				{
					AOTAData.NumberOfFramesBinned = FramesInIntegration;
				}
				AOTAData.IntegrationPreset = false;
				AOTAData.ShowPlotForm(parentWindow, RunModally);
				if (RunModally)
				{
					RunFromTangra = false;
					return ResultsAvailable;
				}
				ResultsAvailable = false;
				return false;
			}
			catch (Exception ex)
			{
				MessageBox.Show("RunAOTA\r\n" + ex.ToString());
				return false;
			}
		}

		public void Set_AOTA_DisplayLocation(int FrameID)
		{
			FrameID_DisplayedInTangra = FrameID;
			TangraFrameDisplayFromAOTA = false;
			TangraFrameDisplayFromTangra = true;
			AOTAData.PlotAOTA();
		}

		public void CloseAOTA()
		{
			RunFromTangra = false;
			try
			{
				((Form)AOTAData.PlotForm).Close();
			}
			catch
			{
			}
		}
	}
}
