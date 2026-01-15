using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using AOTA;
using Occult.File_Actions;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class CameraCorrections : Form
	{
		private CameraDelays CameraDelays;

		internal static List<CameraDelays> Delays = new List<CameraDelays>();

		private IContainer components;

		private Label label1;

		private Label label2;

		private ComboBox cmbVTI;

		internal ComboBox cmbCamera;

		private Label label3;

		private RadioButton optNTSC;

		private RadioButton optPAL;

		private Panel panel1;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		private Label lblFieldAdvice;

		private Label lblFrameAdvice;

		private Label lblCorrnStartFrame;

		private Label lblCorrnMiddleFrame;

		private Label lblCorrnStartField;

		private Label lblCorrnMiddleField;

		private Label label12;

		private Label lblIntegrationAdvice;

		public CameraCorrections()
		{
			InitializeComponent();
		}

		private void CameraCorrections_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\CameraDelays.dat") && !Utilities.InternetIsAvailable())
			{
				Downloads.Download_CameraDelays(SupressMessages: true);
			}
			cmbCamera.get_Items().Clear();
			Delays.Clear();
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\CameraDelays.dat"))
			{
				using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource Files\\CameraDelays.dat");
				do
				{
					string text = streamReader.ReadLine();
					if (!(text.Substring(0, 1) == "#"))
					{
						string[] array = text.Split(new char[1] { ',' });
						CameraDelays = new CameraDelays();
						CameraDelays.Camera = array[0].Trim();
						CameraDelays.IntegrationDelay = double.Parse(array[1]);
						CameraDelays.NonIntegrationDelay = double.Parse(array[2]);
						if (array.Length > 3)
						{
							CameraDelays.MultipleIntegrationDelay = double.Parse(array[3]);
						}
						else
						{
							CameraDelays.MultipleIntegrationDelay = 0.0;
						}
						Delays.Add(CameraDelays);
						cmbCamera.get_Items().Add((object)array[0].Trim());
					}
				}
				while (!streamReader.EndOfStream);
			}
			else
			{
				CameraDelays = new CameraDelays();
				CameraDelays.Camera = "No cameras available";
				CameraDelays.IntegrationDelay = 0.0;
				CameraDelays.NonIntegrationDelay = 0.0;
				Delays.Add(CameraDelays);
				cmbCamera.get_Items().Add((object)"No cameras available");
			}
			try
			{
				((ListControl)cmbCamera).set_SelectedIndex(Settings.Default.LunarCameraIndex);
			}
			catch
			{
			}
			if (((ListControl)cmbCamera).get_SelectedIndex() < 0)
			{
				((ListControl)cmbCamera).set_SelectedIndex(0);
			}
			try
			{
				((ListControl)cmbVTI).set_SelectedIndex(Settings.Default.Lunar_VTI);
			}
			catch
			{
			}
			if (((ListControl)cmbVTI).get_SelectedIndex() < 0)
			{
				((ListControl)cmbVTI).set_SelectedIndex(0);
			}
			optPAL.set_Checked(Settings.Default.Lunar_isPAL);
			optNTSC.set_Checked(!Settings.Default.Lunar_isPAL);
		}

		private void cmbCamera_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.LunarCameraIndex = ((ListControl)cmbCamera).get_SelectedIndex();
			SetDelayTimes(((ListControl)cmbCamera).get_SelectedIndex(), ((ListControl)cmbVTI).get_SelectedIndex());
			if (cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString()!.Contains("120N+"))
			{
				((Control)lblIntegrationAdvice).set_Text("Watec-120N+ uses integration for all 'Slow' settings");
			}
			else if (cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString()!.Contains("120N"))
			{
				((Control)lblIntegrationAdvice).set_Text("Watec-120N uses integration for all settings other than OFF");
			}
			else if (cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString()!.Contains("910"))
			{
				((Control)lblIntegrationAdvice).set_Text("Watec-910BD and 910HX cameras use integration for x2, x4, x8 .... settings");
			}
			else if (cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString()!.Contains("12V1C"))
			{
				((Control)lblIntegrationAdvice).set_Text("Mintron 12V1C uses integration for x2, x4, x6, .... settings");
			}
			else if (cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString()!.Contains("165DNR"))
			{
				((Control)lblIntegrationAdvice).set_Text("PC165DNR uses integration for x2, x4, x8, .... settings");
			}
			else if (cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString()!.Contains("2000N"))
			{
				((Control)lblIntegrationAdvice).set_Text("SCB2000N uses integration for x2, x4, x6, .... settings");
			}
			else if (cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString()!.Contains("MCH"))
			{
				((Control)lblIntegrationAdvice).set_Text("Malincam MCG Plus Color uses integration for x2, x4, x6, .... settings");
			}
			else if (cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString()!.Contains("902H2"))
			{
				((Control)lblIntegrationAdvice).set_Text("Watec 902H2 Ultimate does not use integration");
			}
			else if (cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString()!.Contains("164C"))
			{
				((Control)lblIntegrationAdvice).set_Text("PC164C-EX2 does not use integration");
			}
			else if (cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString()!.Contains("1004XC"))
			{
				((Control)lblIntegrationAdvice).set_Text("SK-1004XC/SO does not use integration");
			}
			else if (cmbCamera.get_Items().get_Item(((ListControl)cmbCamera).get_SelectedIndex()).ToString()!.Contains("LN300"))
			{
				((Control)lblIntegrationAdvice).set_Text("LN300 uses integration for x2, x4, x8 .... settings");
			}
			else
			{
				((Control)lblIntegrationAdvice).set_Text("...");
			}
		}

		private void cmbVTI_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.Lunar_VTI = ((ListControl)cmbVTI).get_SelectedIndex();
			SetDelayTimes(((ListControl)cmbCamera).get_SelectedIndex(), ((ListControl)cmbVTI).get_SelectedIndex());
			if (cmbVTI.get_Items().get_Item(((ListControl)cmbVTI).get_SelectedIndex()).ToString()!.Contains("IOTA"))
			{
				((Control)lblFrameAdvice).set_Text("Correction to the EARLIER Time stamp on this FRAME");
				((Control)lblFieldAdvice).set_Text("Correction to the Time stamp on this FIELD");
			}
			else if (cmbVTI.get_Items().get_Item(((ListControl)cmbVTI).get_SelectedIndex()).ToString()!.Contains("KIWI"))
			{
				((Control)lblFrameAdvice).set_Text("Correction to the 'CLEAR' Time stamp on this FRAME");
				((Control)lblFieldAdvice).set_Text("Correction to the LATER Time stamp on this FIELD");
			}
			else if (cmbVTI.get_Items().get_Item(((ListControl)cmbVTI).get_SelectedIndex()).ToString()!.Contains("TIM-10"))
			{
				((Control)lblFrameAdvice).set_Text("Correction to the 'CLEAR' Time stamp on this FRAME");
				((Control)lblFieldAdvice).set_Text("Correction to LATER Time stamp on this FIELD");
			}
			else if (cmbVTI.get_Items().get_Item(((ListControl)cmbVTI).get_SelectedIndex()).ToString()!.Contains("Sven"))
			{
				((Control)lblFrameAdvice).set_Text("Correction to the EARLIER Time stamp on this FRAME");
				((Control)lblFieldAdvice).set_Text("Correction to the Time stamp on this FIELD");
			}
			else if (cmbVTI.get_Items().get_Item(((ListControl)cmbVTI).get_SelectedIndex()).ToString()!.Contains("GHS"))
			{
				((Control)lblFrameAdvice).set_Text("?? Correction to the Earlier Time stamp on this FRAME");
				((Control)lblFieldAdvice).set_Text("?? Correction to LATER Time stamp on this FIELD");
			}
			else if (cmbVTI.get_Items().get_Item(((ListControl)cmbVTI).get_SelectedIndex()).ToString()!.Contains("BoxSprite2"))
			{
				((Control)lblFrameAdvice).set_Text("Correction to the EARLIER Time stamp on this FRAME");
				((Control)lblFieldAdvice).set_Text("Correction to the Time stamp on this FIELD");
			}
		}

		private void optPAL_CheckedChanged(object sender, EventArgs e)
		{
			Settings.Default.Lunar_isPAL = optPAL.get_Checked();
			SetDelayTimes(((ListControl)cmbCamera).get_SelectedIndex(), ((ListControl)cmbVTI).get_SelectedIndex());
		}

		internal void SetDelayTimes(int Camera, int VTI)
		{
			if (!(Camera < 0 || VTI < 0))
			{
				double nonIntegrationDelay = Delays[Camera].NonIntegrationDelay;
				double num = 0.0;
				double num2 = 0.033366700033366704;
				if (optPAL.get_Checked())
				{
					num2 = 0.04;
				}
				if (cmbVTI.get_Items().get_Item(VTI).ToString()!.Contains("TIM-10"))
				{
					num = 0.5;
				}
				((Control)lblCorrnStartFrame).set_Text($"{num2 * (0.0 - nonIntegrationDelay + num):+#.##0;-#.##0;#.##0} secs");
				((Control)lblCorrnMiddleFrame).set_Text($"{num2 * (0.0 - nonIntegrationDelay + num + 0.5):+#.##0;-#.##0;#.##0} secs");
				((Control)lblCorrnStartField).set_Text($"{num2 * (0.0 - nonIntegrationDelay + num):+#.##0;-#.##0;#.##0} secs");
				((Control)lblCorrnMiddleField).set_Text($"{num2 * (0.0 - nonIntegrationDelay + num + 0.25):+#.##0;-#.##0;#.##0} secs");
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(CameraCorrections));
			label1 = new Label();
			label2 = new Label();
			cmbVTI = new ComboBox();
			cmbCamera = new ComboBox();
			label3 = new Label();
			optNTSC = new RadioButton();
			optPAL = new RadioButton();
			panel1 = new Panel();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			lblFieldAdvice = new Label();
			lblFrameAdvice = new Label();
			lblCorrnStartFrame = new Label();
			lblCorrnMiddleFrame = new Label();
			lblCorrnStartField = new Label();
			lblCorrnMiddleField = new Label();
			label12 = new Label();
			lblIntegrationAdvice = new Label();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(169, 19));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(89, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("My camera is :");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(383, 19));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(68, 13));
			((Control)label2).set_TabIndex(4);
			((Control)label2).set_Text("My VTI is :");
			((Control)cmbVTI).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbVTI).set_FormattingEnabled(true);
			cmbVTI.get_Items().AddRange(new object[6] { "IOTA - VTI", "KIWI - OSD", "TIM-10 (previously Cuno VTI )", "Sven Anderson VTI", "GHS - OSD", "GPSBoxSprite2" });
			((Control)cmbVTI).set_Location(new Point(320, 35));
			((Control)cmbVTI).set_Name("cmbVTI");
			((Control)cmbVTI).set_Size(new Size(194, 21));
			((Control)cmbVTI).set_TabIndex(2);
			cmbVTI.add_SelectedIndexChanged((EventHandler)cmbVTI_SelectedIndexChanged);
			((Control)cmbCamera).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbCamera).set_FormattingEnabled(true);
			cmbCamera.get_Items().AddRange(new object[14]
			{
				"Mintron 12V1C-EX  NTSC", "Mintron 12V1C-EX  PAL", "PC164C-EX2   NTSC", "PC165DNR NTSC", "SCB-2000N NTSC", "SK-1004XC/50   PAL", "WAT-120N   NTSC", "WAT-120N   PAL", "WAT-120N+   PAL", "WAT-120N+  NTSC",
				"WAT-902H2 Ultimate  NTSC", "WAT-902H2 Ultimate  PAL", "WAT-910HX  NTSC", "WAT-910HX  PAL"
			});
			((Control)cmbCamera).set_Location(new Point(132, 35));
			((Control)cmbCamera).set_Name("cmbCamera");
			((Control)cmbCamera).set_Size(new Size(162, 21));
			((Control)cmbCamera).set_TabIndex(1);
			cmbCamera.add_SelectedIndexChanged((EventHandler)cmbCamera_SelectedIndexChanged);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(7, 5));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(83, 13));
			((Control)label3).set_TabIndex(0);
			((Control)label3).set_Text("Video System");
			((Control)optNTSC).set_AutoSize(true);
			((Control)optNTSC).set_Location(new Point(10, 20));
			((Control)optNTSC).set_Name("optNTSC");
			((Control)optNTSC).set_Size(new Size(54, 17));
			((Control)optNTSC).set_TabIndex(1);
			((Control)optNTSC).set_Text("NTSC");
			((ButtonBase)optNTSC).set_UseVisualStyleBackColor(true);
			((Control)optPAL).set_AutoSize(true);
			optPAL.set_Checked(true);
			((Control)optPAL).set_Location(new Point(10, 38));
			((Control)optPAL).set_Name("optPAL");
			((Control)optPAL).set_Size(new Size(45, 17));
			((Control)optPAL).set_TabIndex(2);
			optPAL.set_TabStop(true);
			((Control)optPAL).set_Text("PAL");
			((ButtonBase)optPAL).set_UseVisualStyleBackColor(true);
			optPAL.add_CheckedChanged((EventHandler)optPAL_CheckedChanged);
			((Control)panel1).get_Controls().Add((Control)(object)optPAL);
			((Control)panel1).get_Controls().Add((Control)(object)optNTSC);
			((Control)panel1).get_Controls().Add((Control)(object)label3);
			((Control)panel1).set_Location(new Point(14, 7));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(92, 58));
			((Control)panel1).set_TabIndex(0);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_ForeColor(Color.MidnightBlue);
			((Control)label4).set_Location(new Point(13, 85));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(141, 13));
			((Control)label4).set_TabIndex(5);
			((Control)label4).set_Text("Measured at Frame rate");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(Color.MidnightBlue);
			((Control)label5).set_Location(new Point(14, 150));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(134, 13));
			((Control)label5).set_TabIndex(12);
			((Control)label5).set_Text("Measured at Field rate");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(52, 127));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(132, 13));
			((Control)label6).set_TabIndex(9);
			((Control)label6).set_Text("Event at MIDDLE of frame");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(52, 106));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(127, 13));
			((Control)label7).set_TabIndex(7);
			((Control)label7).set_Text("Event at START of frame");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(52, 192));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(125, 13));
			((Control)label8).set_TabIndex(15);
			((Control)label8).set_Text("Event at MIDDLE of field");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(52, 171));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(120, 13));
			((Control)label9).set_TabIndex(13);
			((Control)label9).set_Text("Event at START of field");
			((Control)lblFieldAdvice).set_AutoSize(true);
			((Control)lblFieldAdvice).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblFieldAdvice).set_ForeColor(Color.MidnightBlue);
			((Control)lblFieldAdvice).set_Location(new Point(208, 150));
			((Control)lblFieldAdvice).set_Name("lblFieldAdvice");
			((Control)lblFieldAdvice).set_Size(new Size(229, 13));
			((Control)lblFieldAdvice).set_TabIndex(11);
			((Control)lblFieldAdvice).set_Text("Correction to Time stamp on this FIELD");
			((Control)lblFrameAdvice).set_AutoSize(true);
			((Control)lblFrameAdvice).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblFrameAdvice).set_ForeColor(Color.MidnightBlue);
			((Control)lblFrameAdvice).set_Location(new Point(207, 85));
			((Control)lblFrameAdvice).set_Name("lblFrameAdvice");
			((Control)lblFrameAdvice).set_Size(new Size(315, 13));
			((Control)lblFrameAdvice).set_TabIndex(6);
			((Control)lblFrameAdvice).set_Text("Correction to the EARLIER Time stamp on this FRAME");
			((Control)lblCorrnStartFrame).set_AutoSize(true);
			((Control)lblCorrnStartFrame).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblCorrnStartFrame).set_Location(new Point(210, 107));
			((Control)lblCorrnStartFrame).set_Name("lblCorrnStartFrame");
			((Control)lblCorrnStartFrame).set_Size(new Size(76, 13));
			((Control)lblCorrnStartFrame).set_TabIndex(8);
			((Control)lblCorrnStartFrame).set_Text("+0.000 secs");
			((Control)lblCorrnMiddleFrame).set_AutoSize(true);
			((Control)lblCorrnMiddleFrame).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblCorrnMiddleFrame).set_Location(new Point(210, 127));
			((Control)lblCorrnMiddleFrame).set_Name("lblCorrnMiddleFrame");
			((Control)lblCorrnMiddleFrame).set_Size(new Size(76, 13));
			((Control)lblCorrnMiddleFrame).set_TabIndex(10);
			((Control)lblCorrnMiddleFrame).set_Text("+0.000 secs");
			((Control)lblCorrnStartField).set_AutoSize(true);
			((Control)lblCorrnStartField).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblCorrnStartField).set_Location(new Point(210, 171));
			((Control)lblCorrnStartField).set_Name("lblCorrnStartField");
			((Control)lblCorrnStartField).set_Size(new Size(76, 13));
			((Control)lblCorrnStartField).set_TabIndex(14);
			((Control)lblCorrnStartField).set_Text("+0.000 secs");
			((Control)lblCorrnMiddleField).set_AutoSize(true);
			((Control)lblCorrnMiddleField).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblCorrnMiddleField).set_Location(new Point(210, 192));
			((Control)lblCorrnMiddleField).set_Name("lblCorrnMiddleField");
			((Control)lblCorrnMiddleField).set_Size(new Size(76, 13));
			((Control)lblCorrnMiddleField).set_TabIndex(16);
			((Control)lblCorrnMiddleField).set_Text("+0.000 secs");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_ForeColor(Color.Maroon);
			((Control)label12).set_Location(new Point(14, 220));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(350, 13));
			((Control)label12).set_TabIndex(17);
			((Control)label12).set_Text("These time corrections assume the Camera is not Integrating");
			((Control)lblIntegrationAdvice).set_AutoSize(true);
			((Control)lblIntegrationAdvice).set_Location(new Point(14, 237));
			((Control)lblIntegrationAdvice).set_Name("lblIntegrationAdvice");
			((Control)lblIntegrationAdvice).set_Size(new Size(16, 13));
			((Control)lblIntegrationAdvice).set_TabIndex(18);
			((Control)lblIntegrationAdvice).set_Text("...");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(528, 258));
			((Control)this).get_Controls().Add((Control)(object)lblIntegrationAdvice);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)lblCorrnMiddleField);
			((Control)this).get_Controls().Add((Control)(object)lblCorrnStartField);
			((Control)this).get_Controls().Add((Control)(object)lblCorrnMiddleFrame);
			((Control)this).get_Controls().Add((Control)(object)lblCorrnStartFrame);
			((Control)this).get_Controls().Add((Control)(object)lblFrameAdvice);
			((Control)this).get_Controls().Add((Control)(object)lblFieldAdvice);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)cmbCamera);
			((Control)this).get_Controls().Add((Control)(object)cmbVTI);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Control)this).set_Name("CameraCorrections");
			((Control)this).set_Text("Time corrections       VTI Time stamp => Event time");
			((Form)this).add_Load((EventHandler)CameraCorrections_Load);
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
