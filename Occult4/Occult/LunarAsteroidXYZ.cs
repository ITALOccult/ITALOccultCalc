using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class LunarAsteroidXYZ : Form
	{
		private readonly string AppPath;

		private IContainer components;

		private GroupBox groupBox4;

		private Button cmdJD248;

		private Button cmdJD247;

		private Button cmdJD246;

		private Button cmdJD245;

		private Button cmdJD244;

		private Button cmdJD243;

		private Button cmdJD242;

		private Button cmdJD241;

		private Button cmdJD240;

		private Button cmdJD239;

		public LunarAsteroidXYZ()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void LunarAsteroidXYZ_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			Check_XYZ_Files();
		}

		private void Check_XYZ_Files()
		{
			Button obj = cmdJD239;
			Button obj2 = cmdJD240;
			Button obj3 = cmdJD241;
			Button obj4 = cmdJD242;
			Button obj5 = cmdJD243;
			Button obj6 = cmdJD244;
			Button obj7 = cmdJD245;
			Button obj8 = cmdJD246;
			Button obj9 = cmdJD247;
			bool flag;
			((Control)cmdJD248).set_Enabled(flag = true);
			bool flag2;
			((Control)obj9).set_Enabled(flag2 = flag);
			bool flag3;
			((Control)obj8).set_Enabled(flag3 = flag2);
			bool flag4;
			((Control)obj7).set_Enabled(flag4 = flag3);
			bool flag5;
			((Control)obj6).set_Enabled(flag5 = flag4);
			bool flag6;
			((Control)obj5).set_Enabled(flag6 = flag5);
			bool flag7;
			((Control)obj4).set_Enabled(flag7 = flag6);
			bool flag8;
			((Control)obj3).set_Enabled(flag8 = flag7);
			bool enabled;
			((Control)obj2).set_Enabled(enabled = flag8);
			((Control)obj).set_Enabled(enabled);
			if (File.Exists(AppPath + "\\Resource Files\\2390000_XYZ.bin") && new FileInfo(AppPath + "\\Resource Files\\2390000_XYZ.bin").Length == 1200000)
			{
				((Control)cmdJD239).set_Enabled(false);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2400000_XYZ.bin") && new FileInfo(AppPath + "\\Resource Files\\2400000_XYZ.bin").Length == 1200000)
			{
				((Control)cmdJD240).set_Enabled(false);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2410000_XYZ.bin") && new FileInfo(AppPath + "\\Resource Files\\2410000_XYZ.bin").Length == 1200000)
			{
				((Control)cmdJD241).set_Enabled(false);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2420000_XYZ.bin") && new FileInfo(AppPath + "\\Resource Files\\2420000_XYZ.bin").Length == 1200000)
			{
				((Control)cmdJD242).set_Enabled(false);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2430000_XYZ.bin") && new FileInfo(AppPath + "\\Resource Files\\2430000_XYZ.bin").Length == 1200000)
			{
				((Control)cmdJD243).set_Enabled(false);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2440000_XYZ.bin") && new FileInfo(AppPath + "\\Resource Files\\2440000_XYZ.bin").Length == 1200000)
			{
				((Control)cmdJD244).set_Enabled(false);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2450000_XYZ.bin") && new FileInfo(AppPath + "\\Resource Files\\2450000_XYZ.bin").Length == 1200000)
			{
				((Control)cmdJD245).set_Enabled(false);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2460000_XYZ.bin") && new FileInfo(AppPath + "\\Resource Files\\2460000_XYZ.bin").Length == 1200000)
			{
				((Control)cmdJD246).set_Enabled(false);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2470000_XYZ.bin") && new FileInfo(AppPath + "\\Resource Files\\2470000_XYZ.bin").Length == 1200000)
			{
				((Control)cmdJD247).set_Enabled(false);
			}
			if (File.Exists(AppPath + "\\Resource Files\\2480000_XYZ.bin") && new FileInfo(AppPath + "\\Resource Files\\2480000_XYZ.bin").Length == 1200000)
			{
				((Control)cmdJD248).set_Enabled(false);
			}
		}

		private void cmdJD239_Click(object sender, EventArgs e)
		{
			GenerateXYZephemeris("239");
		}

		private void cmdJD240_Click(object sender, EventArgs e)
		{
			GenerateXYZephemeris("240");
		}

		private void cmdJD241_Click(object sender, EventArgs e)
		{
			GenerateXYZephemeris("241");
		}

		private void cmdJD242_Click(object sender, EventArgs e)
		{
			GenerateXYZephemeris("242");
		}

		private void cmdJD243_Click(object sender, EventArgs e)
		{
			GenerateXYZephemeris("243");
		}

		private void cmdJD244_Click(object sender, EventArgs e)
		{
			GenerateXYZephemeris("244");
		}

		private void cmdJD245_Click(object sender, EventArgs e)
		{
			GenerateXYZephemeris("245");
		}

		private void cmdJD246_Click(object sender, EventArgs e)
		{
			GenerateXYZephemeris("246");
		}

		private void cmdJD247_Click(object sender, EventArgs e)
		{
			GenerateXYZephemeris("247");
		}

		private void cmdJD248_Click(object sender, EventArgs e)
		{
			GenerateXYZephemeris("248");
		}

		private void GenerateXYZephemeris(string JDbase)
		{
			Application.set_UseWaitCursor(true);
			Application.DoEvents();
			Utilities.AllPlanetsXYZ_Daily(JDbase);
			Application.set_UseWaitCursor(false);
			Check_XYZ_Files();
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
			//IL_06fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0704: Expected O, but got Unknown
			groupBox4 = new GroupBox();
			cmdJD248 = new Button();
			cmdJD247 = new Button();
			cmdJD246 = new Button();
			cmdJD245 = new Button();
			cmdJD244 = new Button();
			cmdJD243 = new Button();
			cmdJD242 = new Button();
			cmdJD241 = new Button();
			cmdJD240 = new Button();
			cmdJD239 = new Button();
			((Control)groupBox4).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD248);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD247);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD246);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD245);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD244);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD243);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD242);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD241);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD240);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdJD239);
			((Control)groupBox4).set_Location(new Point(12, 12));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(756, 93));
			((Control)groupBox4).set_TabIndex(10);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("The period from the Osculating date  to the Occultation prediction date must be covered by the depressed buttons");
			((Control)cmdJD248).set_Location(new Point(576, 21));
			((Control)cmdJD248).set_Name("cmdJD248");
			((Control)cmdJD248).set_Size(new Size(160, 22));
			((Control)cmdJD248).set_TabIndex(9);
			((Control)cmdJD248).set_Text("2077 Nov 28 == 2105 Apr 16");
			((ButtonBase)cmdJD248).set_UseVisualStyleBackColor(true);
			((Control)cmdJD248).add_Click((EventHandler)cmdJD248_Click);
			((Control)cmdJD247).set_Location(new Point(389, 65));
			((Control)cmdJD247).set_Name("cmdJD247");
			((Control)cmdJD247).set_Size(new Size(160, 22));
			((Control)cmdJD247).set_TabIndex(8);
			((Control)cmdJD247).set_Text("2050 Jul 13 == 2077 Nov 28");
			((ButtonBase)cmdJD247).set_UseVisualStyleBackColor(true);
			((Control)cmdJD247).add_Click((EventHandler)cmdJD247_Click);
			((Control)cmdJD246).set_Location(new Point(389, 43));
			((Control)cmdJD246).set_Name("cmdJD246");
			((Control)cmdJD246).set_Size(new Size(160, 22));
			((Control)cmdJD246).set_TabIndex(7);
			((Control)cmdJD246).set_Text("2023 Feb 25 == 2050 Jul 13");
			((ButtonBase)cmdJD246).set_UseVisualStyleBackColor(true);
			((Control)cmdJD246).add_Click((EventHandler)cmdJD246_Click);
			((Control)cmdJD245).set_Location(new Point(389, 21));
			((Control)cmdJD245).set_Name("cmdJD245");
			((Control)cmdJD245).set_Size(new Size(160, 22));
			((Control)cmdJD245).set_TabIndex(6);
			((Control)cmdJD245).set_Text("1995 Oct 10 == 2023 Feb 25");
			((ButtonBase)cmdJD245).set_UseVisualStyleBackColor(true);
			((Control)cmdJD245).add_Click((EventHandler)cmdJD245_Click);
			((Control)cmdJD244).set_Location(new Point(202, 65));
			((Control)cmdJD244).set_Name("cmdJD244");
			((Control)cmdJD244).set_Size(new Size(160, 22));
			((Control)cmdJD244).set_TabIndex(5);
			((Control)cmdJD244).set_Text("1968 May 24 == 1995 Oct 10");
			((ButtonBase)cmdJD244).set_UseVisualStyleBackColor(true);
			((Control)cmdJD244).add_Click((EventHandler)cmdJD244_Click);
			((Control)cmdJD243).set_Location(new Point(202, 43));
			((Control)cmdJD243).set_Name("cmdJD243");
			((Control)cmdJD243).set_Size(new Size(160, 22));
			((Control)cmdJD243).set_TabIndex(4);
			((Control)cmdJD243).set_Text("1941 Jan 06 == 1968 May 24");
			((ButtonBase)cmdJD243).set_UseVisualStyleBackColor(true);
			((Control)cmdJD243).add_Click((EventHandler)cmdJD243_Click);
			((Control)cmdJD242).set_Location(new Point(202, 21));
			((Control)cmdJD242).set_Name("cmdJD242");
			((Control)cmdJD242).set_Size(new Size(160, 22));
			((Control)cmdJD242).set_TabIndex(3);
			((Control)cmdJD242).set_Text("1913 Aug 21 == 1941 Jan 06");
			((ButtonBase)cmdJD242).set_UseVisualStyleBackColor(true);
			((Control)cmdJD242).add_Click((EventHandler)cmdJD242_Click);
			((Control)cmdJD241).set_Location(new Point(15, 65));
			((Control)cmdJD241).set_Name("cmdJD241");
			((Control)cmdJD241).set_Size(new Size(160, 22));
			((Control)cmdJD241).set_TabIndex(2);
			((Control)cmdJD241).set_Text("1886 Apr 04 == 1913 Aug 21");
			((ButtonBase)cmdJD241).set_UseVisualStyleBackColor(true);
			((Control)cmdJD241).add_Click((EventHandler)cmdJD241_Click);
			((Control)cmdJD240).set_Location(new Point(15, 43));
			((Control)cmdJD240).set_Name("cmdJD240");
			((Control)cmdJD240).set_Size(new Size(160, 22));
			((Control)cmdJD240).set_TabIndex(1);
			((Control)cmdJD240).set_Text("1858 Nov 17 == 1886 Apr 04");
			((ButtonBase)cmdJD240).set_UseVisualStyleBackColor(true);
			((Control)cmdJD240).add_Click((EventHandler)cmdJD240_Click);
			((Control)cmdJD239).set_Location(new Point(15, 19));
			((Control)cmdJD239).set_Name("cmdJD239");
			((Control)cmdJD239).set_Size(new Size(160, 22));
			((Control)cmdJD239).set_TabIndex(0);
			((Control)cmdJD239).set_Text("1831 Jul 02 == 1858 Nov 17");
			((ButtonBase)cmdJD239).set_UseVisualStyleBackColor(true);
			((Control)cmdJD239).add_Click((EventHandler)cmdJD239_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(798, 112));
			((Control)this).get_Controls().Add((Control)(object)groupBox4);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarPlanetXYZ", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationLunarPlanetXYZ);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("LunarAsteroidXYZ");
			((Control)this).set_Text("Provide planetary coordinates to integrate the orbits of asteroids");
			((Form)this).add_Load((EventHandler)LunarAsteroidXYZ_Load);
			((Control)groupBox4).ResumeLayout(false);
			((Control)this).ResumeLayout(false);
		}
	}
}
