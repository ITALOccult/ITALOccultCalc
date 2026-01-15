using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace LightCurves
{
	public class LightCurvesSelectRange : Form
	{
		private int Count;

		internal int FirstRecord;

		internal int LastRecord;

		private IContainer components;

		private Label label1;

		private Label label2;

		internal Label lblLightCurveCount;

		private Label label3;

		private Label label4;

		private Label label5;

		private Button cmdOK;

		private Button cmdCancel;

		internal NumericUpDown updnFirstLightCurve;

		internal NumericUpDown updnPlotCount;

		public LightCurvesSelectRange(int LightCurveCount)
		{
			InitializeComponent();
			Count = LightCurveCount;
			((Control)lblLightCurveCount).set_Text(Count.ToString());
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			int num = (int)updnPlotCount.get_Value();
			FirstRecord = (int)updnFirstLightCurve.get_Value() - 1;
			LastRecord = FirstRecord + num - 1;
			if (LastRecord - FirstRecord > 599)
			{
				LastRecord = FirstRecord + 599;
			}
			if (LastRecord >= Count)
			{
				LastRecord = Count - 1;
			}
			((Form)this).set_DialogResult((DialogResult)1);
			((Form)this).Close();
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)2);
			((Form)this).Close();
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
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(LightCurvesSelectRange));
			label1 = new Label();
			label2 = new Label();
			lblLightCurveCount = new Label();
			label3 = new Label();
			updnFirstLightCurve = new NumericUpDown();
			label4 = new Label();
			label5 = new Label();
			updnPlotCount = new NumericUpDown();
			cmdOK = new Button();
			cmdCancel = new Button();
			((ISupportInitialize)updnFirstLightCurve).BeginInit();
			((ISupportInitialize)updnPlotCount).BeginInit();
			((Control)this).SuspendLayout();
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(12, 17));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(219, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("The selected source of light curves  contains");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(12, 60));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(254, 52));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text(componentResourceManager.GetString("label2.Text"));
			((Control)lblLightCurveCount).set_AutoSize(true);
			((Control)lblLightCurveCount).set_Location(new Point(230, 17));
			((Control)lblLightCurveCount).set_Name("lblLightCurveCount");
			((Control)lblLightCurveCount).set_Size(new Size(25, 13));
			((Control)lblLightCurveCount).set_TabIndex(2);
			((Control)lblLightCurveCount).set_Text("600");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(12, 29));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(61, 13));
			((Control)label3).set_TabIndex(3);
			((Control)label3).set_Text("light curves");
			updnFirstLightCurve.set_Increment(new decimal(new int[4] { 20, 0, 0, 0 }));
			((Control)updnFirstLightCurve).set_Location(new Point(44, 146));
			updnFirstLightCurve.set_Maximum(new decimal(new int[4] { 10000, 0, 0, 0 }));
			updnFirstLightCurve.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnFirstLightCurve).set_Name("updnFirstLightCurve");
			((Control)updnFirstLightCurve).set_Size(new Size(64, 20));
			((Control)updnFirstLightCurve).set_TabIndex(4);
			updnFirstLightCurve.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(37, 131));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(78, 13));
			((Control)label4).set_TabIndex(5);
			((Control)label4).set_Text("First light curve");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(136, 131));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(106, 13));
			((Control)label5).set_TabIndex(7);
			((Control)label5).set_Text("Number to be plotted");
			updnPlotCount.set_Increment(new decimal(new int[4] { 20, 0, 0, 0 }));
			((Control)updnPlotCount).set_Location(new Point(168, 146));
			updnPlotCount.set_Maximum(new decimal(new int[4] { 600, 0, 0, 0 }));
			updnPlotCount.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnPlotCount).set_Name("updnPlotCount");
			((Control)updnPlotCount).set_Size(new Size(43, 20));
			((Control)updnPlotCount).set_TabIndex(6);
			updnPlotCount.set_Value(new decimal(new int[4] { 600, 0, 0, 0 }));
			((Control)cmdOK).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdOK).set_Location(new Point(72, 188));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(56, 29));
			((Control)cmdOK).set_TabIndex(8);
			((Control)cmdOK).set_Text("OK");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)cmdCancel).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCancel).set_Location(new Point(151, 188));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(56, 29));
			((Control)cmdCancel).set_TabIndex(9);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(278, 230));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)updnPlotCount);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)updnFirstLightCurve);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)lblLightCurveCount);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)3);
			((Control)this).set_Name("LightCurvesSelectRange");
			((Control)this).set_Text("Light Curves - Select Range for Multi-plot");
			((ISupportInitialize)updnFirstLightCurve).EndInit();
			((ISupportInitialize)updnPlotCount).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
