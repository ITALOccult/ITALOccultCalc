using System;

namespace Occult.MPC_PDS
{
	internal class GridContent : IComparable
	{
		private string label = "";

		private string format = "";

		private string precision = "";

		private string unit = "";

		private string description = "";

		private int sequenceNo;

		internal int SequenceNo
		{
			get
			{
				return sequenceNo;
			}
			set
			{
				sequenceNo = value;
			}
		}

		internal string Label
		{
			get
			{
				return label;
			}
			set
			{
				label = value;
			}
		}

		internal string Format
		{
			get
			{
				return format;
			}
			set
			{
				format = value;
			}
		}

		internal string Precision
		{
			get
			{
				return precision;
			}
			set
			{
				precision = value;
			}
		}

		internal string Unit
		{
			get
			{
				return unit;
			}
			set
			{
				unit = value;
			}
		}

		internal string Description
		{
			get
			{
				return description;
			}
			set
			{
				description = value;
			}
		}

		public int CompareTo(object other)
		{
			return sequenceNo.CompareTo(((GridContent)other).sequenceNo);
		}

		public override string ToString()
		{
			if (format == "")
			{
				return Label.ToString();
			}
			string text = Label.Trim() + ":" + Format.Trim() + ":";
			if ((Precision.Trim().Length > 3) & Precision.Trim().Contains("f") & Precision.Trim().Contains("%"))
			{
				text += Precision.Trim();
			}
			return text + ":" + Unit.Trim() + ":" + Description.Trim();
		}
	}
}
