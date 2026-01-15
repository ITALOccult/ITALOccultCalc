namespace Occult.Lunar_Observations
{
	internal class Archive_File_Record
	{
		private string file = "";

		private int record;

		public string File
		{
			get
			{
				return file;
			}
			set
			{
				file = value;
			}
		}

		public int Record
		{
			get
			{
				return record;
			}
			set
			{
				record = value;
			}
		}
	}
}
